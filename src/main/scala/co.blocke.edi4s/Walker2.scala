package co.blocke.edi4s

import model.*
import scala.collection.mutable
import zio.*
import scala.annotation.tailrec
import pprint.*

object Walker2:

  def compareSpecs(
                    src: RefinedDocumentSpec,
                    target: RefinedDocumentSpec
                  ): ZIO[Any, DifferenceError, List[Difference]] =
    val z = compareSegmentLists("", src.segments, target.segments)
    //pprint.log(z, height = 2000)
    ZIO.succeed(z)


  // Find a segment in a target population (in). May be multiple hits.
  // Return List[(path, segment)]
  private def findSegment(
                           srcPath: String,
                           in: List[RefinedSegmentSpec | RefinedLoopSpec],
                           descendNested: Boolean = false
                         ): ZIO[Any, DifferenceError, List[(String, RefinedSegmentSpec | RefinedLoopSpec)]] = {

    @tailrec
    def loop(
              pathParts: List[String],
              current: List[RefinedSegmentSpec | RefinedLoopSpec],
              prefix: List[String]
            ): ZIO[Any, DifferenceError, List[(String, RefinedSegmentSpec | RefinedLoopSpec)]] =
      pathParts match {
        case Nil =>
          ZIO.succeed(Nil)

        case head :: Nil =>
          val found = current.filter(s => nameOf(s) == head)
          ZIO.succeed(found.map { f => (prefix :+ head).mkString(".") -> f })

        case head :: tail =>
          current.find(s => nameOf(s) == head) match {
            case Some(r: RefinedLoopSpec) =>
              loop(tail, r.body, prefix :+ head)

            case Some(_) =>
              ZIO.fail(
                DifferenceError(
                  s"Invalid path $srcPath: expected loop at ${(prefix :+ head).mkString(".")} but found a segment"
                )
              )

            case None =>
              ZIO.succeed(Nil) // not found is okay
          }
      }

    val parts = srcPath.split('.').toList
    loop(parts, in, Nil) // ← this must be the last line so it's returned
  }


  private def nameOf(x: RefinedSegmentSpec | RefinedLoopSpec): String = x match {
    case s: RefinedSegmentSpec => s.name
    case l: RefinedLoopSpec => l.name
  }

  private def fieldNameOf(x: RefinedSingleFieldSpec | RefinedCompositeFieldSpec): String = x match {
    case s: RefinedSingleFieldSpec => s.name
    case l: RefinedCompositeFieldSpec => l.name
  }

  private def canonicalNameOf(x: RefinedSegmentSpec | RefinedLoopSpec): String = x match {
    case s: RefinedSegmentSpec => s.canonicalName
    case l: RefinedLoopSpec => l.canonicalName
  }

  private def canonicalFieldNameOf(x: RefinedSingleFieldSpec | RefinedCompositeFieldSpec): String = x match {
    case s: RefinedSingleFieldSpec => s.canonicalName
    case l: RefinedCompositeFieldSpec => l.canonicalName
  }

  private def compareTwoSegments(path: String, a: RefinedSegmentSpec, b: RefinedSegmentSpec): Option[SimpleSegmentDifference] =
    val req = Option.when(a.required != b.required)((a.required, b.required))
    val assertions = Option.when(a.assertions.toSet != b.assertions.toSet)((a.assertions, b.assertions))
    val fieldDiffs = compareSegmentFields(getFields(a), getFields(b))
    (req, assertions, fieldDiffs) match {
      case (None, None, Nil) => None
      case _ => Some(SimpleSegmentDifference(path, nameOf(a), canonicalNameOf(b), None, req, assertions, None, fieldDiffs))
    }

  private case class HLNavigator(
                          srcMap: Map[String, List[(String, RefinedSegmentSpec | RefinedLoopSpec)]],
                          targetMap: Map[String, List[(String, RefinedSegmentSpec | RefinedLoopSpec)]]
                        )

  private inline def splitPath(path: String): (String,String) =
    val (prefix, last) = path.splitAt(path.lastIndexOf('.'))
    (prefix.stripSuffix("."), last.stripPrefix("."))

  private def compareTwoLoopSegments(path: String, a: RefinedLoopSpec, b: RefinedLoopSpec): Option[LoopSegmentDifference] =
    val req = Option.when(a.required != b.required)((a.required, b.required))
    val assertions = Option.when(a.assertions.toSet != b.assertions.toSet)((a.assertions, b.assertions))
    val fieldDiffs = compareSegmentFields(getFields(a), getFields(b))
    val minDiff = Option.when(a.minRepeats != b.minRepeats)(a.minRepeats,b.minRepeats)
    val maxDiff = Option.when(a.maxRepeats != b.maxRepeats)(a.maxRepeats,b.maxRepeats)
    val pathPrefix = if path.isEmpty then "" else path+"."
    val bodyAndNest: (Option[List[SegmentDifference]], Option[List[LoopSegmentDifference]]) =
      if canonicalNameOf(a) == "HL" then
        val nav = HLNavigator( buildSegmentPathMap(List(a),""), buildSegmentPathMap(List(b),"") )

        // Find any missing segments, regardless of path in src and target
        val sourceKeys = nav.srcMap.keySet
        val targetKeys = nav.targetMap.keySet
        val onlyInSource = (sourceKeys -- targetKeys).toList
        val onlyInTarget = (targetKeys -- sourceKeys).toList
        val acc1 = onlyInSource.flatMap { k =>
          nav.srcMap(k).map { case (elemPath, spec) =>
            val (prefix, _) = splitPath(elemPath)
            LoopSegmentDifference(prefix, nameOf(spec), canonicalNameOf(spec), Some((true, false)), fieldDiff = Nil)
          }
        }
        val acc2 = onlyInTarget.flatMap { k =>
          nav.targetMap(k).map { case (elemPath, spec) =>
            val (prefix, _) = splitPath(elemPath)
            LoopSegmentDifference(prefix, nameOf(spec), canonicalNameOf(spec), Some((false, true)), fieldDiff = Nil)
          }
        }
        
        def compareFoundOnPath()

        // Now track "moved" segments (NOTE: unordered!)
        val foo = sourceKey.map { k =>
          val srcElemPaths = nav.srcMap(k)
          val targetElemPaths = nav.targetMap(k)
          (srcElemPaths.size, targetElemPaths.size) match {
            // Exact mapping
            case (1,1) =>
              (srcElemPaths.head._2, targetElemPaths.head._2) match {
                case (a: RefinedSegmentSpec, b: RefinedSegmentSpec) =>
                  val (prefix, _) = splitPath(elemPath) // <-- Won't work.... no elemPath here
                  // We need to do 2 things here:
                  // 1) Create a LoopDifference object, noting the difference in pathsw
                  // 2) Compare the actual segments, regardless of their different paths--perhaps put the results (SegmentDifference) in the body of the LoopDifference?
                  compareTwoSegments("(different paths)", a, b)
                case (a: RefinedLoopSpec, b: RefinedLoopSpec) =>
                  val (prefix, _) = splitPath(elemPath)
                  compareTwoLoopSegments(prefix, a, b)
                case (a,b) =>
                  Some(SeriousDifference(path,nameOf(a),canonicalNameOf(a),s"Elements ${srcElemPaths.head._1} and ${targetElemPaths.head._1} have different types!"))
              }
            // Equal mapping (presume same?)
            case (n,m) if n == m => ???
            // 1-to-many: Possible nesting HLs from canonical spec
            case (1,n) => ???
            // n-to-m: Who knows!
            case _ =>
          }
        }

        val allAcc: List[LoopSegmentDifference] = acc1 ++ acc2
        pprint.log(allAcc)
        (None, Some(allAcc))
      /*
case class LoopSegmentDifference(
                                  path: String,
                                  name: String,  // initially canonical name but may be renamed
                                  canonicalName: String,  // name used in the canonical spec
                                  presence: Option[(Boolean,Boolean)] = None,
                                  required: Option[(Boolean,Boolean)] = None,
                                  assertions: Option[(List[String],List[String])] = None,
                                  pathDiff: Option[(String,String)],
                                  fieldDiff: List[FieldDifference],
                                  minDiff: Option[(Option[Int], Option[Int])] = None,
                                  maxDiff: Option[(Option[Int], Option[Int])] = None,
                                  bodyDiff: Option[List[SegmentDifference]] = None,
                                  nested: Option[List[LoopSegmentDifference]] = None
                                ) extends SegmentDifference:
      */
      else
        (Some(compareSegmentLists(canonicalNameOf(a), a.body, b.body)), None)
    val (bodyDiff, nestDiff) = bodyAndNest
    (req, assertions, fieldDiffs, minDiff, maxDiff, bodyDiff, nestDiff) match {
      case (None, None, Nil, None, None, None, None) => None
      case _ =>
        Some(LoopSegmentDifference(path, nameOf(a), canonicalNameOf(b), None, req, assertions, None, fieldDiffs, minDiff, maxDiff, if (bodyDiff.nonEmpty) bodyDiff else None, nestDiff))
    }

  private def compareSegmentLists(
                                 path: String,
                                 src: List[RefinedSegmentSpec | RefinedLoopSpec],
                                 target: List[RefinedSegmentSpec | RefinedLoopSpec]
                               ): List[SegmentDifference] = {
    @tailrec
    def tracker(
               s: Int,
               t: Int,
               acc: List[Option[SegmentDifference]]
       ): List[Option[SegmentDifference]] =
      if s == src.length && t == target.length then
        acc
      else if s == src.length then
        tracker(s, t+1, acc :+ Some(SimpleSegmentDifference(path,nameOf(target(t)),canonicalNameOf(target(t)),Some((false, true)),None,None,None,Nil)))
      else if t == target.length then
        tracker(s+1, t, acc :+ Some(SimpleSegmentDifference(path, nameOf(src(s)), canonicalNameOf(src(s)), Some((false, true)), None, None, None, Nil)))
      else if nameOf(src(s)) == nameOf(target(t)) then
        (src(s), target(t)) match {
          case (s1: RefinedSegmentSpec, t1: RefinedSegmentSpec) =>
            tracker(s+1, t+1, acc :+ compareTwoSegments(path, s1, t1))
          case (s2: RefinedLoopSpec, t2: RefinedLoopSpec) =>
            tracker(s+1, t+1, acc :+ compareTwoLoopSegments(path, s2, t2, None))
          case _ => tracker(s+1, t+1, acc :+ Some(SeriousDifference(path,nameOf(target(t)),canonicalNameOf(target(t)),"Types (loop vs segment) differ. They must match!")))
        }
      else
        // Look for s in t
        val srcName = nameOf(src(s))
        val targetLookAhead = target.indexWhere(e => nameOf(e) == srcName)
        targetLookAhead match {
          case -1 =>
            // not found... go back to target(t) and look for next position in src
            val targetName = nameOf(target(t))
            val srcLookAhead = src.indexWhere(e => nameOf(e) == targetName)
            srcLookAhead match {
              case -1 => acc :+ Some(SeriousDifference(path,"(unknown)","","Cannot reconcile src/target body elements"))
              case a =>
                val chunk = src.slice(s, a)  // segments between t and j (exclusive)
                val diffs = chunk.map { seg =>
                  Some(SimpleSegmentDifference(
                    path,
                    nameOf(seg),
                    canonicalNameOf(seg),
                    Some((true, false)),
                    None, None, None, Nil
                  ))
                }
                tracker(a,t,acc ++ diffs)
            }
          case a =>
            val chunk = target.slice(t, a)  // segments between t and j (exclusive)
            val diffs = chunk.map { seg =>
              Some(SimpleSegmentDifference(
                path,
                nameOf(seg),
                canonicalNameOf(seg),
                Some((false, true)),
                None, None, None, Nil
              ))
            }
            tracker(s, a, acc ++ diffs)
        }

    tracker(0,0,Nil).flatten
  }

  private def getFields(t: RefinedSegmentSpec | RefinedLoopSpec): List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec] =
    t match {
      case u: RefinedSegmentSpec => u.fields
      case v: RefinedLoopSpec => v.fields
    }

  private def compareSegmentFields(
                     fieldsA: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec],
                     fieldsB: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec]
                   ): List[FieldDifference] = {

    def getField(name: String, fields: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec]) =
      fields.find(f => fieldNameOf(f) == name)

    val allNames = (fieldsA.map(fieldNameOf) ++ fieldsB.map(fieldNameOf)).distinct

    allNames.flatMap { name =>
      val fA = getField(name, fieldsA)
      val fB = getField(name, fieldsB)

      (fA, fB) match {
        case (Some(a), None) =>
          Some(SingleFieldDifference(fieldNameOf(a), canonicalFieldNameOf(a), presence = Some(true -> false)))

        case (None, Some(b)) =>
          Some(SingleFieldDifference(fieldNameOf(b), canonicalFieldNameOf(b), presence = Some(false -> true)))

        case (Some(a: RefinedSingleFieldSpec), Some(b: RefinedSingleFieldSpec)) =>
          val required = if (a.required != b.required) Some(a.required -> b.required) else None
          val dataType = if (a.dataType != b.dataType) Some(a.dataType -> b.dataType) else None
          val format = if (a.format != b.format) Some(a.format -> b.format) else None
          val elementId = if (a.elementId != b.elementId) Some(a.elementId -> b.elementId) else None
          val validValues = if (a.validValues.sorted != b.validValues.sorted) Some(a.validValues -> b.validValues) else None
          val validValuesRef = if (a.validValuesRef != b.validValuesRef) Some(a.validValuesRef -> b.validValuesRef) else None

          if (List(required, dataType, format, elementId, validValues, validValuesRef).exists(_.isDefined))
            Some(SingleFieldDifference(name, a.canonicalName, None, required, dataType, format, elementId, validValues, validValuesRef))
          else None

        case (Some(a: RefinedCompositeFieldSpec), Some(b: RefinedCompositeFieldSpec)) =>
          val required = if (a.required != b.required) Some(a.required -> b.required) else None
          val subDiffs = compareSegmentFields(a.components, b.components)
          if (required.isDefined || subDiffs.nonEmpty)
            Some(CompositeFieldDifference(a.name, a.canonicalName, presence = None, required = required, fieldDiff = subDiffs))
          else None

        case _ => None
      }
    }
  }

  private def mergePathMaps(
                     maps: List[Map[String, List[(String, RefinedSegmentSpec | RefinedLoopSpec)]]]
                   ): Map[String, List[(String, RefinedSegmentSpec | RefinedLoopSpec)]] = {
    maps.flatten
      .groupBy(_._1)
      .view
      .mapValues(_.flatMap(_._2))
      .toMap
  }

  private def cleanPath(path: String): String =
    path.replaceAll(">(HL\\.HL)+", ">HL")

  private def buildSegmentPathMap(
                           specs: List[RefinedSegmentSpec | RefinedLoopSpec],
                           path: String = ""
                         ): Map[String, List[(String, RefinedSegmentSpec | RefinedLoopSpec)]] = {
    specs.flatMap {
      case s: RefinedSegmentSpec =>
        val pathName = if s.canonicalName == "HL" && s.description.nonEmpty then s.canonicalName+s"[${s.description}]" else s.canonicalName
        val segPath = cleanPath(if path.isEmpty then s.canonicalName else s"$path.$pathName")
        Map(s.canonicalName -> List((segPath, s)))

      case loop: RefinedLoopSpec =>
        val loopName = loop.canonicalName
        val newPath = if path.isEmpty then loopName else s"$path.$loopName"

        // Traverse body
        val bodyMap = buildSegmentPathMap(loop.body, newPath)

        // Traverse nested loops, if any, using '>' delimiter
        val nestedMap = loop.nested match {
          case Some(nestedLoops) =>
            nestedLoops.flatMap { nestedLoop =>
              val pathName = if nestedLoop.canonicalName == "HL" && nestedLoop.description.nonEmpty then nestedLoop.canonicalName+s"[${nestedLoop.description}]" else nestedLoop.canonicalName
              val newnewPath = if loop.description.isEmpty then newPath else s"$newPath[${loop.description}]"
              val nestedPath = cleanPath(s"$newnewPath>$pathName")
              buildSegmentPathMap(List(nestedLoop), nestedPath)
            }.groupBy(_._1).map { case (k, v) => k -> v.flatMap(_._2) }

          case None => Map.empty
        }

        // Also include this loop in the result
        val thisLoopEntry = Map(loop.canonicalName -> List((newPath, loop)))

        // Merge all maps
        mergePathMaps(List(thisLoopEntry, bodyMap, nestedMap))
    }.groupBy(_._1).view.mapValues(_.flatMap(_._2)).toMap
  }