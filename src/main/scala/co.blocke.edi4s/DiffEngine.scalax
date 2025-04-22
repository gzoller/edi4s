package co.blocke.edi4s

import model.*
import scala.collection.mutable
import zio.*
import scala.annotation.tailrec
import pprint.*

object DiffEngine:

  //
  //  TOP-LEVEL
  //
  def compareSpecs(
                    src: RefinedDocumentSpec,
                    target: RefinedDocumentSpec
                  ): ZIO[Any, DifferenceError, List[Difference]] =
    ZIO.succeed(compareSegmentLists(Path(), src.segments, target.segments))


  private def compareSegmentLists(
                                   path: Path,
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
        tracker(s, t + 1, acc :+ Some(SimpleSegmentDifference(path, nameOf(target(t)), canonicalNameOf(target(t)), Some((false, true)), None, None, None, Nil)))
      else if t == target.length then
        tracker(s + 1, t, acc :+ Some(SimpleSegmentDifference(path, nameOf(src(s)), canonicalNameOf(src(s)), Some((false, true)), None, None, None, Nil)))
      else if nameOf(src(s)) == nameOf(target(t)) then
        (src(s), target(t)) match {
          case (s1: RefinedSegmentSpec, t1: RefinedSegmentSpec) =>
            tracker(s + 1, t + 1, acc :+ compareTwoSegments(path, s1, t1))
          case (s2: RefinedLoopSpec, t2: RefinedLoopSpec) =>
            tracker(s + 1, t + 1, acc :+ compareTwoLoopSegments(path, s2, t2))
          case _ => tracker(s + 1, t + 1, acc :+ Some(SeriousDifference(path, nameOf(target(t)), canonicalNameOf(target(t)), "Types (loop vs segment) differ. They must match!")))
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
              case -1 =>
//                println("OOPS:  Can't reconcile --> "+srcName+" and "+targetName + " in "+path)
                acc :+ Some(SeriousDifference(path, "(unknown)", "", "Cannot reconcile src/target body elements"))
              case a =>
                val chunk = src.slice(s, a) // segments between t and j (exclusive)
                val diffs = chunk.map { seg =>
                  Some(SimpleSegmentDifference(
                    path,
                    nameOf(seg),
                    canonicalNameOf(seg),
                    Some((true, false)),
                    None, None, None, Nil
                  ))
                }
                tracker(a, t, acc ++ diffs)
            }
          case a =>
            val chunk = target.slice(t, a) // segments between t and j (exclusive)
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

    tracker(0, 0, Nil).flatten
  }


  private def compareTwoSegments(path: Path, a: RefinedSegmentSpec, b: RefinedSegmentSpec): Option[SimpleSegmentDifference] =
    val req = Option.when(a.required != b.required)((a.required, b.required))
    val assertions = Option.when(a.assertions.toSet != b.assertions.toSet)((a.assertions, b.assertions))
    val fieldDiffs = compareSegmentFields(path.dot(canonicalNameOf(a)), getFields(a), getFields(b))
    (req, assertions, fieldDiffs) match {
      case (None, None, Nil) => None
      case _ => Some(SimpleSegmentDifference(path, nameOf(a), canonicalNameOf(a), None, req, assertions, None, fieldDiffs))
    }

//  private def printMap(label: String, m: Map[String, List[(Path, RefinedSegmentSpec | RefinedLoopSpec)]]): Unit =
//    println(s"=== $label Segment Map ===")
//    val maxKeyLen = m.keys.map(_.length).maxOption.getOrElse(0)
//    m.foreach { case (key, entries) =>
//      entries.zipWithIndex.foreach {
//        case ((path, _), 0) => println(f"${key.padTo(maxKeyLen, ' ')}  ${path.toString}")
//        case ((path, _), _) => println(f"${" " * maxKeyLen}  ${path.toString}")
//      }
//    }

  private def compareTwoLoopSegments(path: Path, a: RefinedLoopSpec, b: RefinedLoopSpec, descendNesting: Boolean = true): Option[LoopSegmentDifference] =
    val req = Option.when(a.required != b.required)((a.required, b.required))
    val assertions = Option.when(a.assertions.toSet != b.assertions.toSet)((a.assertions, b.assertions))
    val fieldDiffs = compareSegmentFields(path.dot(canonicalNameOf(a)), getFields(a), getFields(b))
    val minDiff = Option.when(a.minRepeats != b.minRepeats)(a.minRepeats,b.minRepeats)
    val maxDiff = Option.when(a.maxRepeats != b.maxRepeats)(a.maxRepeats,b.maxRepeats)
    val bodyAndNest: (Option[List[SegmentDifference]], Option[List[HLDifference]]) =
      if equalIgnoringBrackets(canonicalNameOf(a), "HL") && descendNesting then
        val srcMap = buildSegmentPathMap(List(a), path)
        val targetMap = buildSegmentPathMap(List(b), path)

//        Show all the deep paths discovered for each element
//        printMap("Source",srcMap)
//        printMap("Target",targetMap)

        // Find any missing segments, regardless of path in src and target
        val sourceKeys = srcMap.keySet
        val targetKeys = targetMap.keySet
        val onlyInSource = (sourceKeys -- targetKeys).toList
        val onlyInTarget = (targetKeys -- sourceKeys).toList
        val inBoth = sourceKeys.intersect(targetKeys).toList

        val acc1 = onlyInSource.flatMap { k =>
          srcMap(k).map { case (elemPath, spec) =>
            HLDifference(elemPath.prefix, nameOf(spec), canonicalNameOf(spec), Some((true, false)), None, Nil)
          }
        }
        val acc2 = onlyInTarget.flatMap { k =>
          targetMap(k).map { case (elemPath, spec) =>
            HLDifference(elemPath.prefix, nameOf(spec), canonicalNameOf(spec), Some((false, true)), None, Nil)
          }
        }

        // Now track "moved" segments (NOTE: unordered!)
        val acc3 = inBoth.flatMap { k =>
          val srcElemPaths = srcMap(k)
          val targetElemPaths = targetMap(k)
          (srcElemPaths.size, targetElemPaths.size) match {
            // Exact mapping
            case (1,1) =>
              val (s1, s2) = srcElemPaths.head
              val (t1, t2) = targetElemPaths.head
              val loopDiffs = compareFoundOnPath((s1.prefix, s2), (t1.prefix, t2)).toList
              // See if the paths match and issue a HLDifference if not
              if equalIgnoringBrackets(s1.toString, t1.toString) then loopDiffs
                else HLDifference(path.prefix, nameOf(a), canonicalNameOf(a), None, Some((s1.toString, t1.toString)), Nil) :: loopDiffs

            // Equal mapping (presume same?)
            case (n,m) if n == m =>
              srcElemPaths.zip(targetElemPaths).flatMap { case (s, t) =>
                val (s1,s2) = s
                val (t1,t2) = t
                val loopDiffs = compareFoundOnPath((s1.prefix, s2), (t1.prefix, t2)).toList
                // See if the paths match and issue a HLDifference if not
                if equalIgnoringBrackets(s1.toString, t1.toString) then loopDiffs
                else HLDifference(path.prefix, nameOf(a), canonicalNameOf(a), None, Some((s1.toString, t1.toString)), Nil) :: loopDiffs
              }

            // 1-to-many: Possible nesting HLs from canonical spec
            case (1,n) =>
              val (s1, s2) = srcElemPaths.head
              targetElemPaths.map { tp =>
                val (t1,t2) = tp
                val loopDiffs = compareFoundOnPath((s1.prefix, s2), (t1.prefix, t2)).toList
                // See if the paths match and issue a HLDifference if not
                if equalIgnoringBrackets(s1.toString, t1.toString) then loopDiffs
                else HLDifference(path.prefix, nameOf(a), canonicalNameOf(a), None, Some((s1.toString, t1.toString)), Nil) :: loopDiffs
              }.toList.flatten

            // many-to-1: Possible nesting HLs from canonical spec
            case (n,1) =>
              val (t1, t2) = srcElemPaths.head
              srcElemPaths.map { s =>
                val (s1,s2) = s
                val loopDiffs = compareFoundOnPath((s1.prefix, s2), (t1.prefix, t2)).toList
                // See if the paths match and issue a HLDifference if not
                if equalIgnoringBrackets(s1.toString, t1.toString) then loopDiffs
                else HLDifference(path.prefix, nameOf(a), canonicalNameOf(a), None, Some((s1.toString, t1.toString)), Nil) :: loopDiffs
              }.toList.flatten

            // n-to-m: Who knows!
            case _ =>
              // Attempt to match what we can...
              val extracted = extractSame(srcElemPaths, targetElemPaths)
              val (matched, remainingSrc, remainingTarget) = extracted
              val matchedDiffs = matched.flatMap { case (a, b) => compareFoundOnPath(a, b) }

              // Give up on teh rest
              matchedDiffs ++ List(
                HLDifference(path.prefix, k, k, None, None, Nil, None,
                  Some((remainingSrc.map(_._1.toString),remainingTarget.map(_._1.toString))))
              )
          }
        }

        val allAcc: List[HLDifference] = acc1 ++ acc2 ++ acc3
        // pprint.log(allAcc)
        (None, Some(allAcc))

      else
        (Some(compareSegmentLists(path.dot(canonicalNameOf(a)), a.body, b.body)), None)
    val (bodyDiff, nestDiff) = bodyAndNest
    (req, assertions, fieldDiffs, minDiff, maxDiff, bodyDiff, nestDiff) match {
      case (None, None, Nil, None, None, None, None) => None
      case _ =>
        Some(LoopSegmentDifference(path, nameOf(a), canonicalNameOf(b), None, req, assertions, None, fieldDiffs, minDiff, maxDiff, if (bodyDiff.nonEmpty) bodyDiff else None, nestDiff))
    }

  private def compareFoundOnPath(srcKey: (Path, RefinedSegmentSpec | RefinedLoopSpec), targetKey: (Path, RefinedSegmentSpec | RefinedLoopSpec)): Option[HLDifference] =
    (srcKey._2, targetKey._2) match {
      case (a: RefinedSegmentSpec, b: RefinedSegmentSpec) =>
        compareTwoSegments(targetKey._1.prefix, a, b).map(d => HLDifference(srcKey._1.prefix, nameOf(srcKey._2), canonicalNameOf(srcKey._2), None, None, List(d)))
      case (a: RefinedLoopSpec, b: RefinedLoopSpec) =>
        compareTwoLoopSegments(targetKey._1.prefix, a, b, false).map(d => HLDifference(srcKey._1.prefix, nameOf(srcKey._2), canonicalNameOf(srcKey._2), None, None, List(d)))
      case (a, b) =>
        Some(HLDifference(srcKey._1.prefix, nameOf(a), canonicalNameOf(a), None, None, Nil, Some(s"Corresponding element at ${canonicalNameOf(b)} has different types")))
    }



  private def getFields(t: RefinedSegmentSpec | RefinedLoopSpec): List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec] =
    t match {
      case u: RefinedSegmentSpec => u.fields
      case v: RefinedLoopSpec => v.fields
    }


  private def compareSegmentFields(
                     path: Path,
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
          Some(SingleFieldDifference(path, fieldNameOf(a), canonicalFieldNameOf(a), presence = Some(true -> false)))

        case (None, Some(b)) =>
          Some(SingleFieldDifference(path, fieldNameOf(b), canonicalFieldNameOf(b), presence = Some(false -> true)))

        case (Some(a: RefinedSingleFieldSpec), Some(b: RefinedSingleFieldSpec)) =>
          val required = if (a.required != b.required) Some(a.required -> b.required) else None
          val dataType = if (a.dataType != b.dataType) Some(a.dataType -> b.dataType) else None
          val format = if (a.format != b.format) Some(a.format -> b.format) else None
          val elementId = if (a.elementId != b.elementId) Some(a.elementId -> b.elementId) else None
          val validValues = if (a.validValues.sorted != b.validValues.sorted) Some(a.validValues -> b.validValues) else None
          val validValuesRef = if (a.validValuesRef != b.validValuesRef) Some(a.validValuesRef -> b.validValuesRef) else None

          if (List(required, dataType, format, elementId, validValues, validValuesRef).exists(_.isDefined))
            Some(SingleFieldDifference(path, name, a.canonicalName, None, required, dataType, format, elementId, validValues, validValuesRef))
          else None

        case (Some(a: RefinedCompositeFieldSpec), Some(b: RefinedCompositeFieldSpec)) =>
          val required = if (a.required != b.required) Some(a.required -> b.required) else None
          val subDiffs = compareSegmentFields(path.dot(a.canonicalName), a.components, b.components)
          if (required.isDefined || subDiffs.nonEmpty)
            Some(CompositeFieldDifference(path, a.name, a.canonicalName, presence = None, required = required, fieldDiff = subDiffs))
          else None

        case _ => None
      }
    }
  }

  private def mergePathMaps(
                     maps: List[Map[String, List[(Path, RefinedSegmentSpec | RefinedLoopSpec)]]]
                   ): Map[String, List[(Path, RefinedSegmentSpec | RefinedLoopSpec)]] = {
    maps.flatten
      .groupBy(_._1)
      .view
      .mapValues(_.flatMap(_._2))
      .toMap
  }

//  private def cleanPath(path: String): String =
//    path.replaceAll(">(HL\\.HL)+", ">HL")

  private def buildSegmentPathMap(
                           specs: List[RefinedSegmentSpec | RefinedLoopSpec],
                           path: Path
                         ): Map[String, List[(Path, RefinedSegmentSpec | RefinedLoopSpec)]] = {
    specs.flatMap {
      case s: RefinedSegmentSpec =>
        val pathName = if equalIgnoringBrackets(s.canonicalName, "HL") && s.description.nonEmpty then s.canonicalName+s"[${s.description}]" else s.canonicalName
        val segPath = path.dot(canonicalNameOf(s))
        Map(canonicalNameOf(s) -> List((segPath, s)))

      case loop: RefinedLoopSpec =>
        val loopName = loop.canonicalName
        val newPath = path.dot(canonicalNameOf(loop))

        // Traverse body
        val bodyMap = buildSegmentPathMap(loop.body, newPath)

        // Traverse nested loops, if any, using '>' delimiter
        val nestedMap = loop.nested match {
          case Some(nestedLoops) =>
            nestedLoops.flatMap { nestedLoop =>
              val nestedPath = newPath //newPath.nest(canonicalNameOf(nestedLoop)) //cleanPath(s"$newnewPath>$pathName")
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


  //
  //  Utilities
  //
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
    case l: RefinedLoopSpec =>
      if l.canonicalName == "HL" && l.description.nonEmpty then
        l.canonicalName + s"[${l.description}]"
      else
        l.canonicalName
  }

  private def canonicalFieldNameOf(x: RefinedSingleFieldSpec | RefinedCompositeFieldSpec): String = x match {
    case s: RefinedSingleFieldSpec => s.canonicalName
    case l: RefinedCompositeFieldSpec => l.canonicalName
  }

  private def equalIgnoringBrackets(a: String, b: String): Boolean = {
    def stripBrackets(s: String): String =
      s.replaceAll("\\[.*?\\]", "")

    stripBrackets(a) == stripBrackets(b)
  }

  private def extractSame(
                   a: List[(Path, RefinedSegmentSpec | RefinedLoopSpec)],
                   b: List[(Path, RefinedSegmentSpec | RefinedLoopSpec)]
                 ): (List[((Path, RefinedSegmentSpec | RefinedLoopSpec), (Path, RefinedSegmentSpec | RefinedLoopSpec))], List[(Path, RefinedSegmentSpec | RefinedLoopSpec)], List[(Path, RefinedSegmentSpec | RefinedLoopSpec)]) = {

    def loop(
              remainingA: List[(Path, RefinedSegmentSpec | RefinedLoopSpec)],
              remainingB: List[(Path, RefinedSegmentSpec | RefinedLoopSpec)],
              acc: List[((Path, RefinedSegmentSpec | RefinedLoopSpec), (Path, RefinedSegmentSpec | RefinedLoopSpec))]
            ): (List[((Path, RefinedSegmentSpec | RefinedLoopSpec), (Path, RefinedSegmentSpec | RefinedLoopSpec))], List[(Path, RefinedSegmentSpec | RefinedLoopSpec)], List[(Path, RefinedSegmentSpec | RefinedLoopSpec)]) =
      remainingA match {
        case Nil => (acc.reverse, Nil, remainingB)
        case x :: xs =>
          remainingB.find(y => equalIgnoringBrackets(x._1.toString, y._1.toString)) match {
            case Some(matched) =>
              val newB = remainingB.filterNot(_ == matched) // remove only first match
              loop(xs, newB, (x -> matched) :: acc)
            case None =>
              val (matchedPairs, unmatchedA, unmatchedB) = loop(xs, remainingB, acc)
              (matchedPairs, x :: unmatchedA, unmatchedB)
          }
      }

    loop(a, b, Nil)
  }