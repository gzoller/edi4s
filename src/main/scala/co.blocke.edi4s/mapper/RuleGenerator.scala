package co.blocke.edi4s
package mapper

import model.*

import scala.annotation.tailrec

object RuleGenerator:
  private type EnumFieldMap = Map[String, List[String | EnumeratedDependency]]

  def generate( diffs: List[SegmentDifference], enums: EnumFieldMap ): List[SegmentAssignment] =

    def collectHlPath(diff: LoopSegmentDifference): String =
      @annotation.tailrec
      def loop(current: Option[LoopSegmentDifference], acc: List[String]): String = current match
        case Some(ld) =>
          loop(ld.nested, ld.hlDiscriminator.getOrElse("") :: acc)
        case None =>
          acc.reverse.mkString

      loop(Some(diff), Nil)

    // Pre-scan for HL (LoopSegmentDifference) and build loop hierarchy string--used for BSN05 field
    val hlHierarchyBSN05: Option[String] =
      diffs.collectFirst { case l: LoopSegmentDifference if l.canonicalName.startsWith("HL") => collectHlPath(l) }
        .flatMap( h => hlStructureToBsn05.get(h) )

    def matchCaseAssignments(
                              valids: (List[String], List[String]),
                              canonicalName: String,
                              companion: Option[String]
                            ): Map[String, List[FieldAssignment]] = {
      val (srcValues, tgtValues) = valids

      srcValues.distinct.map { srcVal =>
        val baseAssignment =
          if tgtValues.contains(srcVal) then
            GeneralFieldAssignment(canonicalName, canonicalName, ValueKind.Direct, false)
          else
            GeneralFieldAssignment(canonicalName, "???", ValueKind.Constant, true)

        val companionAssignment = companion.map { c =>
          if tgtValues.contains(srcVal) then
            GeneralFieldAssignment(c, c, ValueKind.Direct, false)
          else
            GeneralFieldAssignment(c, "???", ValueKind.Constant, true)
        }

        srcVal -> (baseAssignment :: companionAssignment.toList)
      }.toMap
    }

    // Need some way to track what fields are handled so we don't gen them twice (match case)
    // Possibly pass some acc?
    // Also need to return (Option[FieldAssignment], acc)
    def genFieldRules( fs: List[FieldDifference], enumList: List[String | EnumeratedDependency] ): List[FieldAssignment] =
      val enumMap = enumList.map {
        case s: String => s -> None
        case ed: EnumeratedDependency => ed.field -> Some(ed.companionField)
      }.toMap

      def genOneFieldRule( fd: FieldDifference, mappedAlready: Set[String] ): (FieldAssignment, Set[String]) =
        fd match {
          case sf: SingleFieldDifference =>
            if (sf.dataType.isEmpty || sf.dataType.get._1 == sf.dataType.get._2) &&
              (sf.format.isEmpty || sf.format.get._1 == sf.format.get._2) &&
              (sf.elementId.isEmpty || sf.elementId.get._1 == sf.elementId.get._2) &&
              (sf.validValues.isEmpty || sf.validValues.get._1 == sf.validValues.get._2) &&
              (sf.validValuesRef.isEmpty || sf.validValuesRef.get._1 == sf.validValuesRef.get._2) then
              (GeneralFieldAssignment( sf.canonicalName, sf.canonicalName, ValueKind.Direct, false), mappedAlready + sf.canonicalName)
            else
              sf.validValues match {
                case Some(Nil, tvv) =>
                  if tvv.length == 1 then // may as well go ahead and assign the only valid target valid value
                    (GeneralFieldAssignment( sf.canonicalName, tvv.head, ValueKind.Constant, false), mappedAlready + sf.canonicalName)
                  else
                    (GeneralFieldAssignment( sf.canonicalName, sf.canonicalName, ValueKind.Direct, true), mappedAlready + sf.canonicalName)
                // All good--all of the valid src values are contained in target -- direct assignment
                case Some((srcVV, targetVV)) if srcVV.forall(targetVV.contains) =>
                  // All src's valid values are present in target, so direct assignment is just fine
                  (GeneralFieldAssignment( sf.canonicalName, sf.canonicalName, ValueKind.Direct, false), mappedAlready + sf.canonicalName)
                case Some((srcVV, targetVV)) => // mix: some present in src/target, others not
                  if srcVV.length == 1 then  // If just one src valid value, and it's not in target list, force human to set the correct value
                    (GeneralFieldAssignment( sf.canonicalName, sf.canonicalName, ValueKind.Direct, true), mappedAlready + sf.canonicalName)
                  else
                    val companion = enumMap.get(sf.canonicalName).flatten
                    (
                      EnumMatchFieldAssignment(sf.canonicalName, matchCaseAssignments(sf.validValues.get, sf.canonicalName, companion)),
                      {
                        if companion.isEmpty then mappedAlready + sf.canonicalName
                        else mappedAlready + sf.canonicalName + companion.get
                      }
                    )
                case _ =>
                  (GeneralFieldAssignment( sf.canonicalName, "???", ValueKind.Constant, true), mappedAlready + sf.canonicalName)
              }

          // TODO: case for CompositeFieldDifference
        }

      @tailrec
      def loop(f: List[FieldDifference], acc: List[FieldAssignment], mappedAlready: Set[String] ): (List[FieldAssignment], Set[String]) =
        if f.isEmpty then (acc, mappedAlready)
        else f.head match {
          case sf: FieldDifference if mappedAlready.contains(sf.canonicalName) =>
            (acc, mappedAlready) // skip fields we've already mapped

          case fd: FieldDifference =>

            // Special cases
            if fd.canonicalName == "BSN05" then
              loop(
                f.tail,
                acc :+ GeneralFieldAssignment( fd.canonicalName, hlHierarchyBSN05.getOrElse(""), ValueKind.Constant, false),
                mappedAlready + "BSN05"
              )
            else
              val (oneRule, newMapped) = genOneFieldRule( fd, mappedAlready )

              // Now consider optionality. Drop any rules that are human-populated that are not required in target.
              // Also consider src:Optional/target:Required fields and wrap that rule in a OrElseFieldAssignment if *not* human-populated already
              fd.required match {
                case (false,true) if !oneRule.isPlaceholder => // wrap if needed
                  loop(f.tail, acc :+ OrElseFieldAssignment(fd.canonicalName, oneRule, "???", ValueKind.Constant, true), newMapped)
                case (x,false) if !fd.presence._1 || oneRule.isPlaceholder => // drop placeholder if not required in target
                  //
                  // NOTE: We may want *NOT* to drop these if users prefer to manually set target-optional fields
                  //
                  loop(f.tail, acc, mappedAlready)
                case _ =>
                  loop(f.tail, acc :+ oneRule, newMapped)
              }
        }

      loop( fs, Nil, Set.empty[String] )._1


    inline def replaceTypeParam(s: String, newParam: String): String =
      s.replaceAll("""\[\s*[^]]+\s*\]""", s"[$newParam]")

    def genSegmentRule( seg: SegmentDifference ): Option[SegmentAssignment] =
      import DiffRelevance.*
      val fieldsPresentInTarget = seg.fieldDiff.filter(f => f.presence._2 && !(!f.presence._1 && !f.required._2))
      seg match {

        case _ if seg.relevance == SRC_PRESENT_TARGET_MISSING =>
//        case _ if seg.presence == (true,false) =>
          Some(NoOpSegmentAssignment(seg.canonicalName))

        // Skip ST/SE segments b/c we handle them as first-class parsed objects rather than general X12 segments
        case _ if seg.relevance == TARGET_MISSING || seg.relevance == SRC_MISSING_TARGET_OPTIONAL || seg.canonicalName == "ST" || seg.canonicalName == "SE" =>
//        case _ if seg.presence == (false,false) || (!seg.presence._1 && !seg.required._2) || seg.canonicalName == "ST" || seg.canonicalName == "SE" =>
          None

        case s: SimpleSegmentDifference if seg.relevance == SRC_MISSING_TARGET_REQ =>  // missing from src, required in target
//        case s: SimpleSegmentDifference if seg.presence == (false,true) && seg.required._2 =>  // missing from src, required in target
          val noneFields = fieldsPresentInTarget.map(f => GeneralFieldAssignment(f.canonicalName, "???", ValueKind.Constant, true))
          Some(FieldsSegmentAssignment(seg.canonicalName, noneFields, true))

        case s: LoopSegmentDifference if seg.relevance == SRC_MISSING_TARGET_REQ =>  // missing from src, required in target
//        case s: LoopSegmentDifference if seg.presence == (false,true) && seg.required._2 =>  // missing from src, required in target
          val noneAssign = { // segment missing in src--assign all fields manually
            val bodyAssign = generate(s.bodyDiff, enums)
            val nestAssign = s.nested.flatMap(genSegmentRule).asInstanceOf[Option[LoopSegmentAssignment]]
            LoopSegmentAssignment(
              s.hlDiscriminator.map(d => replaceTypeParam(s.canonicalName, d)).getOrElse(s.canonicalName),
              fieldsPresentInTarget.map(f => GeneralFieldAssignment(f.canonicalName, "???", ValueKind.Constant, true)),
              bodyAssign,
              nestAssign,
              true
            )
          }
          Some(noneAssign)

        case s: SimpleSegmentDifference if s.relevance == MATCH =>
//        case s: SimpleSegmentDifference if s.required._1 || s.required == (false,false) =>
          Some(FieldsSegmentAssignment(s.canonicalName, genFieldRules(fieldsPresentInTarget, enums.getOrElse(s.canonicalName, Nil))))

        case s: LoopSegmentDifference if s.relevance == MATCH =>
          //        case s: LoopSegmentDifference if s.required._1 || s.required == (false,false) =>
          val bodyAssign = generate(s.bodyDiff, enums)
          val nestAssign = s.nested.flatMap(genSegmentRule).asInstanceOf[Option[LoopSegmentAssignment]]
          Some(LoopSegmentAssignment(
            s.hlDiscriminator.map(d => replaceTypeParam(s.canonicalName, d)).getOrElse(s.canonicalName),
            genFieldRules(fieldsPresentInTarget, enums.getOrElse(s.canonicalName, Nil)),
            bodyAssign,
            nestAssign)
          )

        // Target present, src optional/target required
        case s: SimpleSegmentDifference if s.relevance == SRC_OPT_TARGET_REQ =>
//        case s: SimpleSegmentDifference if s.required == (false, true) =>
          val someAssign =
            FieldsSegmentAssignment(s.canonicalName, genFieldRules(fieldsPresentInTarget, enums.getOrElse(s.canonicalName, Nil)))
          val noneAssign = {
            val noneFields = fieldsPresentInTarget.map(f => GeneralFieldAssignment(f.canonicalName, "???", ValueKind.Constant, true))
            FieldsSegmentAssignment(s.canonicalName, noneFields)
          }
          Some(OrElseFieldsSegmentAssignment(s.canonicalName, someAssign, noneAssign))

        case s: LoopSegmentDifference if s.relevance == SRC_OPT_TARGET_REQ =>
//        case s: LoopSegmentDifference if s.required == (false, true) =>
          val bodyAssign = generate(s.bodyDiff, enums)
          val nestAssign = s.nested.flatMap(genSegmentRule).asInstanceOf[Option[LoopSegmentAssignment]]
          val someAssign =  // segment present in src--assign normally
            LoopSegmentAssignment(
              s.hlDiscriminator.map(d => replaceTypeParam(s.canonicalName, d)).getOrElse(s.canonicalName),
              genFieldRules(fieldsPresentInTarget, enums.getOrElse(s.canonicalName, Nil)),
              bodyAssign,
              nestAssign
            )
          val noneAssign = { // segment missing in src--assign all fields manually
            LoopSegmentAssignment(
              s.hlDiscriminator.map(d => replaceTypeParam(s.canonicalName, d)).getOrElse(s.canonicalName),
              fieldsPresentInTarget.map(f => GeneralFieldAssignment(f.canonicalName, "???", ValueKind.Constant, true)),
              bodyAssign,
              nestAssign
            )
          }
          Some(OrElseFieldsSegmentAssignment(s.canonicalName, someAssign, noneAssign))

        // Cases we don't care about: e.g. not present in target -- should never happen
        case _ =>
          None
      }

    diffs.flatMap(genSegmentRule)  // skip any segments not present in target then generate

