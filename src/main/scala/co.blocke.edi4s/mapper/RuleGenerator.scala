package co.blocke.edi4s
package mapper

import model.*
import Availability.*

import scala.annotation.tailrec

// TODO: Problem... what to do if a loop seg is (OPTIONAL,REQUIRED) and in target all body segments are optional? Maybe put all placeholders?

object RuleGenerator:
  private type EnumFieldMap = Map[String, List[String | EnumeratedDependency]]

  def generate( diffs: List[SegmentDifference], enums: EnumFieldMap ): List[SegmentAssignment] =

    def collectHlPath(prunedDiff: LoopSegmentDifference): String =
      @annotation.tailrec
      def loop(current: Option[LoopSegmentDifference], acc: List[String]): String = current match
        case Some(ld) =>
          loop(ld.nested, ld.hlDiscriminator.getOrElse("") :: acc)
        case None =>
          acc.reverse.mkString

      loop(Some(prunedDiff), Nil)

    // Pre-scan for HL (LoopSegmentDifference) and build loop hierarchy string--used for BSN05 field
    val hlHierarchyBSN05: Option[String] =
      diffs.collectFirst { case l: LoopSegmentDifference if l.canonicalName.startsWith("HL") => collectHlPath(l) }
        .flatMap( h => hlStructureToBsn05.get(h) )

    def matchCaseAssignments(
                              f: SingleFieldDifference,
                              companion: Option[String]
                            ): Map[String, List[FieldAssignment]] = {
      val (srcValues, tgtValues) = f.validValues.get
      val canonicalName = f.canonicalName

      srcValues.distinct.map { srcVal =>
        val baseAssignment =
          if tgtValues.contains(srcVal) then
            DirectAssignment(canonicalName, f.availability, None)
          else
            PlaceholderAssignment(canonicalName, f.availability)

        val companionAssignment = companion.map { c =>
          if tgtValues.contains(srcVal) then
            DirectAssignment(c, f.availability, None)
          else
            PlaceholderAssignment(c, f.availability)
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
            if sf.availability == (MISSING,REQUIRED) then
              (PlaceholderAssignment( sf.canonicalName, sf.availability ), mappedAlready + sf.canonicalName)
            else if (sf.dataType.isEmpty || sf.dataType.get._1 == sf.dataType.get._2) &&
              (sf.format.isEmpty || sf.format.get._1 == sf.format.get._2) &&
              (sf.elementId.isEmpty || sf.elementId.get._1 == sf.elementId.get._2) &&
              (sf.validValues.isEmpty || sf.validValues.get._1 == sf.validValues.get._2) &&
              (sf.validValuesRef.isEmpty || sf.validValuesRef.get._1 == sf.validValuesRef.get._2) then
              (DirectAssignment(sf.canonicalName, sf.availability, None), mappedAlready + sf.canonicalName)
            else
              sf.validValues match {
                case Some(Nil, tvv) =>
                  if tvv.length == 1 then // may as well go ahead and assign the only valid target valid value
                    (ConstantAssignment(sf.canonicalName, tvv.head, sf.availability, None), mappedAlready + sf.canonicalName)
                  else // no valid src values, but valid target values exist--make human decide
                    (PlaceholderAssignment( sf.canonicalName, sf.availability ), mappedAlready + sf.canonicalName)
                // All good--all of the valid src values are contained in target -- direct assignment
                case Some((srcVV, targetVV)) if srcVV.forall(targetVV.contains) =>
                  // All src's valid values are present in target, so direct assignment is just fine
                  (DirectAssignment(sf.canonicalName, sf.availability, None), mappedAlready + sf.canonicalName)
                case Some((srcVV, targetVV)) => // mix: some present in src/target, others not
                  if srcVV.length == 1 then  // If just one src valid value, and it's not in target list, force human to set the correct value
                    (PlaceholderAssignment( sf.canonicalName, sf.availability ), mappedAlready + sf.canonicalName)
                  else
                    val companion = enumMap.get(sf.canonicalName).flatten
                    (
                      MatchFieldAssignment(sf. canonicalName, matchCaseAssignments(sf, companion), sf.availability, None),
                      {
                        if companion.isEmpty then mappedAlready + sf.canonicalName
                        else mappedAlready + sf.canonicalName + companion.get
                      }
                    )
                case _ =>
                  (PlaceholderAssignment( sf.canonicalName, sf.availability ), mappedAlready + sf.canonicalName)
              }

          // TODO: case for CompositeFieldDifference
        }

      @tailrec
      def loop(f: List[FieldDifference], acc: List[FieldAssignment], mappedAlready: Set[String] ): (List[FieldAssignment], Set[String]) =
        if f.isEmpty then (acc, mappedAlready)
        else f.head match {
          case fd: FieldDifference if mappedAlready.contains(fd.canonicalName) =>
            (acc, mappedAlready) // skip fields we've already mapped

          case fd: FieldDifference =>
            // Special cases
            // 1. BSN05 - encoded "map" of HL structure (lookup in package.hlStructureToBsn05)
            if fd.canonicalName == "BSN05" then
              loop(
                f.tail,
                acc :+ ConstantAssignment( fd.canonicalName, hlHierarchyBSN05.getOrElse(""), fd.availability, None ),
                mappedAlready + "BSN05"
              )
            else
              val (oneRule, newMapped) = genOneFieldRule( fd, mappedAlready )

              // Now consider optionality. Drop any rules that are human-populated that are not required in target.
              // Also consider src:Optional/target:Required fields and wrap that rule in a OrElseFieldAssignment if *not* human-populated already
//              fd.availability match {
//                case (MISSING,REQUIRED) if !oneRule.isPlaceholder => // wrap if needed
//                  loop(f.tail, acc :+ OrElseFieldAssignment(fd.canonicalName, oneRule, "???", ValueKind.Constant, true), newMapped)
//                case (MISSING,OPTIONAL) => // drop placeholder if not required in target
//                  //
//                  // NOTE: We may want *NOT* to drop these if users prefer to manually set target-optional fields
//                  //
//                  loop(f.tail, acc, mappedAlready)
//                case _ =>
//                  loop(f.tail, acc :+ oneRule, newMapped)
//              }
              loop(f.tail, acc :+ oneRule, newMapped)
        }

      loop( fs, Nil, Set.empty[String] )._1


    inline def replaceTypeParam(s: String, newParam: String): String =
      s.replaceAll("""\[\s*[^]]+\s*\]""", s"[$newParam]")

    def genSegmentRule( seg: SegmentDifference ): Option[SegmentAssignment] =
      val fieldsPresentInTarget = seg.fieldDiff.filter(f => f.availability._2 != MISSING && !(f.availability == (MISSING,REQUIRED)))

      (seg,seg.availability) match {

        case (_,(MISSING,MISSING)) => // no rule if missing on both sides
          None

        case (_,(_,MISSING)) =>  // present in src/missing in target => generate a placeholder no-op rule to keep alignment during rule processing
          Some(NoOpSegmentAssignment(seg.canonicalName, seg.availability))

        // Skip ST/SE segments b/c we handle them as first-class parsed objects rather than general X12 segments
        case (s,(_,_)) if s.canonicalName == "ST" || s.canonicalName == "SE" =>
          None

        case (_,(MISSING,OPTIONAL)) => // ignore whenever src is missing and target is optional--no need to map anything
          None

        case (s:SingleSegmentDifference, (MISSING,REQUIRED)) =>  // missing from src, required in target
          val assignFields = s.fieldDiff.map(f => PlaceholderAssignment(f.canonicalName, f.availability)) // must be placeholder--no src to map from!
          Some(SingleSegmentAssignment(seg.canonicalName, assignFields, s.availability, None))

        case (s:LoopSegmentDifference, (MISSING,REQUIRED)) =>  // missing from src, required in target
          val assignFields = s.fieldDiff.map(f => PlaceholderAssignment(f.canonicalName, f.availability)) // must be placeholder--no src to map from!
          Some(
            LoopSegmentAssignment(
              s.hlDiscriminator.map(d => replaceTypeParam(s.canonicalName, d)).getOrElse(s.canonicalName),
              assignFields,
              generate(s.bodyDiff, enums),
              None,
              s.availability,
              None
            )
          )

        case (s:SingleSegmentDifference, (a, b)) if a == b =>  // same availability in src and target
          val assignFields = genFieldRules(s.fieldDiff, enums.getOrElse(s.canonicalName, Nil))
          Some(SingleSegmentAssignment(s.canonicalName, assignFields, s.availability, None))

        case (s:LoopSegmentDifference, (a, b)) if a == b => // same availability in src and target
          val assignFields = genFieldRules(s.fieldDiff, enums.getOrElse(s.canonicalName, Nil))
          val nestAssign = s.nested.flatMap(genSegmentRule).asInstanceOf[Option[LoopSegmentAssignment]]
          Some(
            LoopSegmentAssignment(
              s.hlDiscriminator.map(d => replaceTypeParam(s.canonicalName, d)).getOrElse(s.canonicalName),
              assignFields,
              generate(s.bodyDiff, enums),
              nestAssign,
              s.availability,
              None
            )
          )

        case (s:SingleSegmentDifference, (OPTIONAL,REQUIRED)) => // src optional/target required
          Some(
            SingleSegmentAssignment(
              s.canonicalName,
              genFieldRules(s.fieldDiff, enums.getOrElse(s.canonicalName, Nil)),
              s.availability,
              Some(
                SingleSegmentAssignment(
                  s.canonicalName,
                  fieldsPresentInTarget.map(f => PlaceholderAssignment(f.canonicalName, f.availability)),
                  s.availability,
                  None
                )
              )
            )
          )

        case (s:LoopSegmentDifference, (OPTIONAL,REQUIRED)) => // src optional/target required
          val nestAssign = s.nested.flatMap(genSegmentRule).asInstanceOf[Option[LoopSegmentAssignment]]
          Some(
            LoopSegmentAssignment(
              s.hlDiscriminator.map(d => replaceTypeParam(s.canonicalName, d)).getOrElse(s.canonicalName),
              genFieldRules(s.fieldDiff, enums.getOrElse(s.canonicalName, Nil)),
              generate(s.bodyDiff, enums),
              nestAssign,
              s.availability,
              Some(
                LoopSegmentAssignment(
                  s.hlDiscriminator.map(d => replaceTypeParam(s.canonicalName, d)).getOrElse(s.canonicalName),
                  s.fieldDiff.map(f => PlaceholderAssignment(f.canonicalName, f.availability)),
                  generate(s.bodyDiff, enums),
                  None,  // don't descend nesting if no src is available--nothing to map
                  s.availability,
                  None
                )
              )
            )
          )

        // Cases we don't care about: e.g. not present in target -- should never happen
        case _ =>
          None
      }

    diffs.flatMap(genSegmentRule)  // skip any segments not present in target then generate

