package co.blocke.edi4s
package mapper

import model.*

import scala.annotation.tailrec

object RuleGenerator:
  private type EnumFieldMap = Map[String, List[String | EnumeratedDependency]]

  def generate( diffs: List[SegmentDifference], enums: EnumFieldMap ): List[SegmentAssignment] =

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
            val (oneRule, newMapped) = genOneFieldRule( fd, mappedAlready )

            // Now consider optionality. Drop any rules that are human-populated that are not required in target.
            // Also consider src:Optional/target:Requierd fields and wrap that rule in a OrElseFieldAssignment if *not* human-populated already
            fd.required match {
              case (x,false) if oneRule.isPlaceholder => // drop placeholder if not required in target
                //
                // NOTE: We may want *NOT* to drop these if users prefer to manually set target-optional fields
                //
                loop(f.tail, acc, mappedAlready)
              case (false,true) if !oneRule.isPlaceholder => // wrap if needed
                loop(f.tail, acc :+ OrElseFieldAssignment(oneRule, "???", ValueKind.Constant, true), newMapped)
              case _ =>
                loop(f.tail, acc :+ oneRule, newMapped)
            }
        }

      loop( fs, Nil, Set.empty[String] )._1


    def genSegmentRule( seg: SegmentDifference ): Option[SegmentAssignment] =
      val fieldsPresentInTarget = seg.fieldDiff.filter(_.presence._2)
      seg match {
        // Target present, src/target both required -or- src required/target optional -or- src/target optional
        case s: SimpleSegmentDifference if s.required._1 || s.required == (false,false) =>
          Some(FieldsSegmentAssignment(s.canonicalName, genFieldRules(fieldsPresentInTarget, enums.getOrElse(s.canonicalName, Nil))))

        // Target present, src optional/target required
        case s: SimpleSegmentDifference if s.required == (false, true) =>
          val someAssign =
            FieldsSegmentAssignment(s.canonicalName, genFieldRules(fieldsPresentInTarget, enums.getOrElse(s.canonicalName, Nil)))
          val noneAssign = {
            val noneFields = fieldsPresentInTarget.map(f => GeneralFieldAssignment(f.canonicalName, "???", ValueKind.Constant, true))
            FieldsSegmentAssignment(s.canonicalName, noneFields)
          }
          Some(OrElseFieldsSegmentAssignment(s.canonicalName, someAssign, noneAssign))

        case s: LoopSegmentDifference if s.required._1 || s.required == (false,false) =>
          val bodyAssign = generate(s.bodyDiff, enums)
          val nestAssign = s.nested.flatMap(genSegmentRule).asInstanceOf[Option[LoopSegmentAssignment]]
          Some(LoopSegmentAssignment(
            s.canonicalName,
            genFieldRules(fieldsPresentInTarget, enums.getOrElse(s.canonicalName, Nil)),
            bodyAssign,
            nestAssign)
          )

        case s: LoopSegmentDifference if s.required == (false, true) =>
          val bodyAssign = generate(s.bodyDiff, enums)
          val nestAssign = s.nested.flatMap(genSegmentRule).asInstanceOf[Option[LoopSegmentAssignment]]
          val someAssign =  // segment present in src--assign normally
            LoopSegmentAssignment(
              s.canonicalName,
              genFieldRules(fieldsPresentInTarget, enums.getOrElse(s.canonicalName, Nil)),
              bodyAssign,
              nestAssign
            )
          val noneAssign = { // segment missing in src--assign all fields manually
            LoopSegmentAssignment(
              s.canonicalName,
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

    diffs.filter(_.presence._2).flatMap(genSegmentRule)  // skip any segments not present in target then generate

