package co.blocke.edi4s
package mapper

import model.*

import scala.annotation.tailrec

object RuleGenerator:
  private type EnumFieldMap = Map[String, List[String | EnumeratedDependency]]

  def generate( diffs: List[SegmentDifference], enums: EnumFieldMap ): List[SegmentAssignment] =

    def matchCaseAssignment( v: String, possibles: List[String], canonicalName: String ): FieldAssignment =
      if possibles.contains(v) then
        GeneralFieldAssignment( canonicalName, canonicalName, ValueKind.Direct, false)
      else
        GeneralFieldAssignment(canonicalName, "???", ValueKind.Constant, true)

    // Need some way to track what fields are handled so we don't gen them twice (match case)
    // Possibly pass some acc?
    // Also need to return (Option[FieldAssignment], acc)
    def genFieldRules( fs: List[FieldDifference], enumList: List[String | EnumeratedDependency] ): List[FieldAssignment] =
      val enumMap = enumList.map {
        case s: String => s -> None
        case ed: EnumeratedDependency => ed.field -> Some(ed.companionField)
      }.toMap

      @tailrec
      def loop(f: List[FieldDifference], acc: List[FieldAssignment], mappedAlready: Set[String] ): (List[FieldAssignment], Set[String]) =
        if f.isEmpty then (acc, mappedAlready)
        else f.head match {
          case sf: FieldDifference if mappedAlready.contains(sf.canonicalName) =>
            (acc, mappedAlready) // skip fields we've already mapped

          case sf: SingleFieldDifference if sf.required._1 || sf.required == (false,false) =>
            if (sf.dataType.isEmpty || sf.dataType.get._1 == sf.dataType.get._2) &&
              (sf.format.isEmpty || sf.format.get._1 == sf.format.get._2) &&
              (sf.elementId.isEmpty || sf.elementId.get._1 == sf.elementId.get._2) &&
              (sf.validValues.isEmpty || sf.validValues.get._1 == sf.validValues.get._2) &&
              (sf.validValuesRef.isEmpty || sf.validValuesRef.get._1 == sf.validValuesRef.get._2) then
              loop(f.tail, acc :+ GeneralFieldAssignment( sf.canonicalName, sf.canonicalName, ValueKind.Direct, false), mappedAlready + sf.canonicalName)
            else if sf.validValues.nonEmpty then // specificed, but mis-matched validValues == enum field
              // See if this is a stand-alone or dependent enum field
              if sf.validValues.get._1.size == 1 then // just 1 specified--treat as normal assignment
                if sf.validValues.get._2.contains(sf.validValues.get._1.head) then
                  // 1 valid value in src, and that value is present in target
                  loop(f.tail, acc :+ GeneralFieldAssignment( sf.canonicalName, sf.canonicalName, ValueKind.Direct, false), mappedAlready + sf.canonicalName)
                else
                  // 1 valid value in src but that value is unknown in target
                  loop(f.tail, acc :+ GeneralFieldAssignment( sf.canonicalName, "???", ValueKind.Constant, true), mappedAlready + sf.canonicalName)
              else
                if !enumMap.contains(sf.canonicalName) then
                  loop(f.tail, acc :+ GeneralFieldAssignment(sf.canonicalName, "???", ValueKind.Constant, true), mappedAlready + sf.canonicalName)
                else enumMap(sf.canonicalName) match {
                  case Some(companion) =>  // dependent enum (eg unit conversions)
                    val efa = ??? // TODO: dependent enums
                    loop(f.tail, acc :+ efa, mappedAlready + sf.canonicalName + companion)
                  case None => // stand-alone enum
                    val efa = EnumMatchFieldAssignment(
                      sf.canonicalName,
                      sf.validValues.get._1.map( vv => (vv -> List(matchCaseAssignment(vv, sf.validValues.get._2, sf.canonicalName)))).toMap,
                      false
                    )
                    loop(f.tail, acc :+ efa, mappedAlready + sf.canonicalName)
                }
            else
              loop(f.tail, acc :+ GeneralFieldAssignment( sf.canonicalName, "???", ValueKind.Constant, true), mappedAlready + sf.canonicalName)
              
          case sf =>
            loop(f.tail, acc :+ GeneralFieldAssignment( sf.canonicalName, "???", ValueKind.Constant, true), mappedAlready + sf.canonicalName)
        }
      loop( fs, Nil, Set.empty[String] )._1

    // Main generate() starts here ---->
    diffs.collect{
      // Target present, src/target both required -or- src required/target optional -or- src/target optional
      case s: SimpleSegmentDifference if s.presence._2 && (s.required._1 || s.required == (false,false)) =>
        // Filter out any fields not present in target--don't care about these
        val presentInTarget = s.fieldDiff.filter(_.presence._2)
        FieldsSegmentAssignment(s.canonicalName, genFieldRules(presentInTarget, enums.getOrElse(s.canonicalName, Nil)))

      // Target present, src optional/target required
      case s: SimpleSegmentDifference if s.presence._2 && s.required == (false, true) =>
        val presentInTarget = s.fieldDiff.filter(_.presence._2)
        val someAssign =
          FieldsSegmentAssignment(s.canonicalName, genFieldRules(presentInTarget, enums.getOrElse(s.canonicalName, Nil)))
        val noneAssign = {
          val noneFields = presentInTarget.map(f => GeneralFieldAssignment(f.canonicalName, "???", ValueKind.Constant, true))
          FieldsSegmentAssignment(s.canonicalName, noneFields)
        }
        OrElseFieldsSegmentAssignment(s.canonicalName, someAssign, noneAssign)

      // TODO: Loop segments
    }


  /*
case class EnumMatchFieldAssignment(
                                  matchField: String, // usually a REF01-like field
                                  cases: Map[String, List[FieldAssignment]],
                                  isPlaceholder: Boolean = false
                                ) extends FieldAssignment
    case class SimpleSegmentDifference(
                                    path: Path,
                                    name: String,
                                    canonicalName: String,
                                    presence: (Boolean,Boolean),
                                    required: (Boolean,Boolean),
                                    assertions: Option[(List[String],List[String])] = None,
                                    fieldDiff: List[FieldDifference]
                                  ) extends SegmentDifference

case class GeneralFieldAssignment(
                                  targetField: String,
                                  value: String,
                                  valueKind: ValueKind,
                                  isPlaceholder: Boolean = false
                                ) extends FieldAssignment

  def generateMappingSpec(differences: List[SegmentDifference], enumMap: EnumFieldMap): MappingSpec =
    def findEnumCompanion(segment: String, field: String): Option[String] =
      enumMap.getOrElse(segment, Nil).collectFirst {
        case EnumeratedDependency(`field`, companion) => companion
      }

    def isStandaloneEnum(segment: String, field: String): Boolean =
      enumMap.getOrElse(segment, Nil).contains(field)

    def fieldAssignment(fd: FieldDifference): Option[FieldAssignment] = fd match
      case s: SingleFieldDifference if !s.presence._2 => None
      case s: SingleFieldDifference if s.validValues.exists((src, tgt) => src != tgt) =>
        val segment = s.path.value.split('.').lastOption.getOrElse("")

        findEnumCompanion(segment, s.canonicalName) match
          case Some(companionField) =>
            val srcValues = s.validValues.map(_._1).getOrElse(Nil)
            val tgtValues = s.validValues.map(_._2).getOrElse(Nil)

            val uniquePairs = srcValues.distinct.map { srcVal =>
              val (descAssign, valueAssign) =
                if tgtValues.contains(srcVal) then
                  (
                    DirectFieldAssignment(targetField = s.canonicalName, sourceField = s.canonicalName),
                    DirectFieldAssignment(targetField = companionField, sourceField = companionField)
                  )
                else
                  (
                    ConstantFieldAssignment(targetField = s.canonicalName, constantValue = tgtValues.headOption.getOrElse("???"), isPlaceholder = true),
                    ContextFieldAssignment(targetField = companionField, contextPath = "ctx.vendor", isPlaceholder = true)
                  )
              srcVal -> List(descAssign, valueAssign)
            }.toMap

            Some(EnumMatchFieldAssignment(
              matchField = s.canonicalName,
              cases = uniquePairs,
              isPlaceholder = true
            ))

          case None if isStandaloneEnum(segment, s.canonicalName) =>
            val srcValues = s.validValues.map(_._1).getOrElse(Nil)
            val tgtValues = s.validValues.map(_._2).getOrElse(Nil)

            val uniqueCases = srcValues.distinct.map { srcVal =>
              val assignment =
                if tgtValues.contains(srcVal) then
                  DirectFieldAssignment(targetField = s.canonicalName, sourceField = s.canonicalName)
                else
                  ConstantFieldAssignment(targetField = s.canonicalName, constantValue = tgtValues.headOption.getOrElse("???"), isPlaceholder = true)
              srcVal -> List(assignment)
            }.toMap

            Some(EnumMatchFieldAssignment(
              matchField = s.canonicalName,
              cases = uniqueCases,
              isPlaceholder = true
            ))

          case _ =>
            Some(ContextFieldAssignment(
              targetField = s.canonicalName,
              contextPath = "???",
              isPlaceholder = true
            ))

      case s: SingleFieldDifference =>
        val assignment = (s.presence, s.required) match
          case ((false, true), _) =>
            ContextFieldAssignment(
              targetField = s.canonicalName,
              contextPath = "???",
              isPlaceholder = true
            )
          case ((true, false), (true, true)) =>
            OrElseFieldAssignment(
              targetField = s.canonicalName,
              sourceField = s.canonicalName,
              orElseValue = "???",
              orElseIsContext = true,
              isPlaceholder = true
            )
          case ((true, true), _) =>
            DirectFieldAssignment(
              targetField = s.canonicalName,
              sourceField = s.canonicalName
            )
          case _ =>
            ContextFieldAssignment(
              targetField = s.canonicalName,
              contextPath = "???",
              isPlaceholder = true
            )
        Some(assignment)

      case c: CompositeFieldDifference =>
        Some(ContextFieldAssignment(
          targetField = c.canonicalName,
          contextPath = "???",
          isPlaceholder = true
        ))

    def segmentAssignment(sd: SegmentDifference): SegmentAssignment = sd match
      case s: SimpleSegmentDifference if !s.presence._1 && s.presence._2 =>
        val placeholderFields: List[FieldAssignment] = s.fieldDiff.collect {
          case f: SingleFieldDifference if f.presence._2 =>
            ConstantFieldAssignment(
              targetField = f.canonicalName,
              constantValue = "???",
              isPlaceholder = true
            )
        }
        FieldsSegmentAssignment(
          canonicalName = s.canonicalName,
          fieldAssignments = placeholderFields
        )

      case s: SimpleSegmentDifference =>
        val assignments = s.fieldDiff.flatMap(fieldAssignment)
        FieldsSegmentAssignment(
          canonicalName = s.canonicalName,
          fieldAssignments = assignments,
          body = Nil
        )

      case l: LoopSegmentDifference =>
        val assignments = l.fieldDiff.flatMap(fieldAssignment)
        FieldsSegmentAssignment(
          canonicalName = l.canonicalName,
          fieldAssignments = assignments,
          body = l.bodyDiff.map(segmentAssignment)
        )

      case _: DifferenceError =>
        throw new Exception("ERROR!")

    MappingSpec(
      rules = differences.map(segmentAssignment)
    )
  */
