package co.blocke.edi4s.model


sealed trait Difference:
  val name: String
  val canonicalName: String
  val presence: Option[(Boolean,Boolean)]
  val required: Option[(Boolean,Boolean)]

  def subRender(path: String, soFar: List[List[String]], nestLevel: Int): List[List[String]]
  def render(path: String, soFar: List[List[String]], nestLevel: Int = -1): List[List[String]] =
    val presenceList = presence match {
      case Some((true, false)) => List(s"${path}.$canonicalName ($name)", "!present in source - missing in target")
      case Some((false, true)) => List(s"${path}.$canonicalName ($name)", "!missing in source - present in target")
      case _ => Nil
    }
    val requiredList = required match {
      case Some((s, t)) => List(">Required", s.toString, t.toString)
      case _ => Nil
    }
    val allDiffs = (List(requiredList) ++ subRender(path, Nil, nestLevel)).filter(_.nonEmpty)
    if presenceList.nonEmpty then
      soFar ++ List(presenceList)
    else if allDiffs.nonEmpty then
      if nestLevel >= 0 then
        soFar ++ List(List(s"!(nest level $nestLevel) ${path}.$canonicalName ($name)")) ++ allDiffs ++ List(List(s"!(end loop ${path}.$canonicalName})"))
      else
        soFar ++ List(List(s"!${path}.$canonicalName ($name)")) ++ allDiffs
    else
      soFar


sealed trait SegmentDifference extends Difference:
  val assertions: Option[(List[String], List[String])]
  val fieldDiff: List[FieldDifference]
  def subRender(path: String, soFar: List[List[String]], nestLevel: Int): List[List[String]] =
    val assertionsList = assertions match {
      case Some((s, t)) => List(">Assertions", s.mkString(","), t.mkString(","))
      case _ => Nil
    }
    val fieldDiffList = fieldDiff.foldLeft(List.empty[List[String]]) { case (acc, oneFieldDiff) => oneFieldDiff.render(s"${path}.$canonicalName", acc, nestLevel) }
    List(assertionsList) ++ fieldDiffList


sealed trait FieldDifference extends Difference


case class SingleFieldDifference(
                                  name: String,
                                  canonicalName: String,
                                  presence: Option[(Boolean,Boolean)] = None,
                                  required: Option[(Boolean,Boolean)] = None,
                                  dataType: Option[(String,String)] = None,
                                  format: Option[(Option[String], Option[String])] = None,
                                  elementId: Option[(Option[Int], Option[Int])] = None,
                                  validValues: Option[(List[String],List[String])] = None,
                                  validValuesRef: Option[(Option[String], Option[String])] = None
                                ) extends FieldDifference:
  def subRender(path: String, soFar: List[List[String]], nestLevel: Int): List[List[String]] =
    val dataTypeList = dataType match {
      case Some((s,t)) => List(">Data Type",s,t)
      case _ => Nil
    }
    val formatList = format match {
      case Some((s,t)) => List(">Format",s.getOrElse("(nothing)"),t.getOrElse("(nothing)"))
      case _ => Nil
    }
    val elementIdList = elementId match {
      case Some((s,t)) => List(">Element ID",s.map(_.toString).getOrElse("(nothing)"),t.map(_.toString).getOrElse("(nothing)"))
      case _ => Nil
    }
    val validValuesList = validValues match {
      case Some((s,t)) => List(">Valid Values",s.mkString(","),t.mkString(","))
      case _ => Nil
    }
    val validValuesRefList = validValuesRef match {
      case Some((s,t)) => List(">Valid Values Ref",s.getOrElse("(nothing)"),t.getOrElse("(nothing)"))
      case _ => Nil
    }
    List(dataTypeList, formatList, elementIdList, validValuesList, validValuesRefList)


case class CompositeFieldDifference(
                                     name: String,
                                     canonicalName: String,
                                     presence: Option[(Boolean,Boolean)] = None,
                                     required: Option[(Boolean,Boolean)] = None,
                                     fieldDiff: List[FieldDifference]
                                   ) extends FieldDifference:
  def subRender(path: String, soFar: List[List[String]], nestLevel: Int): List[List[String]] =
    fieldDiff.foldLeft(List.empty[List[String]]){ case (acc,fieldDiff) => fieldDiff.render(s"${path}.$canonicalName", acc) }


case class SimpleSegmentDifference(
                                    name: String,
                                    canonicalName: String,
                                    presence: Option[(Boolean,Boolean)] = None,
                                    required: Option[(Boolean,Boolean)] = None,
                                    assertions: Option[(List[String],List[String])] = None,
                                    fieldDiff: List[FieldDifference]
                                  ) extends SegmentDifference


case class LoopSegmentDifference(
                                  name: String,  // initially canonical name but may be renamed
                                  canonicalName: String,  // name used in the canonical spec
                                  presence: Option[(Boolean,Boolean)] = None,
                                  required: Option[(Boolean,Boolean)] = None,
                                  assertions: Option[(List[String],List[String])] = None,
                                  fieldDiff: List[FieldDifference],
                                  minDiff: Option[(Option[Int], Option[Int])] = None,
                                  maxDiff: Option[(Option[Int], Option[Int])] = None,
                                  bodyDiff: Option[List[SegmentDifference]] = None,
                                  nested: Option[List[LoopSegmentDifference]] = None
                                ) extends SegmentDifference:
  override def subRender(path: String, soFar: List[List[String]], nestLevel: Int): List[List[String]] =
    val superList = super.subRender(path, soFar, nestLevel)
    val minList = minDiff match {
      case Some((s, t)) => List(">Min Repeats", s.map(_.toString).getOrElse("(nothing)"), t.map(_.toString).getOrElse("(nothing)"))
      case _ => Nil
    }
    val maxList = maxDiff match {
      case Some((s,t)) => List(">Max Repeats",s.map(_.toString).getOrElse("(nothing)"),t.map(_.toString).getOrElse("(nothing)"))
      case _ => Nil
    }
    val loopBodyList = bodyDiff match {
      case Some(body) =>
        body.foldLeft(List.empty[List[String]]) { case (acc, oneBodyDiff) =>
          acc ++ oneBodyDiff.render(s"${path}.$canonicalName", acc)
        }
      case None => Nil
    }
    val nestedList = nested match {
      case Some(nest) => nest.foldLeft(List.empty[List[String]]){ case (acc, oneNestDiff) => oneNestDiff.render(s"!(nest level $nestLevel) ${path}.$canonicalName", acc, nestLevel+1) }
      case None => Nil
    }
    val allLoopDiff = (List(minList,maxList) ++ loopBodyList ++ nestedList).filter(_.nonEmpty)
    if allLoopDiff.isEmpty then
      if superList.isEmpty then
        soFar
      else
        soFar ++ superList
    else
      if superList.isEmpty then
        soFar ++ List(List(s"!${path}.$canonicalName ($name)")) ++ allLoopDiff ++ List(List(s"!(end loop ${path}.$canonicalName})"))
      else
        soFar ++ superList ++ List(List(s"!${path}.$canonicalName ($name)")) ++ allLoopDiff ++ List(List(s"!(end loop ${path}.$canonicalName})"))


