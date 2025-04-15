package co.blocke.edi4s.model


sealed trait Difference:
  val name: String
  val canonicalName: String
  val presence: Option[(Boolean,Boolean)]
  val required: Option[(Boolean,Boolean)]

  def subRender(path: String, soFar: List[List[String]], nestLevel: Int): List[List[String]]
  def render(path: String, soFar: List[List[String]], nestLevel: Int = -1): List[List[String]] =
    val pathPrefix = if path.isEmpty then "" else path+"."
    val presenceList = presence match {
      case Some((true, false)) => List(s"!$pathPrefix$canonicalName ($name)", "~present in source - missing in target")
      case Some((false, true)) => List(s"!$pathPrefix$canonicalName ($name)", "~missing in source - present in target")
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
        soFar ++ List(List(s"!(nest level $nestLevel) $pathPrefix$canonicalName ($name)")) ++ allDiffs ++ List(List(s"!(end loop $pathPrefix$canonicalName})"))
      else
        soFar ++ List(List(s"!x$pathPrefix$canonicalName ($name)","","")) ++ allDiffs
    else
      soFar


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


sealed trait SegmentDifference extends Difference:
  val assertions: Option[(List[String], List[String])]
  val fieldDiff: List[FieldDifference]
  val pathDiff: Option[(String,String)]

  def subRender(path: String, soFar: List[List[String]], nestLevel: Int): List[List[String]] =
    val assertionsList = assertions match {
      case Some((s, t)) => List(">Assertions", s.mkString(","), t.mkString(","))
      case _ => Nil
    }
    val pathList = pathDiff match {
      case Some((a,b)) => List("> Path", a, b)
      case _ => Nil
    }
    val pathPrefix = if path.isEmpty then "" else path+"."
    val fieldDiffList = fieldDiff.foldLeft(List.empty[List[String]]) { case (acc, oneFieldDiff) => oneFieldDiff.render(s"$pathPrefix$canonicalName", acc, nestLevel) }
    List(assertionsList) ++ List(pathList) ++ fieldDiffList


case class SimpleSegmentDifference(
                                    path: String,
                                    name: String,
                                    canonicalName: String,
                                    presence: Option[(Boolean,Boolean)] = None,
                                    required: Option[(Boolean,Boolean)] = None,
                                    assertions: Option[(List[String],List[String])] = None,
                                    pathDiff: Option[(String,String)],
                                    fieldDiff: List[FieldDifference]
                                  ) extends SegmentDifference


case class LoopSegmentDifference(
                                  path: String,
                                  name: String,  // initially canonical name but may be renamed
                                  canonicalName: String,  // name used in the canonical spec
                                  presence: Option[(Boolean,Boolean)] = None,
                                  required: Option[(Boolean,Boolean)] = None,
                                  assertions: Option[(List[String],List[String])] = None,
                                  pathDiff: Option[(String,String)] = None,
                                  fieldDiff: List[FieldDifference],
                                  minDiff: Option[(Option[Int], Option[Int])] = None,
                                  maxDiff: Option[(Option[Int], Option[Int])] = None,
                                  bodyDiff: Option[List[SegmentDifference]] = None,
                                  nested: Option[List[HLDifference]] = None
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


case class SeriousDifference(
                                    path: String,
                                    name: String,
                                    canonicalName: String,
                                    message: String
                          ) extends SegmentDifference:
  val presence: Option[(Boolean,Boolean)] = None
  val required: Option[(Boolean,Boolean)] = None
  val assertions: Option[(List[String], List[String])] = None
  val pathDiff: Option[(String, String)] = None
  val fieldDiff: List[FieldDifference] = Nil
  override def subRender(path: String, soFar: List[List[String]], nestLevel: Int): List[List[String]] =
    soFar ++ List(List(s"!{path}.$canonicalName ($name) serious error: "+message))


case class HLDifference(
                       path: String,
                       name: String,
                       canonicalName: String,
                       presence: Option[(Boolean,Boolean)] = None,
                       targetDiff: List[SegmentDifference],
                       errMsg: Option[String] = None,
                       unknownMappings: Option[(List[String],List[String])] = None  // srcPaths,targetPaths
                       ) extends Difference:
  val required: Option[(Boolean,Boolean)] = None
  override def subRender(path: String, soFar: List[List[String]], nestLevel: Int): List[List[String]] = Nil
  override def render(path: String, soFar: List[List[String]], nestLevel: Int = -1): List[List[String]] =
    Nil