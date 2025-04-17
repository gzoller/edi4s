package co.blocke.edi4s
package model

import table.*
import pprint.*


// Paths do NOT contain the terminal (ie segment/field name). Just the path to get there.
case class Path(value: String = ""):
  def prefix: Path =
    value.lastIndexOf('.') match
      case -1 => this
      case i => Path(value.substring(0, i))
  override def toString: String = value
  def toStringWith(terminal: String): String =
    if value == "" then terminal else s"${value}.${terminal}"
  def dot(level: String): Path =
    if value == "" then Path(level) else Path(s"${value}.${level}")
  def nest(level: String): Path =
    if value == "" then
      Path(level)
    else
      Path(s"${value}>${level}")


sealed trait Difference:
  val path: Path
  val name: String
  val canonicalName: String
  val presence: Option[(Boolean,Boolean)]
  val required: Option[(Boolean,Boolean)]

  def render(soFar: List[BodyRow] = Nil, nestLevel: Int = 0, isSubHead: Boolean = true): List[BodyRow] =
    val rationalName = if name == canonicalName then name else s"$canonicalName ($name)"
    val presenceList = presence match {
      case Some((true, false)) => List(
        if isSubHead then
          SubHeader(
            List(
              Cell(path.toStringWith(rationalName), indent = nestLevel),
              Cell("present", style = Some(Style.NEUTRAL)),
              Cell("missing", style = Some(Style.WARN))
            )
          )
        else
          Row(
            List(
              Cell(path.toStringWith(rationalName), indent = nestLevel, style = Some(Style.TERTIARY)),
              Cell("present", style = Some(Style.NEUTRAL)),
              Cell("missing", style = Some(Style.WARN))
            )
          )
      )
      case Some((false, true)) => List(
        if isSubHead then
          SubHeader(
            List(
              Cell(path.toStringWith(rationalName), indent = nestLevel),
              Cell("missing", style = Some(Style.WARN)),
              Cell("present", style = Some(Style.NEUTRAL))
            )
          )
        else
          Row(
            List(
              Cell(path.toStringWith(rationalName), indent = nestLevel, style = Some(Style.TERTIARY)),
              Cell("missing", style = Some(Style.WARN)),
              Cell("present", style = Some(Style.NEUTRAL))
            )
          )
      )
      case _ => Nil
    }
    val requiredList = required match {
      case Some((s, t)) => List(Row(List(Cell("Required", indent=nestLevel+1),Cell(s.toString),Cell(t.toString))))
      case _ => Nil
    }
    val allDiffs = requiredList ++ subRender(Nil, nestLevel)
    if presenceList.nonEmpty then
      soFar ++ presenceList
    else if allDiffs.nonEmpty then
      val newRow =
        if isSubHead then
          SubHeader(List( Cell(path.toStringWith(rationalName), indent = nestLevel) ) )
        else
          Row(List( Cell(path.toStringWith(rationalName), indent = nestLevel, style = Some(Style.TERTIARY)) ))
      soFar ++ (newRow :: allDiffs)
    else
      soFar

  def subRender(soFar: List[BodyRow], nestLevel: Int): List[BodyRow]


sealed trait FieldDifference extends Difference


case class SingleFieldDifference(
                                  path: Path,
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
  override def subRender(soFar: List[BodyRow], nestLevel: Int): List[BodyRow] =
    val dataTypeList = dataType match {
      case Some((s, t)) => List(Row(List(Cell("Data Type", indent=nestLevel+1),Cell(s),Cell(t))))
      case _ => Nil
    }
    val formatList = format match {
      case Some((s, t)) => List(Row(List(Cell("Format", indent=nestLevel+1),Cell(s.getOrElse("(nothing)")),Cell(t.getOrElse("(nothing)")))))
      case _ => Nil
    }
    val elementIdList = elementId match {
      case Some((s, t)) => List(Row(List(Cell("Element ID", indent=nestLevel+1),Cell(s.map(_.toString).getOrElse("(nothing)")),Cell(t.map(_.toString).getOrElse("(nothing)")))))
      case _ => Nil
    }
    val validValuesList = validValues match {
      case Some((s, t)) => List(Row(List(Cell("Valid Values", indent=nestLevel+1),Cell(s.mkString(",")),Cell(t.mkString(",")))))
      case _ => Nil
    }
    val validValuesRefList = validValuesRef match {
      case Some((s, t)) => List(Row(List(Cell("Valid Values Ref", indent=nestLevel+1),Cell(s.getOrElse("(nothing)")),Cell(t.getOrElse("(nothing)")))))
      case _ => Nil
    }
    dataTypeList ++ formatList ++ elementIdList ++ validValuesList ++ validValuesRefList


case class CompositeFieldDifference(
                                     path: Path,
                                     name: String,
                                     canonicalName: String,
                                     presence: Option[(Boolean,Boolean)] = None,
                                     required: Option[(Boolean,Boolean)] = None,
                                     fieldDiff: List[FieldDifference]
                                   ) extends FieldDifference:
  override def subRender(soFar: List[BodyRow], nestLevel: Int): List[BodyRow] =
    // TODO: Fix this! Proper thing to do here is *NOT* call render but to create a new Row for the composite field, at appropriate
    // nest level, then rows for each composite part (subfield)
    fieldDiff.foldLeft(List.empty[BodyRow]){ case (acc,fieldDiff) => fieldDiff.render(acc, nestLevel+1, false) }


sealed trait SegmentDifference extends Difference:
  val assertions: Option[(List[String], List[String])]
  val fieldDiff: List[FieldDifference]
  val pathDiff: Option[(String,String)]

  override def subRender(soFar: List[BodyRow], nestLevel: Int): List[BodyRow] =
    val assertionsList = assertions match {
      case Some((s, t)) => List(Row(List(Cell("Assertions", indent=nestLevel+1),Cell(s.mkString(",")),Cell(t.mkString(",")))))
      case _ => Nil
    }
    val pathList = pathDiff match {
      case Some((s,t)) => List(Row(List(Cell("Path", indent=nestLevel+1),Cell(s),Cell(t))))
      case _ => Nil
    }
    val fieldDiffList = fieldDiff.foldLeft(List.empty[BodyRow]) { case (acc, oneFieldDiff) =>
      oneFieldDiff.render(acc, nestLevel+1, false)
//      if oneFieldDiff.canonicalName == "TD102" then
//        println(s">>> |$path| |$pathPrefix|")
//        println(pprint.log(oneFieldDiff))
//      foo
    }
    assertionsList ++ pathList ++ fieldDiffList


case class SimpleSegmentDifference(
                                    path: Path,
                                    name: String,
                                    canonicalName: String,
                                    presence: Option[(Boolean,Boolean)] = None,
                                    required: Option[(Boolean,Boolean)] = None,
                                    assertions: Option[(List[String],List[String])] = None,
                                    pathDiff: Option[(String,String)],
                                    fieldDiff: List[FieldDifference]
                                  ) extends SegmentDifference


case class LoopSegmentDifference(
                                  path: Path,
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
  override def subRender(soFar: List[BodyRow], nestLevel: Int): List[BodyRow] =
    val superList = super.subRender(soFar, nestLevel)
    val minList = minDiff match {
      case Some((s,t)) => List(Row(List(Cell("Min Repeats", indent=nestLevel+1),Cell(s.map(_.toString).getOrElse("(nothing)")),Cell(t.map(_.toString).getOrElse("(nothing)")))))
      case _ => Nil
    }
    val maxList = maxDiff match {
      case Some((s,t)) => List(Row(List(Cell("Max Repeats", indent=nestLevel+1),Cell(s.map(_.toString).getOrElse("(nothing)")),Cell(t.map(_.toString).getOrElse("(nothing)")))))
      case _ => Nil
    }
    val loopBodyList = bodyDiff match {
      case Some(body) =>
        body.foldLeft(List.empty[BodyRow]) { case (acc, oneBodyDiff) =>
          oneBodyDiff.render(acc, nestLevel+1)
        }
      case None => Nil
    }
    val nestedList = nested match {
      case Some(nest) => nest.foldLeft(List.empty[BodyRow]){ case (acc, oneNestDiff) =>
        // TODO: Fix! Maybe oneNestDiff.path?
        oneNestDiff.render(acc, nestLevel+1)
      }
      case None => Nil
    }
    val allLoopDiff = minList ++ maxList ++ loopBodyList ++ nestedList
    if allLoopDiff.isEmpty then
      soFar ++ superList
    else
      soFar ++ (superList :+ Row(List(
        Cell(path.toStringWith(canonicalName+s" ($name)"), style=Some(Style.SECONDARY), indent = nestLevel+1)
      ))) ++ allLoopDiff
        //List(s"!${path}.$canonicalName ($name)")) ++ allLoopDiff ++ List(List(s"!(end loop ${path}.$canonicalName})"))


case class HLDifference(
                       path: Path,
                       name: String,
                       canonicalName: String,
                       presence: Option[(Boolean,Boolean)] = None,
                       pathDiff: Option[(String,String)] = None,  // path moved src->target, presumed same element
                       targetDiff: List[SegmentDifference],
                       errMsg: Option[String] = None,
                       unknownMappings: Option[(List[String],List[String])] = None  // srcPaths,targetPaths
                       ) extends Difference:
  val required: Option[(Boolean,Boolean)] = None
  override def subRender(soFar: List[BodyRow], nestLevel: Int): List[BodyRow] =
    val pathItem = pathDiff.map { case (s, t) =>
      Row(List(
        Cell("Element Path Difference", indent = nestLevel, style = Some(Style.TERTIARY)),
        Cell(s, style = Some(Style.TERTIARY)),
        Cell(t, style = Some(Style.TERTIARY)),
      ))
    }
    if errMsg.isDefined then
      List(Row(List(
        Cell(path.toStringWith(canonicalName+s" ($name)"), indent=nestLevel, style=Some(Style.SECONDARY)),
        Cell(errMsg.get, style=Some(Style.ALERT))
      )))
    else if unknownMappings.isDefined then
      buildPairedRows(s"Can't match for element ${canonicalName}", unknownMappings.get, nestLevel)
    else
      pathItem.toList ++
        List(Row(List(
          Cell(path.toStringWith(canonicalName),indent=nestLevel,style=Some(Style.SECONDARY))
        )) ) ++
        targetDiff.foldLeft(List.empty[BodyRow]) { case (acc, oneBodyDiff) =>
          // TODO: Fix path here
          oneBodyDiff.render(acc, nestLevel+1, false)
        }

  private def buildPairedRows(label: String, data: (List[String], List[String]), nestLevel: Int): List[Row] =
    val (leftList, rightList) = data
    val maxRows = math.max(leftList.length, rightList.length)

    (0 until maxRows).toList.map { i =>
      val labelCell =
        if i == 0 then Cell(label, indent=nestLevel)
        else Cell("", colspan = 1)

      val leftCell =
        if i < leftList.length then Cell(leftList(i))
        else Cell("")

      val rightCell =
        if i < rightList.length then Cell(rightList(i))
        else Cell("")

      Row(List(labelCell, leftCell, rightCell))
    }


case class SeriousDifference(
                              path: Path,
                              name: String,
                              canonicalName: String,
                              message: String
                            ) extends SegmentDifference:
  val presence: Option[(Boolean, Boolean)] = None
  val required: Option[(Boolean, Boolean)] = None
  val assertions: Option[(List[String], List[String])] = None
  val pathDiff: Option[(String, String)] = None
  val fieldDiff: List[FieldDifference] = Nil

  override def subRender(soFar: List[BodyRow], nestLevel: Int): List[BodyRow] =
    soFar :+ Row(List(Cell(path.toStringWith(canonicalName+s" ($name) serious error: " + message), indent = nestLevel, style = Some(Style.ALERT))))

