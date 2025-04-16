package co.blocke.edi4s
package model

import table.*
import pprint.*

sealed trait Difference:
  val name: String
  val canonicalName: String
  val presence: Option[(Boolean,Boolean)]
  val required: Option[(Boolean,Boolean)]

  def render(path: String = "", soFar: List[BodyRow] = Nil, nestLevel: Int = 0, isSubHead: Boolean = true): List[BodyRow] =
    val pathPrefix = if path.isEmpty then "" else path+"."
    val rationalName = if name == canonicalName then name else s"$canonicalName ($name)"
    val presenceList = presence match {
      case Some((true, false)) => List(
        if isSubHead then
          SubHeader(
            List(
              Cell(s"$pathPrefix$rationalName", indent = nestLevel),
              Cell("present in source <- missing in target", style = Some(Style.WARN)),
            )
          )
        else
          Row(
            List(
              Cell(s"$pathPrefix$rationalName", indent = nestLevel, style = Some(Style.TERTIARY)),
              Cell("present in source <- missing in target", style = Some(Style.WARN)),
            )
          )
      )
      case Some((false, true)) => List(
        if isSubHead then
          SubHeader(
            List(
              Cell(s"$pathPrefix$rationalName", indent = nestLevel),
              Cell("missing in source -> present in target", style = Some(Style.WARN)),
            )
          )
        else
          Row(
            List(
              Cell(s"$pathPrefix$rationalName", indent = nestLevel, style = Some(Style.TERTIARY)),
              Cell("missing in source -> present in target", style = Some(Style.WARN)),
            )
          )
      )
      case _ => Nil
    }
    val requiredList = required match {
      case Some((s, t)) => List(Row(List(Cell("Required", indent=nestLevel+1),Cell(s.toString),Cell(t.toString))))
      case _ => Nil
    }
    val allDiffs = requiredList ++ subRender(path, Nil, nestLevel)
    if presenceList.nonEmpty then
      soFar ++ presenceList
    else if allDiffs.nonEmpty then
      val newRow =
        if isSubHead then
          SubHeader(List( Cell(s"$pathPrefix$rationalName", indent = nestLevel) ) )
        else
          Row(List( Cell(s"$pathPrefix$rationalName", indent = nestLevel, style = Some(Style.TERTIARY)) ))
      soFar ++ (newRow :: allDiffs)
    else
      soFar

  def subRender(path: String, soFar: List[BodyRow], nestLevel: Int): List[BodyRow] =
    List(Row(
      List(Cell(s"$path$canonicalName ($name) -- placeholder"))
    ))


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
  override def subRender(path: String, soFar: List[BodyRow], nestLevel: Int): List[BodyRow] =
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
                                     name: String,
                                     canonicalName: String,
                                     presence: Option[(Boolean,Boolean)] = None,
                                     required: Option[(Boolean,Boolean)] = None,
                                     fieldDiff: List[FieldDifference]
                                   ) extends FieldDifference:
  override def subRender(path: String, soFar: List[BodyRow], nestLevel: Int): List[BodyRow] =
    fieldDiff.foldLeft(List.empty[BodyRow]){ case (acc,fieldDiff) => fieldDiff.render(s"${path}.$canonicalName", acc, nestLevel+1, false) }


sealed trait SegmentDifference extends Difference:
  val path: String
  val assertions: Option[(List[String], List[String])]
  val fieldDiff: List[FieldDifference]
  val pathDiff: Option[(String,String)]

  override def subRender(path: String, soFar: List[BodyRow], nestLevel: Int): List[BodyRow] =
    val assertionsList = assertions match {
      case Some((s, t)) => List(Row(List(Cell("Assertions", indent=nestLevel+1),Cell(s.mkString(",")),Cell(t.mkString(",")))))
      case _ => Nil
    }
    val pathList = pathDiff match {
      case Some((s,t)) => List(Row(List(Cell("Path", indent=nestLevel+1),Cell(s),Cell(t))))
      case _ => Nil
    }
    val pathPrefix = if path.isEmpty then "" else path+"."
    val fieldDiffList = fieldDiff.foldLeft(List.empty[BodyRow]) { case (acc, oneFieldDiff) =>
      val foo = oneFieldDiff.render(s"$pathPrefix$canonicalName", acc, nestLevel+1, false)
      if oneFieldDiff.canonicalName == "TD102" then
        println(s">>> |$path| |$pathPrefix|")
        println(pprint.log(oneFieldDiff))
      foo
    }
    assertionsList ++ pathList ++ fieldDiffList


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
  override def subRender(path: String, soFar: List[BodyRow], nestLevel: Int): List[BodyRow] =
    val superList = super.subRender(path, soFar, nestLevel)
    val pathPrefix = if path.isEmpty then "" else s"${path}."
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
          oneBodyDiff.render(s"$pathPrefix$canonicalName", acc, nestLevel+1)
        }
      case None => Nil
    }
    val nestedList = nested match {
      case Some(nest) => nest.foldLeft(List.empty[BodyRow]){ case (acc, oneNestDiff) =>
        oneNestDiff.render(s"$pathPrefix$canonicalName", acc, nestLevel+1)
      }
      case None => Nil
    }
    val allLoopDiff = minList ++ maxList ++ loopBodyList ++ nestedList
    if allLoopDiff.isEmpty then
      soFar ++ superList
    else
      soFar ++ (superList :+ Row(List(
        Cell(s"$pathPrefix$canonicalName ($name)", style=Some(Style.SECONDARY), indent = nestLevel+1)
      ))) ++ allLoopDiff
        //List(s"!${path}.$canonicalName ($name)")) ++ allLoopDiff ++ List(List(s"!(end loop ${path}.$canonicalName})"))


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
  override def subRender(path: String, soFar: List[BodyRow], nestLevel: Int): List[BodyRow] =
    val pathPrefix = if path.isEmpty then "" else s"${path}."
    if errMsg.isDefined then
      List(Row(List(
        Cell(s"$pathPrefix.$canonicalName ($name)", indent=nestLevel, style=Some(Style.SECONDARY)),
        Cell(errMsg.get, style=Some(Style.ALERT))
      )))
    else if unknownMappings.isDefined then
      buildPairedRows("Can't match", unknownMappings.get, nestLevel)
    else
      Row(List(
        Cell(s"$pathPrefix$canonicalName",indent=nestLevel,style=Some(Style.ALERT))
      )) :: targetDiff.foldLeft(List.empty[BodyRow]) { case (acc, oneBodyDiff) =>
        oneBodyDiff.render(s"${oneBodyDiff.path}.$canonicalName", acc, nestLevel+1, false)
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
                              path: String,
                              name: String,
                              canonicalName: String,
                              message: String
                            ) extends SegmentDifference:
  val presence: Option[(Boolean, Boolean)] = None
  val required: Option[(Boolean, Boolean)] = None
  val assertions: Option[(List[String], List[String])] = None
  val pathDiff: Option[(String, String)] = None
  val fieldDiff: List[FieldDifference] = Nil

  override def subRender(path: String, soFar: List[BodyRow], nestLevel: Int): List[BodyRow] =
    soFar :+ Row(List(Cell(s"{path}.$canonicalName ($name) serious error: " + message, indent = nestLevel, style = Some(Style.ALERT))))

