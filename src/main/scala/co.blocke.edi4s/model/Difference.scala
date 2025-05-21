package co.blocke.edi4s
package model

import table.*
import pprint.*


sealed trait Difference:
  val path: Path
  val name: String
  val canonicalName: String
  val presence: (Boolean,Boolean)
  val required: (Boolean,Boolean)
  def isOk: Boolean = presence._1 == presence._2 && (required._1 == required._2 || required._1)


sealed trait FieldDifference extends Difference


case class SingleFieldDifference(
                                  path: Path,
                                  name: String,
                                  canonicalName: String,
                                  presence: (Boolean,Boolean),
                                  required: (Boolean,Boolean),
                                  dataType: Option[(String,String)] = None,
                                  format: Option[(Option[String], Option[String])] = None,
                                  elementId: Option[(Option[Int], Option[Int])] = None,
                                  validValues: Option[(List[String],List[String])] = None,
                                  validValuesRef: Option[(Option[String], Option[String])] = None
                                ) extends FieldDifference


case class CompositeFieldDifference(
                                     path: Path,
                                     name: String,
                                     canonicalName: String,
                                     presence: (Boolean,Boolean),
                                     required: (Boolean,Boolean),
                                     fieldDiff: List[FieldDifference]
                                   ) extends FieldDifference


sealed trait SegmentDifference extends Difference:
  val assertions: Option[(List[String], List[String])]
  val fieldDiff: List[FieldDifference]


case class SimpleSegmentDifference(
                                    path: Path,
                                    name: String,
                                    canonicalName: String,
                                    presence: (Boolean,Boolean),
                                    required: (Boolean,Boolean),
                                    assertions: Option[(List[String],List[String])] = None,
                                    fieldDiff: List[FieldDifference]
                                  ) extends SegmentDifference


case class LoopSegmentDifference(
                                  path: Path,
                                  name: String,  // initially canonical name but may be renamed
                                  canonicalName: String,  // name used in the canonical spec
                                  presence: (Boolean,Boolean),
                                  required: (Boolean,Boolean),
                                  assertions: Option[(List[String],List[String])] = None,
                                  fieldDiff: List[FieldDifference],
                                  minDiff: Option[(Option[Int], Option[Int])] = None,
                                  maxDiff: Option[(Option[Int], Option[Int])] = None,
                                  bodyDiff: List[SegmentDifference],
                                  nested: Option[List[LoopSegmentDifference]] = None
                                ) extends SegmentDifference


// Used as a kind of exception -- halts further diff comparison
case class DifferenceError(
                              path: Path,
                              message: String
                            ) extends SegmentDifference:
  val name: String = ""
  val canonicalName: String = ""
  val presence: (Boolean, Boolean) = (true,true)
  val required: (Boolean, Boolean) = (true,true)
  val assertions: Option[(List[String], List[String])] = None
  val pathDiff: Option[(String, String)] = None
  val fieldDiff: List[FieldDifference] = Nil


case class FieldDifferenceError(
                            path: Path,
                            message: String
                          ) extends FieldDifference:
  val name: String = ""
  val canonicalName: String = ""
  val presence: (Boolean, Boolean) = (true,true)
  val required: (Boolean, Boolean) = (true,true)
  val assertions: Option[(List[String], List[String])] = None
  val pathDiff: Option[(String, String)] = None
  val fieldDiff: List[FieldDifference] = Nil

