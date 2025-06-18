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
  lazy val relevance: DiffRelevance =
    (presence, required) match
      // 1. Source present, target missing
      case ((true, false), _) =>
        DiffRelevance.SRC_PRESENT_TARGET_MISSING

      // 2. Source missing, target present
      case ((false, true), (_, true)) =>
        DiffRelevance.SRC_MISSING_TARGET_REQ
      case ((false, true), (_, false)) =>
        DiffRelevance.SRC_MISSING_TARGET_OPTIONAL

      // 3. Both present
      case ((true, true), (false, true)) =>
        DiffRelevance.SRC_OPT_TARGET_REQ
      case ((true, true), (false, false)) =>
        DiffRelevance.MATCH
      case ((true, true), (true, true)) =>
        DiffRelevance.MATCH
      case ((true, true), (true, false)) =>
        DiffRelevance.MATCH

      // 4. Both missing
      case ((false, false), _) =>
        DiffRelevance.TARGET_MISSING


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
                                  hlRule: Option[HLSpecRule],  // populated only for top-level HL segments
                                  hlDiscriminator: Option[String],
                                  nested: Option[LoopSegmentDifference] = None
                                ) extends SegmentDifference


// Used as a kind of exception -- halts further diff comparison
//case class DifferenceError(
//                              path: Path,
//                              message: String
//                            ) extends SegmentDifference:
//  val name: String = ""
//  val canonicalName: String = ""
//  val presence: (Boolean, Boolean) = (true,true)
//  val required: (Boolean, Boolean) = (true,true)
//  val assertions: Option[(List[String], List[String])] = None
//  val pathDiff: Option[(String, String)] = None
//  val fieldDiff: List[FieldDifference] = Nil


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

enum DiffRelevance:
  case SRC_PRESENT_TARGET_MISSING // presence (true,false)
  case TARGET_MISSING   // presence = (_,false) <-- can skip
  case SRC_MISSING_TARGET_OPTIONAL // presence = (false,true), required = (_,false) // <-- can skip
  case SRC_MISSING_TARGET_REQ // presence (false,true) req (_,true)
  case SRC_OPT_TARGET_REQ // presence (true,true), req (false,true)
  case MATCH // presence (true,true), req (true,true) || (_,false)

/*
  presence  required
   t  t       t   t
   t  t       t   f
   t  t       f   t
   t  t       f   f

   t  f       t   t
   t  f       t   f
   t  f       f   t
   t  f       f   f

   f  t       t   t
   f  t       t   f
   f  t       f   t
   f  t       f   f

   f  f       t   t
   f  f       t   f
   f  f       f   t
   f  f       f   f

      seg match {

        // SRC_PRESENT_TARGET_MISSING
        case _ if seg.presence == (true,false) =>

        // Skip ST/SE segments b/c we handle them as first-class parsed objects rather than general X12 segments
        // TARGET_MISSING || SRC_MISSING_TARGET_OPTIONAL
        case _ if seg.presence == (false,false) || (!seg.presence._1 && !seg.required._2) || seg.canonicalName == "ST" || seg.canonicalName == "SE" =>

        // SRC_MISSING_TARGET_REQ
        case s: SimpleSegmentDifference if seg.presence == (false,true) && seg.required._2 =>  // missing from src, required in target

        // SRC_MISSING_TARGET_REQ
        case s: LoopSegmentDifference if seg.presence == (false,true) && seg.required._2 =>  // missing from src, required in target

        // Target present, src/target both required -or- src required/target optional -or- src/target optional
        // MATCH
        --A-- case s: SimpleSegmentDifference if s.required._1 || s.required == (false,false) =>

        // Target present, src optional/target required
        // SRC_OPT_TARGET_REQ
        --B-- case s: SimpleSegmentDifference if s.required == (false, true) =>

        // MATCH
        --A-- case s: LoopSegmentDifference if s.required._1 || s.required == (false,false) =>

        // SRC_OPT_TARGET_REQ
        --B-- case s: LoopSegmentDifference if s.required == (false, true) =>

        // Cases we don't care about: e.g. not present in target -- should never happen
        case _ =>
      }
  */