package co.blocke.edi4s
package model

sealed trait Element {
  def name: String // Field name as defined in the spec
}

// Simple values
case class Simple(value: String, name: String) extends Element

case class OptionalSimple(value: Option[String], name: String) extends Element

// Composite values (components are strictly atomic)
case class Composite(components: List[AtomicElement], name: String) extends Element

case class OptionalComposite(components: Option[List[AtomicElement]], name: String) extends Element

// List of Elements (to handle repeating fields)
case class ListElement(elements: List[List[Element]], name: String) extends Element

// Marker trait for atomic components
sealed trait AtomicElement {
  def name: String // Field name as defined in the spec
}

object AtomicElement {
  case class Simple(value: String, name: String) extends AtomicElement

  case class OptionalSimple(value: Option[String], name: String) extends AtomicElement
}

/* TODO: Think more about this one!  Also think about custom loops

// This is used for partner-specific (non-standard, not-in-spec) Elements
case class CustomSegment(name: String, data: String) extends Element

// Example: A generic custom parser for any extra segments
case class CustomSegmentParser() extends SegmentParser {
  def parse(pc: ParseContext): ZIO[Any, Throwable, (List[Element], ParseContext)] = {
    // Here you would parse the custom segment and return its content
    val customSegment = CustomSegment("X12S", "extra data")
    ZIO.succeed((List(customSegment), pc))
  }
}
*/
