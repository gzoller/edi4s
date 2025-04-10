package co.blocke.edi4s
package model

// NOTE: For parsed EDI X12 data the Elements will have "" for name. There is none in the data.
//    This must be inferred by x-referencing Elements with the EDI spec.
//    For parsed XML/JSON the Elements will have a populated name

sealed trait X12Token

// Simple values
case class SimpleX12Token(name: String, value: String) extends X12Token
case class EmptyX12Token(name: String) extends X12Token

// Composite values (components are strictly atomic)
case class CompositeX12Token(name: String, value: List[X12Token]) extends X12Token

case class SegmentX12Token(name: String, value: List[X12Token])

case class X12TokenDocument( segments: List[SegmentX12Token] )