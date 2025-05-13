package co.blocke.edi4s
package model

// NOTE: For parsed EDI X12 data the Elements will have "" for name. There is none in the data.
//    This must be inferred by x-referencing Elements with the EDI spec.
//    For parsed XML/JSON the Elements will have a populated name

sealed trait X12Token:
  val name: String

// Simple values
case class SimpleX12Token(name: String, value: String) extends X12Token
case class EmptyX12Token(name: String) extends X12Token
case class RepeatedX12Token(name: String, value: List[String]) extends X12Token

// Composite values (components are strictly atomic)
case class CompositeX12Token(name: String, value: List[X12Token]) extends X12Token

case class SegmentX12Token(name: String, fields: List[X12Token])

case class X12TokenDocument( segments: List[SegmentX12Token] )

case class IsaSegment(
                       authorizationQualifier: String = "00",    // ISA01 - e.g., "00"
                       authorizationInfo: String = "          ", // ISA02 - 10 spaces if unused
                       securityQualifier: String = "00",         // ISA03 - e.g., "00"
                       securityInfo: String = "          ",      // ISA04 - 10 spaces if unused
                       senderQualifier: String = "ZZ",           // ISA05 - e.g., "ZZ"
                       senderId: String,                         // ISA06 - 15 chars, space-padded
                       receiverQualifier: String = "ZZ",         // ISA07 - e.g., "ZZ"
                       receiverId: String,                       // ISA08 - 15 chars, space-padded
                       interchangeDate: String,                  // ISA09 - YYMMDD
                       interchangeTime: String,                  // ISA10 - HHMM
                       repetitionSeparator: Char = '^',          // ISA11 - e.g., '^'
                       version: String,                          // ISA12 - e.g., "00401"
                       controlNumber: String,                    // ISA13 - 9-digit string (e.g., "000000001")
                       ackRequested: Int = 0,                    // ISA14 - "0" or "1"
                       usageIndicator: Char = 'T',               // ISA15 - "T" (test) or "P" (production)
                       componentSeparator: Char = ':',           // ISA16 - e.g., ':'
                       groupSets: List[GsSegment]                // the GS block under this ISA
                     )

case class GsSegment(
                      functionalIdCode: String,              // GS01 - e.g., "SH" for 856
                      applicationSenderCode: String,         // GS02 - usually matches ISA06
                      applicationReceiverCode: String,       // GS03 - usually matches ISA08
                      date: String,                          // GS04 - YYYYMMDD
                      time: String,                          // GS05 - HHMM
                      groupControlNumber: String,            // GS06 - must match GE02
                      responsibleAgencyCode: Char = 'X',     // GS07 - usually "X"
                      versionReleaseIndustryCode: String,    // GS08 - e.g., "004010"
                      transactions: List[StSegment]          // the body of this GS
                    )

case class StSegment(
                      transactionSetIdCode: String,          // ST01 - e.g., "850", "856", "810"
                      transactionSetControlNumber: String,   // ST02 - e.g., "0001"; must match SE02
                      body: List[SegmentX12Token]
                    )

case class X12Transaction( isa: IsaSegment )