package co.blocke.edi4s
package model

case class Field(
                  name: String,
                  friendlyName: String,
                  description: String,
                  ediType: String,
                  subFields: Option[List[Field]] = None
                )

case class Segment(
                    name: String,
                    friendlyName: String,
                    description: String,
                    fields: List[Field]
                  )

case class SegmentCatalog(
                            ediVersion: String,
                            segments: List[Segment]
                          )
