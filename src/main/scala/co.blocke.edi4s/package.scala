package co.blocke.edi4s

import model.*

import co.blocke.scalajack.*
given sjRefinedSpec: ScalaJack[RefinedDocumentSpec] = ScalaJack.sjCodecOf[RefinedDocumentSpec]
given sjAssignment: ScalaJack[MappingSpec] = ScalaJack.sjCodecOf[MappingSpec]
given sjEnumFields: ScalaJack[Map[String, List[String | EnumeratedDependency]]] = ScalaJack.sjCodecOf[Map[String, List[String | EnumeratedDependency]]]
