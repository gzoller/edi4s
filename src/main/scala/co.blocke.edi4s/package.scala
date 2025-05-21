package co.blocke.edi4s

import model.*

import co.blocke.scalajack.*
given sjRefinedSpec: ScalaJack[RefinedDocumentSpec] = ScalaJack.sjCodecOf[RefinedDocumentSpec]
