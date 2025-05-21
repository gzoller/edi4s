package co.blocke.edi4s
package diff

import model.*
import table.*


object DiffReport:

  def asDiff(
               src: RefinedDocumentSpec,
               edi: RefinedDocumentSpec,
               target: RefinedDocumentSpec
             ): List[SegmentDifference] =
    // Initial diff
    val result_pre = DiffEngine.compareSpecs(src, edi, target)

    // See if there's a missing HL level that can be cloned (eg missing Tare but Pack is optionally T or P).
    // If so, clone the next level (eg Pack) and "promote" the clone to the missing level (Tare).
    // If one was missing/cloned, then re-bake the diff
    DiffEngine.detectAndPatchMissingHL(src, result_pre, target).map { maybeChangedTarget =>
      DiffEngine.compareSpecs(src, edi, maybeChangedTarget)
    }.getOrElse(result_pre)


  def asTable(
            srcPartner: String,
            targetPartner: String,
            src: RefinedDocumentSpec,
            edi: RefinedDocumentSpec,
            target: RefinedDocumentSpec,
            filterUnused: Boolean = true
          ): Table =
    val result = asDiff( src, edi, target )

    val titles = List(
      Title(List(Cell("📦 EDI Segment Comparison Report"))),
      Title(List(Cell(s"$srcPartner -to- $targetPartner")))
    )
    val header = Header(List(
      Cell(s"Source ($srcPartner)"),
      Cell("Difference"),
      Cell(s"Target ($targetPartner)"),
      Cell("Difference")
    ))
    val rawRows = result.foldLeft(List.empty[BodyRow]) { case (acc, diff) => acc ++ diff.render() }
    val rows = if filterUnused then
      rawRows.filter { row =>
        // keep rows that have at least one cell *not* muted
        row.cells.exists(cell => !cell.style.contains(Style.MUTED))
      }
    else rawRows
    Table(
      title = titles,
      columns = 4,
      columnWidthPct = List(35, 15, 35, 15),
      tableWidth = 200,
      header,
      rows
    )