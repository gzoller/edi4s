package co.blocke.edi4s
package diff

import zio.*
import model.*
import table.*


object DiffReport:

  def asTable(
            srcPartner: String,
            targetPartner: String,
            diffs: List[SegmentDifference],
            filterUnused: Boolean = true
          ): Table =
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
    val rawRows = diffs.foldLeft(List.empty[BodyRow]) { case (acc, diff) => acc ++ diff.render() }
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