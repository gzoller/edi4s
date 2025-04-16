package co.blocke.edi4s
package table

case class HtmlRenderer(table: Table) extends Renderer:

  def render(): String =
    val sb = new StringBuilder
    sb.append("<!DOCTYPE html><html><head><meta charset=\"UTF-8\">\n")
    sb.append("<style>\n")
    sb.append("""
      table {
        border-collapse: collapse;
        width: 100%;
        font-family: Arial, sans-serif;
      }
      th, td {
        border: 1px solid #ccc;
        padding: 6px 12px;
        vertical-align: top;
      }
      th {
        background-color: #003366;
        color: white;
        text-align: center;
      }
      .primary { background-color: #cce5ff; font-weight: bold; }
      .secondary { background-color: #e2e3e5; font-weight: bold; }
      .tertiary { background-color:#d4edda; color:#155724; }
      .neutral { background-color: #ffffff; }
      .muted { background-color: #f9f9f9; color: #666; }
      .alert { background-color: #f8d7da; color: #721c24; font-weight: bold; }
      .warn { background-color: #ffeeba; color: #856404; font-weight: bold; }
      .left { text-align: left; }
      .center { text-align: center; }
      .right { text-align: right; }
      .decimal { text-align: right; } /* Decimal handled as right in HTML */
    """)
    sb.append("</style></head><body>\n")

    table.title.foreach { t =>
      sb.append(s"<h3>${t.cells.head.content}</h3>\n")
    }

    sb.append("<table>\n<thead>\n<tr>")
    table.header.cells.foreach { c =>
      val align = effectiveAlign(c, table.header.align).toString.toLowerCase
      sb.append(s"<th class='$align'>${c.content}</th>")
    }
    sb.append("</tr>\n</thead>\n<tbody>\n")

    table.rows.foreach {
      case row =>
        sb.append("<tr>")
        val rowStyle = row.style.toString.toLowerCase
        val totalCols = table.header.cells.length
        val numCells = row.cells.length

        // Promote collapse to colspan
        val cells = if numCells < totalCols then
          val collapseIdx = numCells - 1
          row.cells.updated(
            collapseIdx,
            row.cells.last.copy(colspan = totalCols - collapseIdx)
          )
        else row.cells

        cells.foreach { c =>
          val colspanAttr = if c.colspan > 1 then s" colspan='${c.colspan}'" else ""
          val indent = "&nbsp;&nbsp;" * c.indent
          val styleClass = c.style.map(_.toString.toLowerCase).getOrElse(rowStyle)
          val alignClass = effectiveAlign(c, row.align).toString.toLowerCase
          sb.append(s"<td class='$styleClass $alignClass'$colspanAttr>$indent${c.content}</td>")
        }
        sb.append("</tr>\n")
    }

    sb.append("</tbody></table>\n</body></html>")
    sb.toString

  // === Required by Renderer ===
  override def normalizedColumnWidths: List[Int] =
    val total = table.columnWidthPct.sum
    table.columnWidthPct.map(p => (p.toDouble / total * table.tableWidth).toInt)

  override def effectiveStyle(style: Style): String =
    style.toString.toLowerCase

  override def effectiveAlign(cell: Cell, rowAlign: Align): Align =
    cell.align.getOrElse(rowAlign)