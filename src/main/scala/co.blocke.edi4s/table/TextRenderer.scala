package co.blocke.edi4s
package table

case class TextRenderer(table: Table) extends Renderer:
  private val RESET = "\u001b[0m"

  def render(): String =
    val sb = new StringBuilder
    val widths = normalizedColumnWidths
    val decimalAlignedCellOffsets = findDecimalAlignedCells

    table.title.foreach(t => sb.append(renderTitle(t, widths)))
    sb.append(renderDivider(widths, '='))
    sb.append(renderRow(table.header, widths, decimalAlignedCellOffsets))
    sb.append(renderDivider(widths, '='))

    val rowCount = table.rows.length
    table.rows.zipWithIndex.map{
      case (r: SubHeader,i) =>
        if i > 0 && !isSubHeader(table.rows(i-1)) then
          sb.append(renderDivider(widths))
        sb.append(renderRow(r, widths, decimalAlignedCellOffsets))
        sb.append(renderDivider(widths))
      case (r: Row,i) =>
        sb.append(renderRow(r, widths, decimalAlignedCellOffsets))
        if i == table.rows.length-1 then
          sb.append(renderDivider(widths))
    }
    sb.toString

  private def isSubHeader(r: BodyRow): Boolean =
    r match {
      case _: SubHeader => true
      case _ => false
    }

  private def findDecimalAlignedCells: List[(Int, Int)] =
    val allRows = table.header +: table.rows
    val columnCount = table.columnWidthPct.length

    (0 until columnCount).flatMap { colIdx =>
      var maxRightOfDecimal = 0
      allRows.foreach { row =>
        if colIdx < row.cells.length then
          val cell = row.cells(colIdx)
          val alignment = effectiveAlign(cell, row.align)
          if alignment == Align.DECIMAL then
            val content = cell.content.trim
            val parts = content.split("\\.")
            if parts.length == 2 then
              val rightDigits = parts(1).length
              maxRightOfDecimal = math.max(maxRightOfDecimal, rightDigits)
      }
      if maxRightOfDecimal > 0 then
        Some(colIdx -> maxRightOfDecimal)
      else None
    }.toList

  def normalizedColumnWidths: List[Int] =
    val totalPct = table.columnWidthPct.sum
    table.columnWidthPct.map(p => (p.toDouble / totalPct * table.tableWidth).toInt)

  def effectiveStyle(style: Style): String =
    table.styleMap.getOrElse("text", Map()).getOrElse(style, "")

  def effectiveAlign(cell: Cell, rowAlign: Align): Align =
    cell.align.getOrElse(rowAlign)

  private def renderTitle(title: Title, widths: List[Int]): String =
    val totalWidth = widths.sum + widths.length + 1
    val content = title.cells.head.content
    val pad = (totalWidth - content.length) / 2
    " " * pad + content + "\n"

  private def renderDivider(widths: List[Int], char: Char = '-'): String =
    widths.map(w => "+" + char.toString * w).mkString + "+\n"

  private def padDecimal(content: String, width: Int, rightOffset: Int): String =
    val stripped = content.trim
    val (intPart, fracPart) = stripped.split("\\.") match
      case Array(intP, fracP) => (intP, fracP)
      case Array(intP) => (intP, "")

    val contentWithDot = if fracPart.nonEmpty then s"$intPart.$fracPart" else intPart

    val truncated =
      if contentWithDot.length > width then
        val cutoff = width - 3
        if cutoff <= 0 then "..." else contentWithDot.take(cutoff) + "..."
      else contentWithDot

    val totalLength = truncated.length
    val leftPad = width - math.max(totalLength, rightOffset + intPart.length + (if fracPart.nonEmpty then 1 else 0))
    val rightPad = width - (leftPad + totalLength)

    " " * leftPad + truncated + " " * rightPad

  private def pad(content: String, width: Int, align: Align): String =
    val truncated = if content.length > width then content.take(width - 3) + "..." else content

    align match
      case Align.LEFT =>
        truncated.padTo(width, ' ')
      case Align.CENTER =>
        val padLeft = (width - truncated.length) / 2
        val padRight = width - truncated.length - padLeft
        " " * padLeft + truncated + " " * padRight
      case Align.RIGHT =>
        " " * (width - truncated.length) + truncated
      case Align.DECIMAL =>
        pad(truncated, width, Align.RIGHT) // fallback if no decimal info is known

  private def renderRow(row: TableRow, widths: List[Int], decimalAlignOffsets: List[(Int, Int)]): String =
    val sb = new StringBuilder
    sb.append("|")

    val totalCols = widths.length
    val cellCount = row.cells.length

    var colIndex = 0
    val isCollapsed = cellCount < totalCols

    row.cells.zipWithIndex.foreach { case (cell, i) =>
      val isLastCollapsed = isCollapsed && i == cellCount - 1
      val span = if isLastCollapsed then totalCols - colIndex else math.max(cell.colspan, 1)

      val targetWidth = widths.slice(colIndex, colIndex + span).sum + (span - 1) // account for dividers
      val styleStr = cell.style.map(effectiveStyle).getOrElse(effectiveStyle(row.style))
      val align = effectiveAlign(cell, row.align)
      val content = " " * (cell.indent * 2) + cell.content

      val padStr =
        if align == Align.DECIMAL && decimalAlignOffsets.exists(_._1 == colIndex) then
          val offset = decimalAlignOffsets.find(_._1 == colIndex).get._2
          padDecimal(content, targetWidth, offset)
        else
          pad(content, targetWidth, align)

      sb.append(styleStr).append(padStr).append(RESET).append("|")
      colIndex += span
    }

    sb.append("\n")
    sb.toString