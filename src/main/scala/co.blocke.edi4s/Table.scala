package co.blocke.edi4s

import java.io.FileOutputStream
import org.apache.poi.ss.usermodel._
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.apache.poi.ss.util.CellRangeAddress
import org.apache.poi.ss.usermodel.IndexedColors

case class Table(
                  title: String,
                  headers: List[String],
                  columns: List[Int],
                  rows: List[List[String]],
                  format: String = "text" // "text", "color_text", "markdown", "html", "xls"
                ) {

  private val margin = 1
  private val truncationMarker = "..."
  private val maxColumnWidthChars = 50 // reasonable max

  private val RESET = "\u001b[0m"
  private val RED = "\u001b[31m"
  private val BOLD = "\u001b[1m"
  private val BLUE = "\u001b[94m"

  private def stripAnsi(str: String): String =
    str.replaceAll("\u001B\\[[;\\d]*m", "")

  private def truncate(content: String, width: Int): String = {
    val visible = stripAnsi(content)
    if (visible.length <= width) content
    else visible.take(width - truncationMarker.length) + truncationMarker
  }

  private def pad(content: String, width: Int): String = {
    val visible = stripAnsi(content)
    val maxContentWidth = width - 2 * margin
    val truncated = truncate(content, maxContentWidth)
    val visibleTruncated = stripAnsi(truncated)
    val paddingNeeded = width - visibleTruncated.length
    val leftPad = margin
    val rightPad = paddingNeeded - margin
    s"${" " * leftPad}$truncated${" " * rightPad}"
  }

  private def renderDivider(char: Char = '-'): String =
    columns.map(w => "+" + char.toString * w).mkString("") + "+"

  private def renderRow(cells: List[String], useColor: Boolean = false, headers: Boolean = false): String = {
    val sb = new StringBuilder
    var colIdx = 0
    var inRow = true
    val isCollapsedRow = cells.exists(_.startsWith("!"))

    while (colIdx < cells.length && inRow) {
      val raw = cells(colIdx)
      if (raw.startsWith("!")) {
        val collapsedWidth = columns.drop(colIdx).sum + (columns.length - colIdx - 1)
        val content = raw.stripPrefix("!")
        val color = if (useColor) BLUE else ""
        sb.append("|").append(pad(s"$color$content$RESET", collapsedWidth)).append("|")
        inRow = false
      } else {
        val indentLevel = raw.takeWhile(_ == '>').length
        val content = raw.drop(indentLevel)
        val indented = ("  " * indentLevel) + content
        val color =
          if (useColor && headers) BOLD + RED
          else if (useColor && isCollapsedRow) BLUE
          else ""
        val colored = if (useColor) s"$color$indented$RESET" else indented
        sb.append("|").append(pad(colored, columns(colIdx)))
        colIdx += 1
      }
    }

    while (colIdx < columns.length && inRow) {
      sb.append("|").append(pad("", columns(colIdx)))
      colIdx += 1
    }

    if (inRow) sb.append("|")
    sb.toString()
  }

  def writeXls(path: String): Unit = {
    val wb: Workbook = new XSSFWorkbook()
    val sheet = wb.createSheet("Sheet1")
    var rowIdx = 0

    def baseStyle(fg: Short = IndexedColors.WHITE.getIndex(), bold: Boolean = false): CellStyle = {
      val style = wb.createCellStyle()
      val font = wb.createFont()
      font.setFontHeightInPoints(12)
      font.setBold(bold)
      font.setColor(if (fg == IndexedColors.DARK_BLUE.getIndex()) IndexedColors.WHITE.getIndex() else IndexedColors.BLACK.getIndex())
      style.setFont(font)
      style.setWrapText(true)
      style.setVerticalAlignment(VerticalAlignment.CENTER)
      style.setFillForegroundColor(fg)
      style.setFillPattern(FillPatternType.SOLID_FOREGROUND)
      style.setBorderBottom(BorderStyle.THIN)
      style.setBorderTop(BorderStyle.THIN)
      style.setBorderLeft(BorderStyle.THIN)
      style.setBorderRight(BorderStyle.THIN)
      style
    }

    val headerStyle    = baseStyle(IndexedColors.DARK_BLUE.getIndex(), bold = true)
    val collapsedStyle = baseStyle(IndexedColors.GREY_25_PERCENT.getIndex(), bold = true)
    val defaultStyle   = baseStyle(IndexedColors.WHITE.getIndex(), bold = false)

    val maxColLengths = Array.fill(headers.length)(0)

    val headerRow = sheet.createRow(rowIdx)
    headers.zipWithIndex.foreach { case (h, i) =>
      val cell = headerRow.createCell(i)
      cell.setCellValue(h)
      cell.setCellStyle(headerStyle)
      maxColLengths(i) = math.max(maxColLengths(i), h.length)
    }
    headerRow.setHeightInPoints(22)
    rowIdx += 1

    for (row <- rows) {
      val collapseIdx = row.indexWhere(_.startsWith("!"))
      val r = sheet.createRow(rowIdx)
      r.setHeightInPoints(20)

      if (collapseIdx >= 0) {
        val before = row.take(collapseIdx).map(_.replaceAll(">", ""))
        before.zipWithIndex.foreach { case (v, i) =>
          val c = r.createCell(i)
          c.setCellValue(v)
          c.setCellStyle(collapsedStyle)
          maxColLengths(i) = math.min(maxColumnWidthChars, math.max(maxColLengths(i), v.length))
        }
        val collapseText = row(collapseIdx).stripPrefix("!")
        val cell = r.createCell(collapseIdx)
        cell.setCellValue(collapseText)
        cell.setCellStyle(collapsedStyle)
        sheet.addMergedRegion(new CellRangeAddress(rowIdx, rowIdx, collapseIdx, headers.length - 1))
      } else {
        row.map(_.replaceAll(">", "")).zipWithIndex.foreach { case (v, i) =>
          val c = r.createCell(i)
          c.setCellValue(v)
          c.setCellStyle(defaultStyle)
          maxColLengths(i) = math.min(maxColumnWidthChars, math.max(maxColLengths(i), v.length))
        }
      }
      rowIdx += 1
    }

    headers.indices.foreach { i =>
      val widthInChars = maxColLengths(i)
      val poiUnits = (widthInChars + 2) * 256
      sheet.setColumnWidth(i, poiUnits)
    }

    val out = new FileOutputStream(path)
    wb.write(out)
    out.close()
    wb.close()
  }

  private def toPlainText(useColor: Boolean): String = {
    val sb = new StringBuilder
    sb.append(title).append("\n")
    sb.append(renderDivider('-')).append("\n")
    sb.append(renderRow(headers, useColor, headers = true)).append("\n")
    sb.append(renderDivider('=')).append("\n")

    val rowCount = rows.length
    var i = 0

    while (i < rowCount) {
      val row = rows(i)
      val isCollapsed = row.exists(_.startsWith("!"))
      val prevCollapsed = i > 0 && rows(i - 1).exists(_.startsWith("!"))

      if (isCollapsed && !prevCollapsed && i > 0) {
        sb.append(renderDivider('-')).append("\n")
      }

      sb.append(renderRow(row, useColor)).append("\n")

      if (isCollapsed) {
        sb.append(renderDivider('-')).append("\n")
      }

      i += 1
    }

    val lastIsCollapsed = rows.nonEmpty && rows.last.exists(_.startsWith("!"))
    if (!lastIsCollapsed)
      sb.append(renderDivider('-'))

    sb.toString()
  }

  private def toMarkdownString: String = {
    val sb = new StringBuilder
    sb.append(s"### $title\n\n")
    sb.append("| ").append(headers.mkString(" | ")).append(" |\n")
    sb.append("|").append(headers.map(_ => "---").mkString("|")).append("|\n")

    for (row <- rows) {
      val collapseIdxOpt = row.indexWhere(_.startsWith("!")) match {
        case -1 => None
        case idx => Some(idx)
      }

      collapseIdxOpt match {
        case Some(collapseIdx) =>
          val prefixCells = row.take(collapseIdx).map { cell =>
            val indentLevel = cell.takeWhile(_ == '>').length
            val content = cell.drop(indentLevel)
            val indented = ("&nbsp;&nbsp;" * indentLevel) + content
            s"**$indented**"
          }
          val collapseText = row(collapseIdx).stripPrefix("!")
          val totalCols = headers.length
          val blankCells = totalCols - (prefixCells.length + 1)
          sb.append("| ")
          sb.append(prefixCells.mkString(" | "))
          if (prefixCells.nonEmpty) sb.append(" | ")
          sb.append(s"**$collapseText**")
          if (blankCells > 0)
            sb.append(" |" + (" " * blankCells).replace(" ", " |"))
          sb.append(" |\n")
        case None =>
          val rendered = row.map { cell =>
            val indentLevel = cell.takeWhile(_ == '>').length
            val content = cell.drop(indentLevel)
            ("&nbsp;&nbsp;" * indentLevel) + content
          }
          sb.append("| ").append(rendered.mkString(" | ")).append(" |\n")
      }
    }
    sb.toString()
  }

  private def toHtmlString: String = {
    val sb = new StringBuilder
    sb.append(s"<h3>$title</h3>\n<table border=\"1\" cellspacing=\"0\" cellpadding=\"4\">\n<thead><tr>")
    headers.foreach(h => sb.append(s"<th>$h</th>"))
    sb.append("</tr></thead>\n<tbody>\n")

    for (row <- rows) {
      val collapseIdxOpt = row.indexWhere(_.startsWith("!")) match {
        case -1 => None
        case idx => Some(idx)
      }

      collapseIdxOpt match {
        case Some(collapseIdx) =>
          sb.append("<tr>")
          val prefixCells = row.take(collapseIdx)
          prefixCells.foreach(c => sb.append(s"<td><b>${c}</b></td>"))
          val collapseText = row(collapseIdx).stripPrefix("!")
          val remainingCells = headers.length - prefixCells.length
          sb.append(s"<td colspan=\"$remainingCells\"><b>$collapseText</b></td>")
          sb.append("</tr>\n")
        case None =>
          sb.append("<tr>")
          row.foreach { cell =>
            val indentLevel = cell.takeWhile(_ == '>').length
            val content = cell.drop(indentLevel)
            val indented = ("&nbsp;&nbsp;" * indentLevel) + content
            sb.append(s"<td>$indented</td>")
          }
          sb.append("</tr>\n")
      }
    }
    sb.append("</tbody>\n</table>")
    sb.toString()
  }

  override def toString: String = format match {
    case "color_text" => toPlainText(useColor = true)
    case "markdown"    => toMarkdownString
    case "html"        => toHtmlString
    case "xls"         => s"(XLS written externally, not rendered here)"
    case _              => toPlainText(useColor = false)
  }
}