package co.blocke.edi4s
package table

//import java.io.FileOutputStream
//import org.apache.poi.ss.usermodel._
//import org.apache.poi.xssf.usermodel.XSSFWorkbook
//import org.apache.poi.ss.util.CellRangeAddress
//import org.apache.poi.ss.usermodel.IndexedColors

import scala.Console._

enum Style:
  case PRIMARY, SECONDARY, TERTIARY, NEUTRAL, MUTED, ALERT, WARN

enum Align:
  case LEFT, CENTER, RIGHT, DECIMAL

case class Cell(
                 content: String,
                 style: Option[Style] = None,
                 align: Option[Align] = None,
                 indent: Int = 0,
                 colspan: Int = 1
               )

sealed trait TableRow:
  val cells: List[Cell]
  val style: Style
  val align: Align

sealed trait BodyRow extends TableRow

case class Title(cells: List[Cell], style: Style = Style.PRIMARY, align: Align = Align.CENTER) extends TableRow

case class Header(cells: List[Cell], style: Style = Style.PRIMARY, align: Align = Align.CENTER) extends TableRow

case class SubHeader(cells: List[Cell], style: Style = Style.SECONDARY, align: Align = Align.LEFT) extends BodyRow

case class Row(cells: List[Cell], style: Style = Style.NEUTRAL, align: Align = Align.LEFT) extends BodyRow

val defaultStyleMap: Map[String, Map[Style, String]] = Map(
  "text" -> Map(
    Style.PRIMARY -> "\u001b[1m\u001b[34m", // Bold Blue
    Style.SECONDARY -> "\u001b[1m\u001b[36m", // Bold Cyan
    Style.TERTIARY -> "\u001b[32m", // Green
    Style.NEUTRAL -> "",
    Style.MUTED -> "\u001b[2m", // Dim
    Style.ALERT -> "\u001b[31m", // Red
    Style.WARN -> "\u001b[38;5;180m"
  )
)

trait Renderer:
  def render(): String
  def normalizedColumnWidths: List[Int]
  def effectiveStyle(style: Style): String
  def effectiveAlign(cell: Cell, rowAlign: Align): Align


case class Table(
                  title: List[Title],
                  columns: Int,
                  columnWidthPct: List[Int],
                  tableWidth: Int,
                  header: Header,
                  rows: List[BodyRow],
                  styleMap: Map[String, Map[Style, String]] = defaultStyleMap
                ):
  override def toString: String =
    TextRenderer(this).render()

  def toHtml: String =
    HtmlRenderer(this).render()

