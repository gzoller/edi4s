package co.blocke.edi4s
package model


// Paths do NOT contain the terminal (ie segment/field name). Just the path to get there.
case class Path(value: String = ""):
  def prefix: Path =
    value.lastIndexOf('.') match
      case -1 => this
      case i => Path(value.substring(0, i))
  override def toString: String = value
  def toStringWith(terminal: String): String =
    if value == "" then terminal else s"${value}.${terminal}"
  def dot(level: String): Path =
    if value == "" then Path(level) else Path(s"${value}.${level}")
  def nest(level: String): Path =
    if value == "" then
      Path(level)
    else
      Path(s"${value}>${level}")