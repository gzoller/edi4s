package co.blocke.edi4s
package model


// Separate the "show" funcitonality--don't mess up the model classes
object CanonicalShow:

  def show[T](value: T, tab: Int, schemas: Map[String, EdiEnum | EdiSchema]) :String =
    value match {
      case v: EdiEnum =>
        " " * tab * 2 + "(enum value)\n"
      case v: EdiSchema =>
        val label = v.`x-openedi-segment-id`.map(s => "segment " + s)
          .orElse(v.`x-openedi-loop-id`.map(c => "loop " + c))
          .orElse(v.`x-openedi-composite-id`.map(c => "composite " + c))
          .getOrElse("unknown")
        " " * tab * 2 + s"$label\n" +
          v.properties.map { case (k, v) =>
            " " * (tab) * 2 + k + " -- " + show(v, tab, schemas)
          }.mkString("\n")
      case v: EdiRefProperty =>
        val extractKey = ".*/([^/]+)$".r
        v.`$ref` match {
          case extractKey(key) =>
            val s = schemas(key)
            show(schemas(key), tab + 1, schemas)
          case _ => throw new Exception(s"Can't find reference ${v.`$ref`} in schema")
        }
      case v: EdiItemsProperty =>
        s"items (${v.`type`}) " + show(v.items, tab, schemas)
      case v: EdiElementProperty =>
        s"""element (${v.`type`}) ${v.format.getOrElse("no fmt")} - ${if v.allOf.nonEmpty then "has constraints" else "no constraints"} - ${v.`x-openedi-element-id`.getOrElse("no id")}"""
      case _ => "Unknown canonical type"
    }
