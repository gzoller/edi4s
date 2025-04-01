import co.blocke.edi4s.model._

object EDI855_5010 {
  def parse(edi: String): EDI855 = {
    EDI855(
      ST("855", "9999"),
      BAK("00", "AD", "PO5010", "20250329", Some("20250330"))
    )
  }

  def generate(doc: EDI855): String = {
    s"ST*${doc.st.transactionType}*${doc.st.transactionControlNumber}~" +
    s"BAK*${doc.bak.purposeCode}*${doc.bak.acknowledgmentType}*${doc.bak.purchaseOrderNumber}*${doc.bak.purchaseOrderDate}*${doc.bak.acknowledgmentDate.getOrElse("")}~"
  }
}
