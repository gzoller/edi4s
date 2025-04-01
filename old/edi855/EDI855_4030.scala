import co.blocke.edi4s.model._

object EDI855_4030 {
  def parse(edi: String): EDI855 = {
    EDI855(
      ST("855", "0001"),
      BAK("00", "AC", "PO123", "20250327", Some("20250328"))
    )
  }

  def generate(doc: EDI855): String = {
    s"ST*${doc.st.transactionType}*${doc.st.transactionControlNumber}~" +
    s"BAK*${doc.bak.purposeCode}*${doc.bak.acknowledgmentType}*${doc.bak.purchaseOrderNumber}*${doc.bak.purchaseOrderDate}*${doc.bak.acknowledgmentDate.getOrElse("")}~"
  }
}
