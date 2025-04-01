package co.blocke.edi4s.model

trait EDIDocument

case class ST(
  transactionType: String,                // ST01 - Required in all versions
  transactionControlNumber: String       // ST02 - Required in all versions
)

case class BAK(
  purposeCode: String,                   // BAK01 - Required in all versions
  acknowledgmentType: String,           // BAK02 - Required in all versions
  purchaseOrderNumber: String,          // BAK03 - Required in all versions
  purchaseOrderDate: String,            // BAK04 - Required in all versions
  acknowledgmentDate: Option[String]    // BAK09 - Required in 4030, optional in 5010
)

case class EDI855(
  st: ST,
  bak: BAK
) extends EDIDocument
