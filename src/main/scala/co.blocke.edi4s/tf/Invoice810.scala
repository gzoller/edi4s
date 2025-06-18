package co.blocke.edi4s.tf

import co.blocke.scalajack.*

@xmlLabel("_DOC")
case class Invoice810(
                       @xmlLabel("TYPE") `type`: String,
                       recid: String,
                       invoiceno: String,
                       invoicedate: String,
                       orderno: String,
                       ordertype: String,
                       orderdate: String,
                       purchaseorderno: String,
                       cusno: String,
                       division: String,
                       department: String,
                       shipdate: String,
                       canceldate: String,
                       collect: String,
                       bolno: String,
                       iscreditmemo: String,
                       miscamount: Double,
                       freightamount: Double,
                       totinvoiceamount: Double,
                       tottaxableamount: Double,
                       totsalesamount: Double,
                       totcartons: Int,
                       totweight: Double,
                       lineitemtotal: Int,
                       @xmlStruct @xmlLabel("_CARRIER") carrier: Carrier,
                       @xmlStruct @xmlLabel("_MESSAGE") message: List[Message],
                       @xmlStruct @xmlLabel("_TERMS") terms: Terms,
                       @xmlStruct @xmlLabel("_CURRENCY") currency: Currency,
                       @xmlStruct @xmlLabel("_CREDITMEMO") creditmemo: CreditMemo,
                       @xmlStruct @xmlLabel("_ADDRESS") address: List[Address],
                       @xmlStruct @xmlLabel("_TAX") tax: Tax,
                       @xmlStruct @xmlLabel("_USERDEF") userdef: List[Userdef],
                       @xmlStruct @xmlLabel("_ITEM") item: List[Item]
                     ):
  def toX12: String =
    def segment(elems: String*): String = elems.mkString("", "*", "~\n")

    val header = new StringBuilder

    header ++= segment("ISA", "00", "", "00", "", "ZZ", "SENDERID", "ZZ", "RECEIVERID", "240603", "1200", "U", "00401", "000000001", "0", "P", ">")
    header ++= segment("GS", "IN", "SENDERID", "RECEIVERID", "20240603", "1200", "1", "X", "005010")
    header ++= segment("ST", "810", recid)
    header ++= segment("BIG", invoicedate, invoiceno, orderdate, orderno)

    // Loop N1 - Bill To
    address.find(_.`type` == "billto").foreach { addr =>
      header ++= segment("N1", "BT", addr.name, "92", addr.id)
      if addr.add1.nonEmpty then header ++= segment("N3", addr.add1)
      if addr.city.nonEmpty || addr.state.nonEmpty || addr.zip.nonEmpty then header ++= segment("N4", addr.city, addr.state, addr.zip)
    }

    // Loop N1 - Ship To
    address.find(_.`type` == "shipto").foreach { addr =>
      header ++= segment("N1", "ST", addr.name, "92", addr.id)
      if addr.add1.nonEmpty then header ++= segment("N3", addr.add1)
      if addr.city.nonEmpty || addr.state.nonEmpty || addr.zip.nonEmpty then header ++= segment("N4", addr.city, addr.state, addr.zip)
    }

    // ITD - Payment Terms
    val t = terms
    header ++= segment("ITD", "", "", t.discountpercent.toString, "", t.discountdays.toString, "", t.duedays.toString)

    // TDS - Total Monetary Value Summary (amount in cents)
    header ++= segment("TDS", (totinvoiceamount * 100).toInt.toString)

    // Loop IT1 - Line items
    item.zipWithIndex.foreach { case (item, i) =>
      header ++= segment(
        "IT1",
        (i + 1).toString,
        item.qtytoship.toString,
        item.uom,
        item.price.toString,
        "",
        "BP",
        item.itemid,
        "VP",
        item.cusitemid
      )
      if item.itemdesc.nonEmpty then header ++= segment("PID", "F", "", "", "", item.itemdesc)
    }

    // Tax summary
    if tax.taxamt > 0 then header ++= segment("TXI", "TX", tax.taxamt.toString, "", "", tax.taxpercent.toString)

    // CTT - Transaction Totals
    header ++= segment("CTT", item.size.toString)

    // SE - Transaction set trailer
    val segmentCount = header.toString().count(_ == '~') + 1
    header ++= segment("SE", segmentCount.toString, recid)
    header ++= segment("GE", "1", "1")
    header ++= segment("IEA", "1", "000000001")

    header.toString()

case class Carrier(
                    carrierid: String,
                    carrierdesc: String
                  )

case class Message(
                    @xmlLabel("TYPE") `type`: String,
                    message: String
                  )

case class Terms(
                  id: String,
                  desc: String,
                  duedays: Int,
                  discountdays: Int,
                  discountpercent: Int,
                  discountdate: String,
                  datedue: String
                )

case class Currency(
                     currencycode: String,
                     currencyrate: Double
                   )

case class CreditMemo(
                       origordtype: String,
                       origordno: String,
                       origorddate: String,
                       applytono: Int
                     )

case class Address(
                    @xmlLabel("TYPE") `type`: String,
                    id: String,
                    name: String,
                    add1: String,
                    add2: String,
                    add3: String,
                    city: String,
                    state: String,
                    zip: String,
                    country: String,
                    contact: String,
                    phone: String,
                    fax: String,
                    email: String,
                    @xmlStruct @xmlLabel("_USERDEF") userdef: List[Userdef]
                  )

case class Userdef(
                    @xmlLabel("TYPE") `type`: String,
                    userdef: String
                  )

case class Tax(
                taxsched: String,
                taxcode: String,
                taxableamount: Double,
                taxamt: Double,
                taxpercent: Double,
                taxstate: String
              )

case class Item(
                 recid: String,
                 lineno: Double,
                 itemid: String,
                 cusitemid: String,
                 itemdesc: String,
                 itemdesc2: String,
                 price: Double,
                 extendedprice: Double,
                 taxable: String,
                 taxflag: String,
                 extendedtaxamount: String,
                 qtyord: Double,
                 qtytoship: Double,
                 uom: String,
                 requestdate: String,
                 promisedate: String,
                 requestedshipdate: String,
                 pickdate: String,
                 shipdate: String,
                 qtyreturntostk: String,
                 reasoncd: String,
                 @xmlStruct @xmlLabel("_MESSAGE") message: List[Message],
                 @xmlStruct @xmlLabel("_USERDEF") userdef: List[Userdef],
                 @xmlStruct @xmlLabel("_ITEMTAX") itemtax: ItemTax
               )

case class ItemTax(
                    taxableamt: Double,
                    taxamount: Double,
                    taxsched: String,
                    taxcd: String
                  )