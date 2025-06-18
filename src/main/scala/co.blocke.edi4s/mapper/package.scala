package co.blocke.edi4s
package mapper

// Calculates correct BSN005 value given the structure we're returning for 856 HL nested levels
//
val hlStructureToBsn05: Map[String, String] = Map(
  "SOI"    -> "0004",  // Shipment → Order → Item
  "SOPI"   -> "0001",  // Shipment → Order → Pack → Item
  "SOTPI"  -> "0009",  // Shipment → Order → Tare → Pack → Item
  "SOTP"   -> "0008",  // Shipment → Order → Tare → Pack (no Item)
  "SOP"    -> "0008",  // (Some partners use this too; edge case)
  "SPI"    -> "0003",  // Shipment → Pack → Item
  "SO"     -> "0004",  // Shipment → Order
  "STPI"   -> "0009",  // Shipment → Tare → Pack → Item (if skipping Order)
  "SP"     -> "0003",  // Shipment → Pack
  "SOIREF" -> "0004",  // Rare case: Shipment → Order → Item + extra metadata
  // Add more as needed
)