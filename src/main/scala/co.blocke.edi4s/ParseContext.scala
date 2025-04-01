package co.blocke.edi4s

import model.*

// Add this method inside ParseContext
case class ParseContext(
                         doc: String,
                         docSpec: DocumentSpec,
                         offset: Int,
                         config: ParseConfig
                       ):

  def getNextToken: (String, ParseContext) = {
    val currentOffset = offset.toInt

    // Start scanning from the current offset
    val remainingDoc = doc.substring(currentOffset)

    // Find the position of the next segment delimiter
    val segmentEndPos = remainingDoc.indexOf(config.segmentDelimiter)
    if (segmentEndPos == -1) {
      // No more segments
      return ("", this)  // Empty string, indicating the end of the document
    }

    // Extract the segment name (up to the first delimiter)
    val segmentName = remainingDoc.substring(0, segmentEndPos).trim

    // Create a new ParseContext with the updated offset
    val updatedContext = this.copy(offset = currentOffset + segmentEndPos + 1)

    (segmentName, updatedContext)
  }
