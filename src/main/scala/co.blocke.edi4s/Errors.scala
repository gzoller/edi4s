package co.blocke.edi4s


case class WrongNumberOfElementsException(segment: String, expected: Int, actual: Int) extends Exception {
  override def getMessage: String =
    s"Wrong number of elements in segment $segment. Expected $expected, but got $actual."
}

case class MissingRequiredFieldException(fieldNum: Int, segment: String) extends Exception {
  override def getMessage: String = s"Required field in position $fieldNum of segment $segment is missing in data."
}

// Custom error for unexpected end of data
case class UnexpectedEndOfData(message: String) extends Exception(message)
