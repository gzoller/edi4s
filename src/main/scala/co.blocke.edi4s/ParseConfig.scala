package co.blocke.edi4s

// Config for delimiters
case class ParseConfig(
                        segmentDelimiter: Char = '~',
                        elementDelimiter: Char = '*',
                        componentDelimiter: Char = ':',  // for composite values
                        escapeCharacter: Char = '\\'
                      )
