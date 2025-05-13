package co.blocke.edi4s.model

// Config for delimiters
case class TokenizerConfig(
                        segmentDelimiter: Char = '~',
                        elementDelimiter: Char = '*',
                        componentDelimiter: Char = ':',  // for composite values
                        escapeCharacter: Char = '\\',
                        repeatDelimiter: Char = '^'
                      )
