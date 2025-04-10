package co.blocke.edi4s
package tokenizer


// Add this method inside ParseContext
case class TokenizerContext(
                         doc: String,
                         offset: Int,
                         config: TokenizerConfig
                       )