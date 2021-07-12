#' @title
#' Convert characters from legacy Vietnamese encodings to UTF-8 encoding
#'
#' @param x data.frame or character vector
#' @param input Text encoding of input x
#' @param output Text encoding of output
#' @param diacritics logical. Preserve diacritics (TRUE) or not (FALSE)
# @param ... Additional arguments to gsubfn()
#'
#' @details
#' Many characters in legacy Vietnamese encodings (e.g. TCVN3, VNI, VPS, VISCII)
#' are not read correctly in R, particularly those with diacritics (accents). The particular
#' encodings don't seem to be supported by R, at least on many locales. Reading them as if they have UTF-8
#' encoding results in wrong characters being printed and garbled text (Mojibake).
#'
#' This functions converts character vectors to from various Vietnamese legacy encodings to readable
#' Unicode characters in UTF-8 encoding. By default the function attempts the conversion from TCVN3 to UTF-8
#' while preserving the diacritics, but also supports other Vietnamese encodings  (TCVN3, VNI, VPS, VISCII - via argument \code{input})
#'
#'  \code{diacritics = TRUE} will return characters with their diacritics. With \code{diacritics = FALSE},
#'  the output will be ASCII letters without diacritics. Upper/lower case will be preserved regardless.
#'
#' The internal search and replace is performed by the \code{\link[gsubfn]{gsubfn}} function from the \pkg{gsubfn} package. It performs a simple character replacements to fix the text.
#'
#' Currently the function converts from the Vietnamese encodings to UTF-8, not vice versa. Please contact the maintainer
#' if the conversion from Unicode to Vietnamese encodings would be relevant for you.
#'
#' The character conversion table was adapted from \url{http://vietunicode.sourceforge.net/charset/}.
#'
#' @return character string or data frame (depending on x)

#' @export
#'
#'
#' @examples
#'    # First we produce the wrongly formatted character string
#'    # using Unicode symbols is only necessary to create a portable example in the R package
#'    # you don't need to use Unicode characters like this in your data
#'
#'    string <- c("Qu\u00B6ng Tr\u00DE", "An \u00A7\u00ABn", "Th\u00F5a Thi\u00AAn Hu\u00D5")
#'
#'    # Below we have a look at the wrongly formatted character string.
#'    # This is what it would look like when you load TCVN3 encoded data as UTF8
#'    string
#'
#'    # convert character vector from TCVN3 > UTF-8
#'    decodeVN(string)
#'    decodeVN(string, diacritics = FALSE)
#'
#'    # # convert data frame columns from TCVN3 > UTF-8
#'    df <- data.frame(id = c(1,2,3),
#'                    name  = string)
#'
#'    df_decode <- decodeVN(df)
#'    df_decode
#'    # NOTE: some characters may be displayed as unicode in the R console
#'    # check the individual column to see if they are correct:
#'    df_decode[,2]
#'
#'    decodeVN(df, diacritics = FALSE)
#'
decodeVN <- function(x,
                     input = c("TCVN3", "VNI", "VPS", "VISCII", "Unicode"),
                     output = "Unicode", # c("Unicode", "TCVN3", "VNI", "VPS", "VISCII"),
                     diacritics = TRUE) {

  if(!class(x) %in% c("data.frame", "character")) stop("x must be a character vector or data.frame")
  enc_table <- loadEncodingTableVN()

  input <- match.arg(input)
  output <- match.arg(output)

  if(isFALSE(diacritics) & output != "Unicode") stop("diacritics can only be FALSE when output = 'Unicode'")


  if(diacritics) {
    tmp <- as.list(enc_table[,output])
  } else {
    tmp <- as.list(enc_table$ascii)
  }
  names(tmp) <- enc_table[, input]

  #names(tmp) <- sapply(enc_table[, input], FUN = function(x) as.character(Unicode::as.u_char(utf8ToInt(x))))
  #names(tmp) <- gsub("U+", "\u", names(tmp) )
  #paste0("[\u", as.hexmode(sapply(enc_table[, input], utf8ToInt)), "]")

  if(is.data.frame(x)) {
    char_cols <- which(sapply(x, typeof) %in% "character")

    if(length(char_cols) == 0) stop("No character columns in x")
    for(i in char_cols){
      x[,i] <- gsubfn::gsubfn(".", replacement = tmp, x = x[,i])
    }

    return(x)
  }

  if(is.character(x)) {


    out <- gsubfn::gsubfn(".", replacement = tmp, x = x, fixed = TRUE)    # add ...


    # out <- x
    # for(i in 1:length(tmp)){
    #   out <- gsub(pattern =  names(tmp)[i], replacement = tmp[[i]], x = x)
    # }
    # gsub(pattern =  "[\u1EA3]", replacement = tmp[[48]], x = x)   # why is this correct, above not??
    # # seems like gsubfn doesn't recognize the special character patterns correctly...


    return(out)
  }
}
