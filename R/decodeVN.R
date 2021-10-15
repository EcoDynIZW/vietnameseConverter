#' @title
#' Convert characters from legacy Vietnamese encodings to UTF-8 encoding
#'
#' @param x data.frame or character vector
#' @param from Text encoding of input x
#' @param to Text encoding of output
#' @param diacritics logical. Preserve diacritics (TRUE) or not (FALSE)
# @param ... Additional arguments to gsubfn()
#'
#' @details
#' Many characters in legacy Vietnamese encodings (e.g. TCVN3, VPS, VISCII)
#' are not read correctly in R, particularly those with diacritics (accents). The particular
#' encodings don't seem to be supported by R, at least on many locales. Reading them as if they have UTF-8
#' encoding results in wrong characters being printed and garbled text (Mojibake).
#'
#' This functions converts character vectors to from various Vietnamese legacy encodings to readable
#' Unicode characters in UTF-8 encoding. By default the function attempts the conversion from TCVN3 to UTF-8
#' while preserving the diacritics, but also supports other Vietnamese encodings  (TCVN3, VPS, VISCII - via argument \code{from}).
#' Currently VNI and VNU are not supported.
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
#' @section Warning:
#' When printing a data frame with Unicode characters using the standard print method, the R console will show the Unicode escape characters (e.g. "<U+1EA3>") instead of the actual Unicode characters. This is a limitation of the R console. The data are correct and will show correctly when using e.g. View() or when printing columns as vectors.
#'
#' @return character string or data frame (depending on x)
#'
#' @export
#' @importFrom methods is
#' @importFrom utf8 as_utf8
#' @importFrom gsubfn gsubfn
#' @importFrom sf st_geometry
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_set_geometry
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
#'    # using the built-in sample data
#'    data(vn_samples)
#'    decodeVN(vn_samples$TCVN3)   # TCVN -> Unicode   # TCVN3 -> Unicode
#'    decodeVN(vn_samples$TCVN3, diacritics = FALSE)   # TCVN3 -> Unicode (ASCII characters only)
#'    decodeVN(vn_samples$VISCII, from = "VISCII")     # VISCII -> Unicode
#'
decodeVN <- function(x,
                     from = c("TCVN3", "VISCII", "VPS", "Unicode"),     #  "VNI",  "VNU",
                     to =  c("Unicode", "TCVN3", "VISCII", "VPS"),   # "VNI"
                     diacritics = TRUE
                     ) {


  # if spatial objects, temporarily store spatial information
  if(is(x, "sf")) {
    spatial <- TRUE
    x_geometry <- st_geometry(x)  # temporarily store geometry column
    x          <- st_drop_geometry(x)      # remove geometry column
  } else {
    spatial <- FALSE
  }

  if(inherits(x, "data.frame")) x <- as.data.frame(x)    # for tibbles, sf, data.table

  if(!class(x) %in% c("data.frame", "character")) stop("x must be a character vector or data.frame")
  enc_table <- loadEncodingTableVN(version = 2)

  from <- match.arg(from, choices = c("TCVN3", "VISCII", "VPS", "Unicode"))
  to <- match.arg(to, choices = c("Unicode", "TCVN3", "VISCII", "VPS"))

  if(isFALSE(diacritics) & to != "Unicode") stop("diacritics can only be FALSE when 'to' != 'Unicode'")

  # set enginge for gsubfn
  perl <- TRUE     # the default sometimes led to unexpected results

  if(diacritics) {
    tmp <- as.list(enc_table[,to])
  } else {
    tmp <- as.list(enc_table$ASCII)
  }
  names(tmp) <- enc_table[, from]


  if(is.data.frame(x)) {
    char_cols <- which(sapply(x, typeof) %in% "character")

    if(length(char_cols) == 0) stop("No character columns in x")
    out <- x
    for(i in char_cols){
      out[,i] <- as_utf8(x[,i])
      which_na <- which(is.na(out[,i]))
      if(length(which_na) >= 1){
        out[-which_na,i] <- gsubfn(".", replacement = tmp, x = out[-which_na ,i], perl = perl)
      } else {
        out[,i] <- gsubfn(".", replacement = tmp, x = out[ ,i], perl = perl)
      }
    }

    if(spatial) {
      out <- st_set_geometry(out, x_geometry) # assign geometry column again
    }
  }

  if(is.character(x)) {

    x <- as_utf8(x)

    # if(from == "VNI"){     # doesn't yet work for all characters
    #   x_tmp <- x
    #   for(i in 1:length(tmp))
    #     x_tmp <- gsubfn(pattern = names(tmp)[i], replacement = tmp[[i]], x = x_tmp, perl = T)
    # } else {

    # }

    which_na <- which(is.na(x))
    if(length(which_na) >= 1){
      out <- x
      out[-which_na] <- gsubfn(".", replacement = tmp, x = x[-which_na], perl = T)
    } else {
      out <- gsubfn(".", replacement = tmp, x = x, perl = T)
    }
  }

  return(out)
}
