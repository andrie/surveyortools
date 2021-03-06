#' Converts a character vector to an integer vector
#' 
#' Conversion of character vector to integer vector.  The encoding of the character vector can be specified but will default to the current locale.  
#' 
#' @param x Character vector 
#' @param encoding A character string describing the encoding of x.  Defaults to the current locale.  See also \code{\link{iconvlist}}
#' @return An integer vector
#' @seealso \code{\link{intToEnc}}, \code{\link{iconv}} and \code{\link{utf8ToInt}}
#' @examples
#' encToInt("\\xfa")
#' @export 
#' @keywords encoding
encToInt <- function(x, encoding=localeToCharset()){
	utf8ToInt(iconv(x, from=encoding, to="UTF-8"))
}

#' Converts an integer vector to a character vector.
#' 
#' Conversion of integer vector to character vector.  The encoding of the character vector can be specified but will default to the current locale.  
#' 
#' @param x Integer vector 
#' @param encoding A character string describing the encoding of x.  Defaults to the current locale.  See also \code{\link{iconvlist}}
#' @return A character vector
#' @seealso \code{\link{encToInt}}, \code{\link{iconv}} and \code{\link{utf8ToInt}}
#' @examples
#' intToEnc(8212)
#' @export 
#' @keywords encoding
intToEnc <- function(x, encoding=localeToCharset()){
	iconv(intToUtf8(x), from="UTF-8", to=encoding)
}

#' Fix common encoding problems when working with web imported data.
#' 
#' This function tries to resolve typical encoding problems when importing web data on Windows.
#' Typical problems occur with pound and hyphen (-), especially when these originated in MS-Word.
#' @param x A character vector
#' @export
#' @keywords encoding
fix_common_encoding_problems <- function(x){
  # Define character constants that need to be replaced
  ps <- list()
  ps[[1]] <- c(intToEnc(194), "")
  ps[[2]] <- c(intToEnc(128), "")
  ps[[3]] <- c(intToEnc(226), "-")
  ps[[4]] <- c(intToEnc(147), "")
  ps[[5]] <- c("^Missing$", "NA")
  # Now perform the actual processing
  for(i in 1:length(ps)){
    x <- gsub(ps[[i]][1], ps[[i]][2], x)
  }
  x 
}

