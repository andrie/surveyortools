# Functions to perform data cleanup
# 
# Author: Andrie
###############################################################################


#' Tests whether levels contain "Don't know".
#' 
#' Returns TRUE if x contains any instances of dk  
#' 
#' @param x Character vector or Factor 
#' @param dk Character vector, containing search terms, e.g. c("Don't know", "Don't Know")
#' @return TRUE or FALSE
#' @seealso \code{\link{remove_dk}} and \code{\link{remove_all_dk}}
#' @export 
#' @keywords "clean data"
has_dk <- function(x, dk="Don't Know"){
	ifelse(is.factor(x), 
			l <- levels(x),
			l <- unique(x)
	)
	any(l %in% dk)
}

#' Removes "Don't know" from levels and replaces with NA.
#' 
#' Tests the levels of x contain any instances of "Don't know".  If so, replaces these levels with NA
#' 
#' @param x Vector or Factor 
#' @param dk Character vector, containing search terms, e.g. c("Don't know", "Don't Know")
#' @return A factor with "Dont know" removed
#' @seealso \code{\link{has_dk}} and \code{\link{remove_all_dk}}
#' @export 
#' @keywords "clean data"
remove_dk <- function(x, dk="Don't Know"){
	if (has_dk(x, dk)){
    if(is.factor(x)){
  		l <- levels(x)
  		l[which(levels(x) %in% dk)] <- NA
  		x <- factor(x, levels=l)
    } else {
      pattern <- paste("^(", paste(dk, collapse="|"), ").?$", sep="")
      x <- gsub(pattern, "", x)
    }
  }  
  x
}

#' Removes "Do not know" and other similar words from factor levels in data frame.
#' 
#' Removes "Do not know" and other similar words from factor levels in data frame
#' 
#' @param x List or data frame 
#' @param dk Character vector, containing search terms, e.g. c("Do not know", "DK").  These terms will be replaced by NA. If NULL, will default to c("I don't know", "Don't Know", "Don't know", "Dont know" , "DK")
#' @seealso \code{\link{has_dk}} and \code{\link{remove_dk}}
#' @return A data frame
#' @export
#' @keywords "clean data"
remove_all_dk <- function(x, dk=NULL){
	if (is.null(dk)) dk <- c("I don't know", "Don't Know", "Don't know", "Dont know" , "DK")		
	newx <- lapply(x, remove_dk, dk)
	n1 <- sum(as.numeric(lapply(x, has_dk, dk)))
	n2 <- sum(as.numeric(lapply(newx, has_dk, dk)))
	dk <- paste(dk, collapse=", ")
	message(paste("Removed", n1-n2,"instances of levels that equal [", dk, "]"))
	ret <- quickdf(newx)
	attributes(ret) <- attributes(x)
  class(ret) <- class(x)
	ret
}	

#' Fix level formatting of all question with Yes/No type answers.
#' 
#' @param x Surveyor data object
#' @export
#' @keywords "clean data"
#' @seealso \code{\link{fix_levels_01_SPSS}}, \code{\link{fix_levels_01_R}}
leveltest_SPSS <- function(x){
  ret <- FALSE
  if(class(x)=="numeric"){
    if(!is.null(attributes(x)$value.labels)){
      if(all(attributes(x)$value.labels==c(1, 0))){
        ret <- TRUE
      }}}
  ret
}

#' Fix level formatting of all question with Yes/No type answers.
#' 
#' @param dat Surveyor data object
#' @export
#' @keywords "clean data"
fix_levels_01_SPSS <- function(dat){
  ret <- lapply(dat, function(x){
        if(leveltest_SPSS(x)){
          x <- factor(x)
          levels(x) <- c("No", "Yes")
          x
        } else {
          x
        }
      }
  )
  ret <- plyr::quickdf(ret)
  attributes(ret)$variable.labels <- varlabels(dat)
  ret
}

#' Fix level formatting of all question with Yes/No type answers.
#' 
#' @param x Surveyor data object
#' @export
#' @keywords "clean data"
#' @seealso \code{\link{fix_levels_01_SPSS}}, \code{\link{fix_levels_01_R}}
leveltest_R <- function(x){
  ret <- FALSE
  if(class(x)=="factor"){
    if(length(levels(x))==2){
      if(all(levels(x)==c("Yes", "Not selected"))){
        ret <- TRUE
      }}}
  ret
}

#' Fix level formatting of all question with Yes/No type answers.
#' 
#' @param dat surveydata object
#' @export
#' @keywords "clean data"
fix_levels_01_R <- function(dat){
  stopifnot(is.surveydata(dat))
  ret <- lapply(dat, function(x){
        if(leveltest_R(x)){
          levels(x) <- c("Yes", "No")
          x
        } else {
          x
        }
      }
  )
  ret <- plyr::quickdf(ret)
  pattern(ret) <- pattern(dat)
  varlabels(ret) <- varlabels(dat)
  as.surveydata(ret)
}

