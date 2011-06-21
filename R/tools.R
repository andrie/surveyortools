# Additional tools to manipulate surveyor objects
# 
# Author: Andrie
###############################################################################

#' Returns variable.labels attribute of data.
#' 
#' @param dat Surveyor data object
#' @export
varlabels <- function(dat){
  attributes(dat)$variable.labels
}

#' Updates variable.labels attribute of data.
#' 
#' @param dat Surveyor data object
#' @param new_labels New variable labels
#' @export
replace_varlabels <- function(dat, new_labels){
  attributes(dat)$variable.labels <- new_labels
  dat
}

#' Updates names of variable.labels attribute of data.
#' 
#' @param dat Surveyor data object
#' @param new_names New variable labels
#' @export
replace_varlabel_names <- function(dat, new_names){
  names(attributes(dat)$variable.labels) <- new_names
  dat
}


#' Updates names and variable.labels attribute of data.
#' 
#' @param dat Surveyor data object
#' @param new_names New variable labels
#' @export
replace_names <- function(dat, new_names){
  names(dat) <- new_names
  dat <- replace_varlabel_names(dat, new_names)
  dat
}

#' Merges variable.labels attribute from two surveyor objects
#' 
#' Merges variable labels from two data objects.  The labels from dat1 takes precedence.
#' 
#' @param dat1 Surveyor data object
#' @param dat2 Surveyor data object
#' @param new_names A vector with names of the merged varlabels.  Defaults to the union of names of dat1 and dat2
#' @export
#' @keywords tools
merge_varlabels <- function(dat1, dat2, new_names=union(names(dat1), names(dat2))){
  labels1 <- varlabels(dat1)
  labels2 <- varlabels(dat2)
  names(labels1) <- names(dat1)
  names(labels2) <- names(dat2)
  #merge(labels1, labels2)
  ret <- new_names
  names(ret) <- ret
  ret[names(labels2)] <- labels2
  ret[names(labels1)] <- labels1
  ret
}


#' Merge surveyor data objects.
#' 
#' The base R merge will merge data but not all of the attributes.  This function also merges the variable.labels attribute.
#'
#' @param dat1 Data frame
#' @param dat2 Data frame
#' @param ... Other parameters passed to merge
#' @export 
#' @keywords tools
surveyor_merge <- function(dat1, dat2, ...){
#  var_labels <- attributes(dat1)$variable.labels
#  new_vars <- names(dat2)[!(names(dat2) %in% names(dat1))]
#  var_labels[new_vars] <- new_vars
#  tmp <- merge(dat1, dat2, ...)
#  attributes(tmp)$variable.labels <- var_labels
  tmp <- merge(dat1, dat2, ...)
  newlabels <- merge_varlabels(dat1, dat2, new_names=names(tmp))
  tmp <- replace_varlabels(tmp, new_labels=newlabels)
  tmp
}

#' Renames surveyor data columns.
#' 
#' When renaming surveyor data objects, it's important to also rename the variable.labels attribute.
#' 
#' @param dat Surveyor data object
#' @param colnames Character vector with column names to modify
#' @param newnames Character vector with replacement names
#' @export
#' @keywords tools
surveyor_rename_cols <- function(dat, colnames, newnames){
  stopifnot(length(colnames)==length(newnames))
  x <- unname(sapply(colnames, function(xt)grep(xt, names(dat))))
  stopifnot(length(x)==length(newnames))
  names(dat)[x] <- newnames

  #  x <- unname(sapply(colnames, function(xt)grep(xt, names(attributes(dat)$variable.labels))))
  x <- unname(sapply(colnames, function(xt)grep(xt, names(varlabels(dat)))))
  stopifnot(length(x)==length(newnames))
  names(attributes(dat)$variable.labels)[x] <- newnames

  dat
}



#' Extract block of questions from surveyor object.
#'
#' @param surveyor Surveyor object
#' @param  extract Character vector of question names to extract
#' @export 
#' @keywords tools
surveyor_extract <- function(surveyor, extract){
  require(surveyor)
  data <- surveyor$q_data
  Qs   <- surveyor$q_text
  qnames <- unlist(sapply(extract, 
          function(qid){
            tmp <- surveyor::get_q_text_unique(data, qid, Qs)
            ifelse(tmp=="", Qs[qid], tmp)
          }
      )
  )
  cluster_pattern <- paste(paste("^", extract, "(_\\d*)*$", sep=""), collapse="|")
  names(qnames) <- grep(cluster_pattern, names(data), value=TRUE) 
  cdata <- data[, grep(cluster_pattern, names(data))]
  attributes(cdata)$qtext <- qnames
  names(cdata) <- names(qnames)
  cdata
}

#' Create list of questions in survey
#'
#' @param surveyor Surveyor object
#' @export 
#' @keywords tools
question_list <- function(surveyor){
  unique(gsub("_[[:digit:]]*(_other)?$", "", names(surveyor$q_data)))
}
