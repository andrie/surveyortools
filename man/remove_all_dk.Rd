\name{remove_all_dk}
\alias{remove_all_dk}
\title{Removes "Do not know" and other similar words from factor levels in data frame.}
\usage{remove_all_dk(x, dk)}
\description{Removes "Do not know" and other similar words from factor levels in data frame.}
\details{Removes "Do not know" and other similar words from factor levels in data frame}
\seealso{\code{\link{has_dk}} and \code{\link{remove_dk}}}
\value{A data frame}
\keyword{"clean}
\keyword{data"}
\arguments{\item{x}{List or data frame}
\item{dk}{Character vector, containing search terms, e.g. c("Do not know", "DK").  These terms will be replaced by NA. If NULL, will default to c("I don't know", "Don't Know", "Don't know", "Dont know" , "DK")}}
