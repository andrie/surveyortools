\name{has_dk}
\alias{has_dk}
\title{Tests whether levels contain "Don't know".}
\usage{has_dk(x, dk="Don't Know")}
\description{Tests whether levels contain "Don't know".}
\details{Returns TRUE if x contains any instances of dk}
\value{TRUE or FALSE}
\seealso{\code{\link{remove_dk}} and \code{\link{remove_all_dk}}}
\keyword{"clean}
\keyword{data"}
\arguments{\item{x}{Character vector or Factor}
\item{dk}{Character vector, containing search terms, e.g. c("Don't know", "Don't Know")}}
