\name{remove_dk}
\alias{remove_dk}
\title{Removes "Don't know" from levels and replaces with NA.}
\usage{
  remove_dk(x, dk = "Don't Know")
}
\arguments{
  \item{x}{Vector or Factor}

  \item{dk}{Character vector, containing search terms, e.g.
  c("Don't know", "Don't Know")}
}
\value{
  A factor with "Dont know" removed
}
\description{
  Tests the levels of x contain any instances of "Don't
  know".  If so, replaces these levels with NA
}
\seealso{
  \code{\link{has_dk}} and \code{\link{remove_all_dk}}
}
\keyword{"clean}
\keyword{data"}

