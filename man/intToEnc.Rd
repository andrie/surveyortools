\name{intToEnc}
\alias{intToEnc}
\title{Converts an integer vector to a character vector.}
\usage{intToEnc(x, encoding=localeToCharset())}
\description{Converts an integer vector to a character vector.}
\details{Conversion of integer vector to character vector.  The encoding of the character vector can be specified but will default to the current locale.}
\value{A character vector}
\seealso{\code{\link{encToInt}}, \code{\link{iconv}} and \code{\link{utf8ToInt}}}
\keyword{encoding}
\arguments{\item{x}{Integer vector}
\item{encoding}{A character string describing the encoding of x.  Defaults to the current locale.  See also \code{\link{iconvlist}}}}
\examples{intToEnc(8212)}
