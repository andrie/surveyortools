\name{encToInt}
\alias{encToInt}
\title{Converts a character vector to an integer vector...}
\usage{encToInt(x, encoding=localeToCharset())}
\description{Converts a character vector to an integer vector}
\details{Conversion of character vector to integer vector.  The encoding of the character vector can be specified but will default to the current locale.}
\value{An integer vector}
\seealso{\code{\link{intToEnc}}, \code{\link{iconv}} and \code{\link{utf8ToInt}}}
\keyword{encoding}
\arguments{\item{x}{Character vector}
\item{encoding}{A character string describing the encoding of x.  Defaults to the current locale.  See also \code{\link{iconvlist}}}}
\examples{encToInt("\\xfa")}
