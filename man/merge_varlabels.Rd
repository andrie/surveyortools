\name{merge_varlabels}
\alias{merge_varlabels}
\title{Merges variable...}
\usage{merge_varlabels(dat1, dat2, new_names=union(names(dat1), names(dat2)))}
\description{Merges variable.labels attribute from two surveyor objects}
\details{Merges variable labels from two data objects.  The labels from dat1 takes precedence.}
\keyword{tools}
\arguments{\item{dat1}{Surveyor data object}
\item{dat2}{Surveyor data object}
\item{new_names}{A vector with names of the merged varlabels.  Defaults to the union of names of dat1 and dat2}}
