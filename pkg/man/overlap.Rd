
\name{overlap}
\docType{methods}

\alias{overlap}
\alias{overlap-methods}
\alias{overlap,Performance,missing-method}
\alias{overlap,Performance,Performance-method}

\title{ Find conceptual overlap between performances }

\description{

	\code{overlap} returns a character vector containing the terms shared across the two performances
   (or list of performances). The meaningvector of each performance activates certain terms in the
   social semantic vector space. This function identifies all terms that are activated by both (or all)
   performances.
   
}


\usage{

overlap(x)

overlap(x, y)

}

\arguments{
  \item{x}{a \code{Performance} or a vector of references pointing to performances (such as returned by \code{\link{performances}}). The vector has to have the class attribute \code{Performance}.}
  \item{y}{optional: a second \code{Performance} }
}

\value{
	\code{NULL} or character vector with the names of the terms.
}

\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\keyword{mpia}
\keyword{terms}
\keyword{concepts}
\keyword{overlap}
