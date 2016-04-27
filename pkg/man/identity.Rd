
\name{identity}
\docType{methods}

\alias{==-methods}
\alias{==,Performance,Performance-method}
\alias{==,Person,Person-method}

\title{ Testing for Identity }
\description{
	\code{==} tests whether two performances or two persons are identical, i.e. in very close proximity above the \code{identityThreshold} of the domain.
}
\section{Methods}{
\describe{

	Returns \code{TRUE} if the cosine proximity is above threshold and \code{FALSE} otherwise.

}}

\seealso{
	\code{\link{Person}}, \code{\link{Performance}}, \code{\link{proximity}}
}

\author{Fridolin Wild <wild@brookes.ac.uk>}
\references{Fridolin Wild (2016): Learning Analytics in R with SNA, LSA, and MPIA, Springer:Berlin.}

\keyword{mpia}
\keyword{identity}
\keyword{Performance}
\keyword{Person}
