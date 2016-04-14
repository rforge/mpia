\name{position}
\docType{methods}

\alias{position}
\alias{position-methods}
\alias{position,Performance-method}
\alias{position,Person-method}

\title{ The Position of a Person}

\description{

   \code{position} calculates the centroid meaning vector representing the current or specified competence position occupied by the
   person through the person's performances in the social semantic space.

}

\usage{

   position(x)

}

\arguments{

  \item{x}{Either a single or vector of \code{Person} objects, or a single or vector of \code{Performance} objects. }

}

\section{Methods}{
\describe{

	\item{\code{Performance}}{ Returns the position occupied by the single or vector of performance(s). }

	\item{for \code{Person}}{ Returns the position occupied by the person(s). }
   
}}

\seealso{
   \code{\link{competences}}, \code{\link{performances}}, \code{\link{Performance-class}}, \code{\link{Person-class}}.
}


\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\keyword{mpia}
\keyword{position}
\keyword{competence}
