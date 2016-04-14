
\name{proximity}
\docType{methods}

\alias{proximity}
\alias{proximity-methods}
\alias{proximity,HumanResourceManager,missing-method}
\alias{proximity,Performance,missing-method}
\alias{proximity,Performance,Performance-method}
\alias{proximity,Performance,Person-method}
\alias{proximity,Person,Person-method}

\title{ Proximity: Measure Associative Closeness }

\description{
	
   Calculates the cosine closeness and returns its value.

}


\usage{

   proximity(a, b, method="cosine")

}

\arguments{

  \item{a}{ Object of class \code{Person}, \code{Performance}, or \code{HumanResourceManager}. }
  \item{b}{ Object of class \code{Person}, \code{Performance}, or missing. }
  \item{method}{ Currently only "cosine" is supported. }

}


\section{Methods}{
\describe{

   \item{\code{signature(a = "HumanResourceManager", b = "missing")}}{ Return proximity values as matrix for all persons cared for in the HR manager. }

   \item{\code{signature(a = "Performance", b = "missing")}}{ If a vector of performances, then return the proximity matrix; if a single Performane, return 1. }

   \item{\code{signature(a = "Performance", b = "Performance")}}{ Return the proximity value between the two performances. }

   \item{\code{signature(a = "Performance", b = "Person")}}{ Return proximity values of the performance and the person. }

   \item{\code{signature(a = "Person", b = "Person")}}{ Return proximity value between the two persons. }

}}


\seealso{
   \code{\link{cosine}}
}

\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\keyword{mpia}
\keyword{proximity}

