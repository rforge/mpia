
\name{near}
\docType{methods}

\alias{near}
\alias{near-methods}
\alias{near,HumanResourceManager,Performance-method}
\alias{near,HumanResourceManager,Person-method}
\alias{near,Performance,Performance-method}
\alias{near,Performance,Person-method}
\alias{near,Person,Performance-method}
\alias{near,Person,Person-method}

\title{ Who or What is Near? }
\description{

  Determine whether positions of persons or their textual activity (aka 'performances') are in close proximity.

  This set of methods test whether vector locations in the conceptual space (of given domain) are close to each other:
  to be close, the cosine between the vectors has to be above the proximity threshold (see \code{Domain$proximityThreshold}).

  The meaning vectors of individual 'performances' such as readings and writings have a location in the space and so do the
  positions of persons (calculated via the centroid of the locations of all their performances).
  
}

\section{Methods}{
\describe{

   \item{\code{signature(object = "HumanResourceManager", to = "Performance")}}{
   Test whether any person has a position close to the location of a textual activity (aka 'performance'). Returns a vector of persons that are in close proximity (or \code{NULL} otherwise).
   }

   \item{\code{signature(object = "HumanResourceManager", to = "Person")}}{
   Test whether any person has a position close to the location of the person handed over in 'to'. Returns a vector of persons that are in close proximity (or \code{NULL} otherwise).
   }

   \item{\code{signature(object = "Performance", to = "Performance")}}{
	Test wether the locations of two textual activities are location-wise close to each other. Returns \code{TRUE} if this is the case and \code{FALSE} otherwise.
   }

   \item{\code{signature(object = "Performance", to = "Person")}}{
   Test whether the location of a textual activity in the multidimensional conceptual space is close to the position of a given person 'to'. Returns \code{TRUE} if this is the case and \code{FALSE} otherwise.
   }

   \item{\code{signature(object = "Person", to = "Performance")}}{
   Test whether the position of a person is close to the location of a textual activity 'to'. Returns \code{TRUE} if this is the case and \code{FALSE} otherwise.
   }

   \item{\code{signature(object = "Person", to = "Person")}}{
   Test whether the positions of two persons are close. Returns \code{TRUE} if this is the case and \code{FALSE} otherwise.
   }

}}

\author{Fridolin Wild <wild@brookes.ac.uk>}
\references{Fridolin Wild (2016): Learning Analytics in R with SNA, LSA, and MPIA, Springer:Berlin.}

\keyword{mpia}
\keyword{near, proximity, closeness, close, analyse, analyze}
