
\name{path}
\docType{methods}

\alias{path}
\alias{path-methods}
\alias{path,Person-method}

\title{ The Learning Path }
\description{

	\code{path} returns a vector of references pointing to the chronologically ordered \code{Performance} 
   objects, the person of interest has completed.

}

\usage{

   path( x, ix=NULL )

   path( x, from=NULL, to=Sys.time() )

}

\arguments{

  \item{x}{a \code{Person} object. }


  \item{ix}{optional: numeric vector with the index positions of the performances to be returned. Using this, \code{path} will reshuffle the performances in the desired order. }
  

  \item{from}{character string containing a valid start date with time in the format "\%Y-\%m-\%d \%H:\%M:\%S". All performances before the from timestamp will be ignored. }
  \item{to}{optional: character string containing a valid end date with time in the format "\%Y-\%m-\%d \%H:\%M:\%S". If \code{to} is missing, the current date and time will be used. All performances after the to timestamp will be ignored. }

}

\value{

	\code{NULL} or vector with references pointing to the matching \code{Performance} objects, sorted ascending by date (from oldest to latest).

}

\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\keyword{mpia}
\keyword{path}
\keyword{learning path}
