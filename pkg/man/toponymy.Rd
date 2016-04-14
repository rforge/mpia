
\name{toponymy}
\docType{methods}

\alias{toponymy}
\alias{toponymy-methods}
\alias{toponymy,Domain-method}

\title{ Find landmarks in the map and label them }
\description{

	Find interesting places on the map and label them. This generic calls the visualisers \code{toponymy} method
   to first identify potential landmarks on the map (such as labels for the mountains) and then plot them into the
   map visualisation.

}


\usage{

   toponymy(x, ...)

}

\arguments{

  \item{x}{ A \code{Domain} object. }
  \item{\dots}{ Additional graphical parameters passed on to the domain's visualiser object. }

}

\seealso{
   \code{\link{Visualiser}} and \code{\link{Visualiser-class}} for more details on how to configure the map and marker display.
}


\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\keyword{mpia}
\keyword{plot}
\keyword{visualisation}
