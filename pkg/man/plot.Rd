
\name{plot-methods}
\docType{methods}

\alias{plot}
\alias{plot-methods}
\alias{plot,Domain,ANY-method}
\alias{plot,Performance,ANY-method}
\alias{plot,Person,ANY-method}

\title{ Plot Domains, Performances, and Persons }

\description{

   \code{plot} creates the visualisation. First, a domain object has to be plotted (or at least it's toponymy),
   as this will render the visualisation data. Alternatively, the domain's visualiser can be used to calculate,
   but not plot the map data, which is required internally to project persons, performances, or paths of performances.
   
   Once the domain cartographic visualisation is calculated (and rendered on the display), persons, performances,
   and paths can be added to the plot.

}

\usage{

   plot(x, ...)

}

\arguments{

  \item{x}{a \code{Person}, \code{Performance}, or \code{Domain} object. }
  \item{...}{graphic parameters (for details, see plot method of each class).}

}


\section{Methods}{
\describe{

   For \code{Domain}: A perspective or plain map visualisation is plotted. It is recommended to add \code{toponymy} to the map.


   For \code{Performance}: For a single performance (or a vector of performances), the location of the performance(s) is marked up in the plot.


   For \code{Person}: The competence position of the person is calculated and marked up on the plot.

}}

\seealso{
   \code{\link{Visualiser}} and \code{\link{Visualiser-class}} for more details on how to configure the map and marker display.
}

\author{Fridolin Wild <wild@brookes.ac.uk>}
\references{Fridolin Wild (2016): Learning Analytics in R with SNA, LSA, and MPIA, Springer:Berlin.}

\keyword{mpia}
\keyword{plot}
\keyword{visualisation}
