
\name{Visualiser}
\alias{Visualiser}

\title{The Visualiser Object}

\description{

	Every \code{Domain} uses a Visualiser to create cartographic plots of the social semantic
	network it holds: the visualized conceptual map can then be used as a projection
	surface to display locations, positions, and pathways of learners or their individual,
	textually represented learning activities.

}

\usage{
Visualiser(domain, ...)
}

\arguments{
  	\item{domain}{ Back reference to the domain the visualiser belongs to. }
  	\item{\dots}{ Additional arguments to be passed on to super. }

}

\details{

	The \code{Visualiser} is responsible for all plotting activities: all other objects interface
   with it to output something onto the plot. The \code{Domain} uses the visualiser to render a
   cartographic map and label the map for its landmarks. The \code{Person} uses the visualiser
   to mark up its position, path, or the map location of individual \code{Performance} objects.

}

\value{

	The constructor 'initialize' returns the object.

}

\examples{

dmgr = DomainManager()
#d = dmgr$get("generic")
data(generic.domain)
dmgr$add(d, title="generic")

summary(d$visualiser)

plot(d)
toponymy(d)

}

\seealso{
	More detailed information: \code{\link{Visualiser-class}},
   \code{\link{Domain}}, \code{\link{DomainManager}}, \code{\link{plot}}, \code{\link{toponymy}}
}

\author{Fridolin Wild <wild@brookes.ac.uk>}
\references{Fridolin Wild (2016): Learning Analytics in R with SNA, LSA, and MPIA, Springer:Berlin.}

\keyword{mpia}
\keyword{Visualiser}
\keyword{visualization}
\keyword{visualisation}
\keyword{map}
\keyword{knowledge cartography}
\keyword{plot}

