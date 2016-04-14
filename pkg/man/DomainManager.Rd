
\name{DomainManager}
\alias{DomainManager}

\title{
	Administer multiple social semantic spaces.
}

\description{

	The DomainManager is used to organise Domains, i.e. those objects holding
	the corpus, space, traces, and other additional data used in investigating
	learning conversations.

}

\usage{
	DomainManager( domain=NULL )
}

\arguments{
  \item{domain}{Optional: \code{\link[DomainManager-class]{get}} the requested domain immediately into memory.}
}

\value{
	Constructor 'initialize' returns the reference pointing to the DomainManager object.
}

\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\seealso{
	More detailed information: \code{\link{DomainManager-class}},
	\code{\link{Visualiser}}, 
	\code{\link{Domain}}
}

\examples{

dmgr = DomainManager()
d = dmgr$get("generic")
dmgr$upgrade()
dmgr$materialise(d$signature)

}

\keyword{mpia}
\keyword{domain}
\keyword{textmining}
\keyword{spaces}
\keyword{vector space model}
\keyword{vector space}
