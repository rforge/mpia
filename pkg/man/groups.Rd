
\name{groups}
\docType{methods}

\alias{groups}
\alias{groups-methods}
\alias{groups,HumanResourceManager-method}

\title{ Identify groups of persons with identical competence positions }

\description{

 	This method uses the agglomerative clustering provided by \code{\link[cluster:agnes]{agnes}} to find
   those sets of persons that occupy a competence position in close vicinity to each other.
   The proximity thereby has to be above the domain's identity threshold to be clustered together.

	Set \code{\link[Domain-class]{identityThreshold}} to relax (or raise)
	the threshold above which identity is assumed.

}

\usage{
groups(x, ...)
}

\arguments{
  \item{x}{a \code{HumanResourceManager}.}
  \item{\dots}{additional parameters to be passed on to \code{findGroups}.}
}

\value{
	References to freshly created \code{Person} objects holding the group cluster centroids.
}

\examples{

dmgr = DomainManager()
data(generic.domain)
dmgr$add(d, title="generic")

ppl = HumanResourceManager(dmgr)

fridolin = ppl$add("fridolin")
fridolin$write("this demonstrates skills in business.", label="example")
terms(fridolin)

max = ppl$add("maximilian")
max$write("shows skills in business")

jo = ppl$add("jo")
jo$write("risk assessment is another topic")

groups(ppl)

}

\seealso{
	More detailed information: \code{\link[HumanResourceManager-class]{findGroups}},
	\code{\link{Visualiser}}, \code{\link{Domain}}
}

\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\keyword{mpia}
\keyword{groups}
\keyword{clustering}
\keyword{competence position}
