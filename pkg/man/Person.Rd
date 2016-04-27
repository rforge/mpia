\name{Person}
\alias{Person}

\title{ Digital Representation of a Learner }

\description{
	
   \code{Person} implements a representation of a learner: persons have names, competences, positions --
   calculated from the activity traces that are stored about them. Such traces are implemented as 
   so-called \code{Performance}.

}

\usage{

	Person(name, domain)

}

\arguments{
  \item{name}{ Character string with the realname of the Person. }
  \item{domain}{ Reference pointing to a \code{Domain} object.	}
}

\details{

	Persons should best be added using the \code{HumanResourceManager}, rather than instantiating
   them directly. This way, the HR manager maintains a record of all persons of interest in the analysis.

}

\value{
	The constructor 'initialize' returns the object.
}

\examples{

dmgr = DomainManager()
data(generic.domain)
dmgr$add(d, title="generic")

ppl = HumanResourceManager(dmgr)

fridolin = ppl$add("fridolin")
fridolin$read("a paper about business management")
fridolin$write("a forum message in the course forum of business management 101")

path(fridolin)

}

\seealso{
   More detailed information: \code{\link{Person-class}},
   \code{\link{Performance}}, \code{\link{position}}, \code{\link{performances}}, \code{\link{competences}}, \code{\link{near}}, \code{\link{names}},
   \code{\link{path}}, \code{\link{plot}}, \code{\link{proximity}}, \code{\link{cosine}}, \code{\link{identity}}
}

\author{Fridolin Wild <wild@brookes.ac.uk>}
\references{Fridolin Wild (2016): Learning Analytics in R with SNA, LSA, and MPIA, Springer:Berlin.}

\keyword{ mpia }
\keyword{ Person }
\keyword{ Performance }
