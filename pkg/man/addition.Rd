
\name{+-methods}
\docType{methods}

\alias{+-methods}
\alias{+,Performance,Performance-method}

\title{ Adding performance's meaning vectors. }

\description{

	Add together two meaning vectors, return the resulting meaning vector.

}

\examples{

dmgr = DomainManager()

data(generic.domain)
dmgr$add(d, title="generic")

ppl = HumanResourceManager(dmgr)

fridolin = ppl$add("fridolin")

fridolin$write("this demonstrates skills in business.", label="example")
fridolin$write("shows skills in business")
fridolin$write("risk assessment is another topic")

fridolin[1] + fridolin[2]
terms(fridolin[1] + fridolin[2])

}

\author{Fridolin Wild <wild@brookes.ac.uk>}
\references{Fridolin Wild (2016): Learning Analytics in R with SNA, LSA, and MPIA, Springer:Berlin.}

\keyword{mpia}
\keyword{addition}
