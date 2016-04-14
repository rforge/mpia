
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
ppl = HumanResourceManager(dmgr)

fridolin = ppl$add("fridolin")

fridolin$write("this demonstrates skills in business.", label="example")
fridolin$write("shows skills in business")
fridolin$write("risk assessment is another topic")

fridolin[1] + fridolin[2]
terms(fridolin[1] + fridolin[2])

}

\keyword{mpia}
\keyword{addition}
