
\name{[}
\docType{methods}

\alias{[}
\alias{[-methods}
\alias{[,Person,ANY,ANY,ANY-method}

\title{ Accessor for a Person's Performances }
\description{
	\code{[} provides a quick way to access a \code{Performance}s.

}
\section{Methods}{
\describe{

	Returns the performance at a given index position.

}}


\examples{

dmgr = DomainManager()
ppl = HumanResourceManager(dmgr)

fridolin = ppl$add("fridolin")

fridolin$write("this demonstrates skills in business.", label="example")
fridolin$write("shows skills in business")
fridolin$write("risk assessment is another topic")

fridolin[1]

}

\keyword{mpia}
\keyword{Performance}
