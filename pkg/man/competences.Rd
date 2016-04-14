
\name{competences}
\docType{methods}

\alias{competences}
\alias{competences-methods}
\alias{competences,HumanResourceManager-method}
\alias{competences,Performance-method}
\alias{competences,Person-method}

\title{ Calculate Competence Positions }

\description{

   \code{competences} calculates a single or a set of meaning vectors representing the competence positions occupied by the 
   given performances in the social semantic space. Multiple performances can be positioned in the space in such close vicinity that they occupy
   a single competence position (with their mutual associative closeness being above the identity threshold).
   
	The given collection of performances is analysed using agglomerative clustering (provided by \code{agnes}), thereby
	cutting the resulting cluster dendrogram at the \code{identityThreshold}. 
   
   Returns a reference to a single object or a vector of references pointing to multiple objects of type \code{\link{Performance}}.

}

\usage{
competences(x)
}

\arguments{
\item{x}{a vector of references to \code{Performance} objects.}
}

\value{
	One or more competence positions (of type object \code{Performance}).
}

\section{Methods}{
\describe{

	\item{\code{signature(x = "HumanResourceManager")}}{
		Collects all performances of all persons managed by the HumanResourceManager (field \code{people}) and subsequently calculates 
      their underlying competence positions (using the \code{competences} generic of the performance list object). Returns a list of \code{Performance} positions.
	}

	\item{\code{signature(x = "Person")}}{
   	Calculates the competence positions underyling the performances of a given person using the \code{competences} generic of the performance list object. Returns a list of \code{Performance} positions.
	}

	\item{\code{signature(x = "Performance")}}{
		Calculates the competence positions underlying a given list of performances. If the argument is a single performance, the performance itself is returned.
	}

}}

\examples{

dmgr = DomainManager()
ppl = HumanResourceManager(dmgr)

fridolin = ppl$add("fridolin")
fridolin$write("this demonstrates skills in business.", label="example")
terms(fridolin)

max = ppl$add("maximilian")
max$write("shows skills in business")

jo = ppl$add("jo")
jo$write("risk assessment is another topic")

competences(ppl)

}

\seealso{
	\code{agnes}, \code{cutree}, \code{identityThreshold}
}

\keyword{mpia}
\keyword{competence}
\keyword{competences}
\keyword{competencies}
\keyword{competency}
