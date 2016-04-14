
\name{Domain}
\alias{Domain}

\title{Domain}

\description{

	This class encapsulates fields and methods for processing and storing social semantic vector spaces: a given corpus 
   of texts	is analysed to derive an Eigenbasis such that their first Eigenvalues explain 80\% of the total stretch 
   needed to expand its eigenvectors to the mapping provided by the raw document-term matrix (constructed over the input corpus). 
   
   
   Through this approximation, the input corpus is lifted up to a more semantic representation, thus allowing to investigate 
   the nature of the associative closeness relations of its term vectors, document vectors, or any of their combinations 
   used in representing competence positions and performance locations of a \code{Person} or \code{groups}.

}

\usage{
	Domain(name='', ...)
}

\arguments{
	\item{name}{A human-readable name (preferably unique).}
   \item{\dots}{Any additional arguments.}
}

\value{
	Returns the object.
}

\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\seealso{
	More detailed information: \code{\link{Domain-class}}, 
	\code{\link{plot}},
   \code{\link{toponymy}},
	\code{\link{Performance}},
   \code{\link{Person}},
   \code{\link{Visualiser}}
}
\examples{

	d = Domain(name="test")
	evidence = VectorSource( c("abc abc def", "def def ghi", "ghi ijk, lmno", "pqrs tuv wxyz") )
	d$corpus(evidence)
	d$spacify()

}

\keyword{ mpia }
\keyword{ domain }
