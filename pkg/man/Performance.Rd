\name{Performance}

\alias{Performance}

\title{Record of an Activity Trace}

\description{

	The \code{Performance} holds a digital record representing a particular human action: it stores a representation of meaning 
   and an human-readable label for the purpose of the action. The purpose thereby is a character string, whereas the meaning 
   vector is constructed by mapping the descriptive source text to its social semantic vector space representation. 
   
   Several class methods (and generics) support in inspecting the result of such mapping: \code{terms}, for example, returns 
   all terms activated above \code{threshold} in the vector space through the fold in of the source text.
   
   Each \code{Performance} object is bound to a particular \code{Domain}.

}

\usage{
	Performance( text=NULL, purpose=NULL, domain=NULL, name=NULL, weighting=NULL, logging=TRUE )
}

\arguments{

	\item{text}{ Character string containg the text written or read (or the descriptive text for any other sorts of actions). }

	\item{purpose}{ Character string describing the purpose of the action, e.g. "course 101 exam". }
   
   \item{domain}{ Reference pointing to a \code{Domain} object. }
   
   \item{name}{ Character string labelling the performance (e.g. "Essay for business management exam"). }

   \item{weighting}{ A weighting function to be executed on the raw query vector (before projection into the Eigenspace). }

   \item{logging}{ Logical flag indicating whether the source text shall be internally stored in field \code{sourcetext}.  }

}

\details{

	Any \code{Performance} is associated to a particular \code{Domain}. It is constructed from a given source \code{text}
   (which may or may not be internally stored, depending on the setting for \code{logging}) by mapping the text to its
   social semantic vector space representation. This projection uses the \code{fold-in} facilities provided by the 
   \code{Domain} object. For efficiency reasons, the projection appends the meaning vector in the domain object in
   the \code{traces} matrix and returns its index position to be held by the performance record.

}

\value{

	The constructor 'initialize' returns the reference pointing to the object.

}

\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\section{Warning}{ Using weighting functions is complicated! }

\seealso{
	More detailed information: \code{\link{Performance-class}},
   \code{\link{terms}}, \code{\link{performances}}, \code{\link{overlap}}, \code{\link{plot}}, \code{\link{Domain-class}}, \code{\link{Domain}}
}

\examples{

d = Domain(name="test")
evidence = TermDocumentMatrix( Corpus( VectorSource( c("abc abc def",
    "def ghi", "ghi abc, lmno", "lmno abc wxyz") ) ) )
d$corpus(evidence)
d$spacify()

p = Performance(text="abc def ghi", domain=d, purpose="demo", name="demo")
terms(p)


}

\keyword{ mpia }
\keyword{ Performance }
\keyword{ Meaning }
\keyword{ Purpose }