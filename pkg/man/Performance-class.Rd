\name{Performance-class}
\Rdversion{1.1}
\docType{class}

\alias{Performance-class}

\title{Class \code{"Performance"}}
\description{

	The \code{Performance} holds a digital record representing a particular human action: it stores a representation of meaning 
   and an human-readable label for the purpose of the action. The purpose thereby is a character string, whereas the meaning 
   vector is constructed by mapping the descriptive source text to its social semantic vector space representation. 
   
   Several class methods (and generics) support in inspecting the result of such mapping: \code{terms}, for example, returns 
   all terms activated above \code{threshold} in the vector space through the fold in of the sourcetext.
   
   Each \code{Performance} object is bound to a particular \code{Domain}.

}

\section{Fields}{
  \describe{
    \item{\code{name}:}{ Character string holding the human-readable label. }
    \item{\code{sourcetext}:}{ Character string holding the original source text. }
    \item{\code{logging}:}{ Logical flag indicating whether to internally store the source text (or discard it, once the meaning vector is constructed). }
    \item{\code{meaningvector}:}{ Index position of the meaning vector in the Domain's \code{traces} matrix. }
    \item{\code{terms}:}{ The top loading terms (above \code{threshold}). }
    \item{\code{domain}:}{ Reference pointing to the according Domain object. }
    \item{\code{purpose}:}{ Character string indicating the intended purpose of the performance. }
  }
}


\section{Generics}{
  \describe{
    \item{+}{ Add together two meaning vectors, return result vector. }
    \item{==}{ Test two performances for identity (i.e. whether they are close to each other above the proximity threshold). }
    \item{competences}{ Calculate the competence positions underlying the vector with references to performances. In case this is only one performance, it returns the performance. }
    \item{cosine}{ Calculate the cosine between performances. }
    \item{names}{ Return character string with the human-readable label of the performance record. }
    \item{names<-}{ Set the human-readable label of the performance record. }
    \item{near}{ Return cosine closeness value of performances (or performances and persons) tested. }
    \item{overlap}{ Return a vector containing those terms that are shared by all performance objects under investigation. }
    \item{plot}{ Plot a marker indicating the location of the performance on the projection surface created by the map visualisation of the domain.  }
    \item{position}{ Return the competence position held by a vector of performances (calculated as the centroid of the performances). }
    \item{print}{ Pretty printing the performance object. }
    \item{proximity}{ Return \code{TRUE} if the performances in the argument(s) are in proximity to each other (above the proximityThreshold of the domain). }
    \item{show}{ Display the object by printing its key characteristics. }
    \item{summary}{ Describe the performance (top loading terms, name, source text). }
    \item{terms}{ Return a vector of character strings containing the top loading terms of the performance (activated above threshold). }
  }
}

\section{Class Methods}{
  \describe{

    \item{\code{initialize(text, purpose, domain, name, weighting, logging)}:}{ Constructor: requires the sourcetext \code{text}, can optionally set purpose and name. Logging indicates whether the sourcetext will be stored internally. A weighting function can be handed over. }

    \item{\code{getName()}:}{ Get the name of the performance (better use the generic \code{names} instead). }
    \item{\code{setName(value)}:}{ Set the name of the performance (better use the generic \code{names<-} instead). }
    \item{\code{getSourceText()}:}{ Return a character string with the raw source text of the performance. }
    \item{\code{setMeaningVector(vec)}:}{ Add the meaning vector to the domain's \code{trace} matrix field and internally store the index position in field \code{meaningvector}. }
    \item{\code{getActivatedTerms(threshold)}:}{ Return a vector of character strings containing the top loading terms of the performance (activated above threshold). Use the generic \code{terms} instead.  }
    \item{\code{getMeaningVector()}:}{ Fetch the meaning vector from the domain's \code{traces} matrix and return it. }
    \item{\code{getDomain()}:}{ Return reference pointing to the according domain object. }
    \item{\code{getPurpose()}:}{ Return character string with the human-readable purpose the performance aimed at achieving. }
    

    \item{\code{print()}:}{ Pretty printing the performance object (use the generic \code{print} instead). }
    \item{\code{show()}:}{ Display the object by printing its key characteristics (use the generic \code{show} instead). }

  }
}

\examples{

d = Domain(name="test")
evidence = TermDocumentMatrix( Corpus( VectorSource( c("abc abc def", "def ghi", "ghi abc, lmno", "lmno abc wxyz") ) ) )
d$corpus(evidence)
d$spacify()

p = Performance(text="abc def ghi", domain=d, purpose="demo", name="demo")
terms(p)

}

\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\seealso{
	\code{\link{terms}}, \code{\link{overlap}}, \code{\link{near}}, \code{\link{proximity}}, \code{\link{competences}}, 
   \code{\link{names}}, \code{\link{position}}, \code{\link{plot.performance}}, \code{\link{Domain-class}}, \code{\link{Domain}}
}

\keyword{ mpia }
\keyword{ performances }
\keyword{ meaning }
\keyword{ meaning vector }
