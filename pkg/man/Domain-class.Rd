\name{Domain-class}
\Rdversion{1.1}
\docType{class}

\alias{Domain-class}

\title{Class \code{"Domain"}}

\description{

	This class encapsulates fields and methods for processing and storing social semantic vector spaces: a given corpus
	of texts	is analysed to derive an Eigenbasis such that their first Eigenvalues explain 80\% of the total stretch
	needed to expand its eigenvectors to the mapping provided by the raw document-term matrix (constructed over the input corpus).


	Through this approximation, the input corpus is lifted up to a more semantic representation, thus allowing to investigate
	the nature of the associative closeness relations of its term vectors, document vectors, or any of their combinations
	used in representing competence positions and performance locations of a \code{Person} or \code{groups}.

}

\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\section{Fields}{

  \describe{

      \item{\code{name}:}{ The name of the domain (character string) }
	  \item{\code{mode}:}{ Mode of analysis: currently only 'terminology' is supported, though other views are theoretically plausible
			(such as focusing on the 'incidences' provided as documents or 'both'). }


	  \item{\code{textmatrix}:}{ The TermDocumentMatrix holding the raw data from which a mpia space is constructed with \code{spacify}. }
      \item{\code{space}:}{ The Eigensystem: holds the three truncated matrices resulting from the singular value decomposition. }
	  \item{\code{processed}:}{ Logical: denotes whether the \code{space} was already calculated from \code{textmatrix}.}
	  \item{\code{signature}:}{ A unique identifier of the domain (hash value of the space variable); automatically given by the \code{spacify} method. }


      \item{\code{traces}:}{ Internally used to store temporary fold-in data (of positions). }


      \item{\code{termProximities}:}{ The symmetric matrix of cosine proximities for all term pairs. }
	  \item{\code{proximityThreshold}:}{ The threshold for associative closness to be considered \code{near}. }
	  \item{\code{identityThreshold}:}{ The threshold for associative closness to be considered 'same'. }


      \item{\code{visualiser}:}{ The visualiser object. }
      \item{\code{version}:}{ The version number of the Domain-class. }

  }

}

\section{Generics}{
	\describe{
		\item{\code{plot}}{\code{signature(x = "Domain", y = "ANY")}: Visualise the projection surface of the domain as plain or perspective plot. }
		\item{\code{toponymy}}{\code{signature(x = "Domain")}: Analyse the places in the visualisation and label landmarks accordingly. }
		\item{\code{summary}}{\code{signature(x = "Domain")}: Print basic descriptive statistics about the data held. }
	}
}

\section{Class Methods}{
  \describe{

	 \item{\code{initialize(name, ...)}:}{ Constructor; name should preferably be a unique identifier. }

\item{\code{calculateTermProximities(mode, normalise, mincomp)}:}{ Determine the associative closeness of all term pairs in a given domain, defined as their cosine proximity in the Eigenspace. If normalise is set to \code{TRUE} (default is false), the cosine values are normalised according to their frequency distribution (interval-scaled from min to max). The parameter mincomp is per default set to the (ceiling of the) square root of the number of rows of the term-loadings in the mpia space \emph{Tk}. If any component of the graph has less members than mincomp, its node with the highest betweenness is reattached to its closest node (with the original term proximity value as edge weight). This means that the number of stray isolates and stray isolate components is reduced via loose attachment below the proximity threshold.}

    \item{\code{spacify()}:}{ Determine optimal number of dimensions for the conceptual space and convert the source vectors to a \code{space} in its Eigenbasis. }
	 \item{\code{corpus(x)}:}{ Create a document-term matrix from corpus \code{x} (either a list of files or directory, a \code{\link[tm:Source]{Source}} object, or a \code{\link[tm:TermDocumentMatrix]{TermDocumentMatrix}}). Store it internally in field \code{textmatrix}. }

	 \item{\code{addTrace(vecs)}:}{ User interface for adding query document-term matrix vectors using fold-ins: project new texts into an existing Eigenspace. }

\item{\code{fold_in(docvecs)}:}{ Internally used fold-in routine: returns a context vector appendable to the right singular Eigenvectors (\emph{not} a document-term matrix vector such as provided by \code{\link[lsa:fold_in]{fold_in}}). }

    \item{\code{getVocabulary()}:}{ Returns the list of terms used in the conceptual vector space. }
    \item{\code{getName()}:}{ Returns the (manually assigned) label of the domain. }
    \item{\code{getSpace()}:}{ Returns the space object. }
    \item{\code{setSpace(x)}:}{ Set the space object (an \code{LSAspace}). }

	 \item{\code{submit()}:}{ Not yet implemented: hook for remoting via \link{http://cRunch.kmi.open.ac.uk}. }

    \item{\code{print()}:}{ Pretty printing of the domain object. }
	 \item{\code{show()}:}{ Display the object by printing its key characteristics. }

    \item{\code{copy(shallow)}:}{ Internal routines required for upgrading Domain objects to newer versions of the \code{Domain-class}. }

  }
}

\examples{

	d = Domain(name="test")
	evidence = TermDocumentMatrix( Corpus( VectorSource( c("abc abc def", "def def ghi", "ghi ijk, lmno", "pqrs tuv wxyz") ) ) )
	d$corpus(evidence)
	d$spacify()

}

\seealso{
\code{\link[lsa]{lsa}}, \code{\link[tm]{tm}}, \code{\link[lsa:textmatrix]{textmatrix}}
}

\keyword{mpia}
\keyword{corpus}
\keyword{corpora}
\keyword{lsa}
\keyword{spacify}
\keyword{spacification}
\keyword{truncation}
