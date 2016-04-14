\name{DomainManager-class}
\Rdversion{1.1}
\docType{class}

\alias{DomainManager-class}

\title{Class \code{"DomainManager"}}

\description{

	Administer the domains held: provides methods to add, retrieve, upgrade, materialise, and remove 
   domain objects. Some of the calculation processes involved in creating social semantic network representations 
   of learning are computationally rather costly. The DomainManager supports efficient management of domains: it 
   provides methods to materialise to the cache directory on the local hard disk and it supports the 
   user in finding, loading, and upgrading domain objects (and their visualisers) to new class releases.

}

\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\seealso{
	\code{\link{DomainManager}}, 
	\code{\link{Visualiser}}, 
	\code{\link{Domain}}
}

\examples{

dmgr = DomainManager()
d = dmgr$get("generic")
dmgr$upgrade()
dmgr$materialise(d$signature)

}

\section{Fields}{
  \describe{
    \item{\code{domains}:}{ The domain objects currently held \emph{in memory}. }
    \item{\code{signatures}:}{ The unique identifiers of the domain objects held in memory. }
    \item{\code{tempdir}:}{ The path to the cache directory. }
    \item{\code{caching}:}{ Logical: flag whether caching should be enabled (\code{TRUE}) or not (\code{FALSE}). }
    \item{\code{remoting}:}{ Logical: flag wether retrieval from and processing on cRunch should be enabled; not yet implemented. }
  }
}

\section{Class Methods}{
  \describe{
    \item{\code{initialize(domain)}:}{  }

    \item{\code{add(x, title)}:}{ Create a new domain object and add it to the list of domains available in memory. }
    \item{\code{retrieve(id, name)}:}{ Load a materialised domain object from the local cache directory. }
    \item{\code{get(name, id)}:}{ Return the reference pointer to the domain object requested, load it from the cache directory if needed. }
    \item{\code{materialise(id)}:}{ Save the domain object to local cache directory. }
    \item{\code{status(id)}:}{ Check for availability of the domain with signature 'id'. }

    \item{\code{flush(domain)}:}{ Remove all (or specified) domain objects from memory. }
    \item{\code{flushCache()}:}{ Delete all domain object cache files from the local cache directory. }
    \item{\code{upgrade(force)}:}{ Update all domain objects currently in memory to the latest class code version. }

    \item{\code{last()}:}{ Return the reference pointer to the last domain added. }
    \item{\code{ls()}:}{ Return the names of all domains currently in memory. }
    \item{\code{all()}:}{ Return reference pointers to all domains currently in memory. }

    \item{\code{print()}:}{ Pretty printing: listing the number of domains in memory. }
    \item{\code{show()}:}{ Display the object by printing its key characteristics. }

  }
}

\keyword{mpia}
\keyword{domain}
\keyword{textmining}
\keyword{spaces}
\keyword{vector space model}
\keyword{vector space}
