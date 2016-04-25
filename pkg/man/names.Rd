
\name{names}
\docType{methods}

\alias{names}
\alias{names-methods}
\alias{names,HumanResourceManager-method}
\alias{names,Performance-method}
\alias{names,Person-method}

\alias{names<-}
\alias{names<--methods}
\alias{names<-,Performance,character-method}
\alias{names<-,Person,character-method}

\title{ The Names of a Performance or Person }

\description{

	\code{names} is a generic accessor function and \code{names<-} is a generic replacement function. 
   
   The default methods can be used to get and set the name of a \code{Performance} and \code{Person}; get additionally retrieves a character vector of the names of all persons cared for, when acting on a \code{HumanResourceManager}.

   \code{names} is a generic accessor function. It returns the field \code{name} of a \code{Performance} or \code{Person}. Can also be executed on a \code{HumanResourceManager} object to retrieve the character vector of the names of all persons.

	\code{names<-} can only be executed over \code{Performance} or \code{Person} objects - to set their name.

}

\usage{

   names(x)

   names(x) <- value

}

\arguments{
  \item{x}{a \code{Person}, \code{HumanResourceManager}, or \code{Performance}.}
  \item{value}{a character vector of the same length as \code{x}.}
}

\value{
	\code{NULL} or character vector of the same length as x. 
}


\examples{

dmgr = DomainManager()

data(generic.domain)
dmgr$add(d, title="generic")
#d = dmgr$get("generic")

ppl = HumanResourceManager(dmgr, d)
fridolin = ppl$add("fridolin")
p = fridolin$write("business management", label="demo")

names(p)
names(p) = "demo-renamed"
names(p)

names(ppl)
names(fridolin)

}

\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\keyword{mpia}
\keyword{Performance}
\keyword{names}
