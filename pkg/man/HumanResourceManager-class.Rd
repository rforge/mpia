
\name{HumanResourceManager-class}
\Rdversion{1.1}
\docType{class}

\alias{HumanResourceManager-class}

\title{Class \code{"HumanResourceManager"}}

\description{

	Administer the persons of interest in the MPIA: HR managers act in a particular domain and care for people
   (references to objects of class \code{\link{Person}}). The HR manager is used to add or remove people, to cluster together
   in groups those persons that share a similar competence profile, and to find persons with experience in 
   a particular area.

}

\section{Fields}{
  \describe{
  
    \item{\code{envir}:}{ Internally used: the environment of the HR manager. }
    \item{\code{people}:}{ A vector of references pointing to the individuals' Person objects. }
    \item{\code{groups}:}{ A vector containing reference pointing to a group of persons (itself being an object of class Person). }
    \item{\code{domains}:}{ Reference pointing to the DomainManager object. }
    \item{\code{currentDomain}:}{ Reference pointing to the current Domain object. }

  }
}


\section{Generics}{
  \describe{

    \item{competences}{\code{signature(x = "HumanResourceManager")}: Calculate competence positions amongst all performances of all persons. }
    \item{groups}{\code{signature(x = "HumanResourceManager")}: Identify groups of persons that have a position closer to each other than the minimal identity threshold. }
    \item{names}{\code{signature(x = "HumanResourceManager")}: List the names of all persons this HumanResourceManager cares for. }
    \item{near}{\code{signature(object = "HumanResourceManager", to = "Performance")}: Identify all persons that are competent in the area of the given performance. }
    \item{near}{\code{signature(object = "HumanResourceManager", to = "Person")}: Identify all persons with a similar competence profile as the given person. }
    \item{performances}{\code{signature(x = "HumanResourceManager")}: Return the list of all performances of all persons. }
    \item{proximity}{\code{signature(a = "HumanResourceManager", b = "missing")}: Returns the pairwise proximity of all persons to each other (in matrix form). }

  }
}

\section{Class Methods}{

  \describe{

    \item{\code{initialize(domainmanager, domain, environment)}:}{ Constructor: requires a DomainManager to be handed over. }

    \item{\code{findGroups()}:}{ Internally used to identify groups with similar competence positions. }
    \item{\code{flushPositions(domain)}:}{ Calculating positions creates temporary traces in the domain; if memory is a scarce resource, flushing these traces every once in a while may be helpful. }

    \item{\code{add(name, domain)}:}{ Create a new \code{\link{Person}} object and add it to the list of people.}
    \item{\code{remove(id, name)}:}{ Remove a person. }

    \item{\code{collect(varname, name, environment)}:}{ Legacy routine to collect stray person objects NOT created through the HumanResourceManager. }

    \item{\code{last()}:}{ Return reference pointing to the last person added. }
    \item{\code{all()}:}{ Return a vector of references pointing to all person objects cared for. }
    \item{\code{getPersonByName(name)}:}{ Return reference pointing to the person object of the person with the name 'name'. }
    \item{\code{ls(environment)}:}{ Return a character vector containing the names of all persons. }

    \item{\code{print()}:}{ Pretty print basic data about the personnel cared for. }
    \item{\code{show()}:}{ Display the object by printing its basic stats. }
    
  }

}

\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\seealso{
	\code{\link{Person}},
	\code{\link{Domain}}
}

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

ppl$ls()
terms( performances(ppl) )
groups(ppl)
competences(ppl)

}


\keyword{classes}
