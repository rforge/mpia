
\name{Person-class}
\Rdversion{1.1}
\docType{class}

\alias{Person-class}

\title{Class \code{"Person"}}

\description{
	
   
   \code{Person} implements a representation of a learner: persons have names, competences, positions --
   calculated from the activity traces that are stored about them. Such traces are implemented as 
   so-called \code{Performance}.

}

\section{Fields}{
  \describe{
    \item{\code{name}:}{ The realname of the person (character string). }
    \item{\code{performances}:}{ Vector with references pointing to the person's performance objects. }
    \item{\code{positions}:}{ Vector temporarily holding references pointing to positions calculated from selections or all of the performances. May be cleared any time.  }
    \item{\code{activityType}:}{ Character string describing the type of activity (e.g. 'read' or 'write').  }
    \item{\code{scores}:}{ Double holding a grade of the performance (if available) or \code{NULL} if not.  }
    \item{\code{timestamps}:}{ Vector holding timestamps for each performance about when it was enacted (character string). }
    \item{\code{labels}:}{ Vector holding character strings with labels of each performance (e.g. 'text written for exam'). }
    \item{\code{currentDomain}:}{ Reference pointing to a domain object.  }
    \item{\code{logging}:}{ Logical flag indicating on whether the person's performance objects shall internally store the raw source text. }
  }
}

\section{Generics}{
  \describe{

    \item{names}{ Return character string with the realname of the person. }
    \item{names<-}{ Set realname of the person (to given character string). }


    \item{[}{ Accessor to the performances (e.g. \code{fridolin[1]} returns the first performance). }
    \item{performances}{ Return vector with references pointing to the person's performances. }
    \item{path}{ Return vector with references pointing to the person's performances, sorted chronologically or by given index positions. }
    \item{terms}{ Return list with one entry for each performance containg the terms (as a vector with the character strings of the terms). }


    \item{competences}{ Calculate competences held by the person (clustering those performance together that belong together). }
    \item{position}{ Calculate the compence position held by the person. }


    \item{near}{ Return those persons cared for by a given \code{HumanResoureManager} that are in proximity to this person. }
    \item{proximity}{ Return cosine proximity value of the person's competence position to another person's position or a performance. }
    \item{==}{ Test for identity: return \code{TRUE} if two persons are identical (i.e. close above the domain's identity threshold). }
    \item{cosine}{ Return the cosine value between the person object and other given objects. }


    \item{plot}{ Plot a marker for the person's current competence position. }


    \item{print}{ Pretty printing of the object. }
    \item{show}{ Display the object by printing its key characteristics. }
    \item{summary}{ Print basic descriptive statistics about the data held. }

  }
}

\section{Class Methods}{
  \describe{

    \item{\code{initialize(name, domain)}:}{ Constructor: create a new Person object with given \code{name} and in a given \code{Domain} }

    \item{\code{perform(txt, purpose, activity, score, when, label)}:}{ This method is called from \code{read} and \code{write} to instantiate a \code{Performance} object and fill it with the data handed over. }
    \item{\code{write(txt, purpose, score, when, label)}:}{ This method is used when learners write a text. It instantiates a \code{Performance} object and fill it with data (calling \code{perform}). The parameters contain the raw text (\code{txt}), a human-readable character string for its \code{purpose}, if available a score, a character string \code{when} holding the timestamp, and a human-readable \code{label} to be used when displaying the performance in visualisations. If no label is handed over, the method constructs one. If no timestamp is handed over, it picks the current date and time via \code{Sys.time()}. } 
    \item{\code{read(txt, performance, purpose, when, label)}:}{ This method is used when learners read a text. It instantiates a \code{Performance} object and fill it with data (calling \code{perform}). The parameters contain the raw text (\code{txt}), a human-readable character string for its \code{purpose}, a character string \code{when} holding the timestamp, and a human-readable \code{label} to be used when displaying the performance in visualisations. If no label is handed over, the method constructs one. If no timestamp is handed over, it picks the current date and time via \code{Sys.time()}. }


    \item{\code{path(ix, from, to)}:}{ Return a vector with references pointing to the persons \code{Performance} objects, ordered chronologically or by index \code{ix}, possibly restricted to the interval \code{from} to \code{to}. }
    \item{\code{position(when)}:}{ Return a newly instantiated \code{Performance} object for a given time and date via \code{when} or up to the current date and time. }
    \item{\code{lastPosition()}:}{ Return the latest competence position occupied by the person. If no position was previously calculated, the current position is calculated and returned. This method thus caches performance positions. }

    \item{\code{setName(value)}:}{ Set the realname of the person (use the generic \code{names<-} instead). }
    \item{\code{getName()}:}{ Get the realname of the person (use the generic \code{names} instead). }
    \item{\code{getActivatedTerms(ix)}:}{ Returns a list holding the names of the top activated terms in a vector for each performance. }
    \item{\code{getMeaningVectors(ix)}:}{ Returns a matrix with the performances' meaning vectors in the rows. }
    \item{\code{getSourceTexts()}:}{ Return a vector of character strings containing the source texts. }
    \item{\code{setSourceLogging(x)}:}{ Change the setting for source logging: \code{TRUE} will make all future performances internally store the raw source text (and not only the meaning vectors). }
    \item{\code{getPurposes()}:}{ Return a vector of character strings containing the purpose labels. }
    \item{\code{getDomains()}:}{ Return a vector of references pointing to the domain each performance resides in. }
    \item{\code{setCurrentDomain(dom)}:}{ Return the current domain the person is active in, that is used by all future \code{performance}, \code{read}, and \code{write} acts. }
    

    \item{\code{print()}:}{ Pretty printing the object (use the generic \code{print} instead). }
    \item{\code{show()}:}{ Display the object by printing its key characteristics (use the generic \code{show} instead). }
  }
}

\examples{

dmgr = DomainManager()
data(generic.domain)
dmgr$add(d, title="generic")

ppl = HumanResourceManager(dmgr)

fridolin = ppl$add("fridolin")
fridolin$read("a paper about business management")
fridolin$write("a forum message in the course forum of business management 101")

path(fridolin)

}

\seealso{
   \code{\link{Performance}}, \code{\link{position}}, \code{\link{performances}}, \code{\link{competences}}, \code{\link{near}}, \code{\link{names}}, 
   \code{\link{path}}, \code{\link{plot}}, \code{\link{proximity}}, \code{\link{cosine}}, \code{\link{identity}}
}

\author{Fridolin Wild <wild@brookes.ac.uk>}
\references{Fridolin Wild (2016): Learning Analytics in R with SNA, LSA, and MPIA, Springer:Berlin.}

\keyword{ mpia }
\keyword{ Person }
\keyword{ Performance }
