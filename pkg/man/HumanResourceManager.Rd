
\name{HumanResourceManager}
\alias{HumanResourceManager}

\title{HumanResourceManager}

\description{

	Administer the persons of interest in the MPIA: HR managers act in a particular domain and care for people
   (objects of class \code{\link{Person}}). The HR manager is used to add or remove people, to cluster together
   in groups those persons that share a similar competence profile, and to find persons with experience in 
   a particular area.

}
\usage{

	HumanResourceManager( domainmanager, domain, environment )

}

\arguments{

  \item{domainmanager}{ An object of class \code{\link{DomainManager}}. }
  \item{domain}{ Optional: an object of class \code{\link{Domain}}. If no domain is referenced, the Domain Manager's default domain will be selected (typically 'generic'). }
  \item{environment}{ Optional: a particular environment, default is \code{\link{parent.frame}}. }

}
\details{
	
   Just like in a company, the HumanResourceManager cares for the people working in it: it holds
   pointers to the Person objects, provides an interface for group mining, and helps 
   with identifying experts (with a particular profile).

}
\value{
	The constructor (see 'initialize') returns the DomainManager object.
}

\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\seealso{
	More detailed information: \code{\link{HumanResourceManager-class}},
	\code{\link{environment}},
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

\keyword{ mpia }
\keyword{ competences }
\keyword{ groups }
\keyword{ persons }
\keyword{ person }
\keyword{ people }
\keyword{ profile }
\keyword{ user profile }
