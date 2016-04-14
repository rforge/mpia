\name{mpia-package}
\alias{mpia-package}
\alias{mpia}
\docType{package}

\title{Meaningful Purposive Interaction Analysis (MPIA)}

\description{


	'Learning' is building competence through (targeted) conversation between an aspirant
   and a typically more experienced other. Such conversation, -- particularly when born
   digital in form of messages, essays, articles, or books available online --, can be subjected 
   to analysis, looking into both developed conceptual structures as well as the social 
   interaction leading to them.


   Meaningful Purposive Interaction Analysis (MPIA) was developed to facilitate the analysis
   of social semantic relations. It uses conceptual spaces built from the conversational content 
   collected from networked learning activity to represent persons and their competence demonstrated.


   MPIA is dervied from the same mathematical foundation as latent semantic analysis and (social)
   network analysis, sharing their advantages, while at the same time overcoming some of their shortcomings.

}
\details{

	The package provides routines to construct and investigate representations of social semantic performance networks.
   

   The community knowledge expressed digitally in a larger amount of unstructured text (such as a collection of messages, 
   essays, articles, or books) can be used to derive its specific 'conceptual graph' of (both generic as well as professional 
   language) terms and their associative proximity relations. 
  

   Such conceptual graph provides a projection surface on which it is possible to investigate
   social as well as semantic relations of persons, their performance, and the underlying competence.
   
   
   The package additionally provides visual analytics to support the analyst.

}
\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\seealso{
	\pkg{\link{lsa}}, \pkg{\link{sna}}, \pkg{\link{network}}
}

\examples{

# load domain from cache
dmgr=DomainManager()
d=dmgr$get("generic")

# visualise terminology map
plot(d)
toponymy(d)

# the virtual HR manager
ppl = HumanResourceManager(dmgr)

# add a learner
fridolin = ppl$add(name="fridolin")
fridolin$write("risk management is crucial for preventing a business from failure.", label="risk")
fridolin$write("shareholders can maximize their profits, if the price level is increased:
   most shareholders appreciate such performance management oriented approach to business",
   label="profits")
fridolin$write("in management, it is the role of the ceo in business to appreciate 
   the contributions of the panel members and to balance risk", label="control")
fridolin$write("donor development in fundraising is all about ensuring that you and 
   your donors get the most you can from your relationship in ways which are mutually agreeable
   and beneficial", label="donors")
terms(fridolin)
terms(position(fridolin))

# visualise
plot(position(fridolin), col="red")
plot(path(fridolin), col="red")

# add a second learner
max = ppl$add(name="maximilian")
max$write("advertising includes the activity in the media such as running newspaper 
   ads, direct marketing, cold calling", label="advertising")
max$write("marketing management, however, is a wide field: this basically boils down 
   to comparing investment into advertising with the return on investment and 
   making decisions", label="marketing")

# visualize
plot(position(max), col="yellow")

# check for competence profile identity
max == fridolin
near(max, fridolin)
competences(fridolin)

}

\keyword{ textmining }
\keyword{ natural language processing }
\keyword{ learning analytics }
