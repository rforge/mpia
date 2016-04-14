\name{labels-methods}
\docType{methods}

\alias{labels-methods}
\alias{labels,Performance-method}

\title{ The Labels of Performance(s) }
\description{

	Function to get the label of the performance.

}

\section{Methods}{
\describe{

   \code{labels} is a generic accessor function. It returns the field \code{name} of a \code{Performance}.

}}

\usage{
   \code{labels(x)}
}

\value{
	\code{NULL} or character vector of the same length as x. 
}


\exampe{

dmgr = DomainManager()
dmgr$tempdir = "~/Documents/werkstatt/mpia-package/cache/"
d = dmgr$get("generic")
fridolin = Person(name="fridolin", d)
p = fridolin$write("business management", label="demo")
labels(p)

}

\keyword{mpia}
\keyword{Performance}
\keyword{names}
