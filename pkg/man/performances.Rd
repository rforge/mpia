
\name{performances}
\docType{methods}

\alias{performances}
\alias{performances-methods}
\alias{performances,HumanResourceManager-method}
\alias{performances,Person-method}

\title{ The Performances of a Single or All Persons }
\description{

	Return a vector with references pointing to the Performance objects of a single
   person (for the argument being a \code{Person}) or all persons (for the argument
   being a \code{HumanResourceManager}).

}
\section{Methods}{
\describe{

\item{\code{signature(x = "HumanResourceManager")}}{
	Return all performances of all persons this \code{HumanResourceManager} cares for.
}

\item{\code{signature(x = "Person")}}{
	Return all performances of the person of interst.
}
}}

\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\seealso{
   \code{\link{competences}}
}

\keyword{ mpia }
\keyword{ Performance }
\keyword{ HumanResourceManager }
\keyword{ Person }
\keyword{ competences }
