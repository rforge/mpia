\name{summary-methods}
\docType{methods}

\alias{summary-methods}

\alias{summary,Domain-method}
\alias{summary,Performance-method}
\alias{summary,Person-method}
\alias{summary,Visualiser-method}

\title{Summary methods of the mpia package}

\description{
	Provide basic descriptive statistics for the main objects of MPIA: Domain, DomainManager, Person, HumanResourceManager, Visualiser, Performance.
}

\section{Methods}{
\describe{

	\item{\code{signature(object = "Domain")}}{
	Describe the data held in this Domain.
	}

	\item{\code{signature(object = "Performance")}}{
	Describe the meaning vector of this performance (label, source text if stored, key term descriptors activated).
	}

	\item{\code{signature(object = "Person")}}{
	Describe the performances stored for this person (using the performance summary).
	}

	\item{\code{signature(object = "Visualiser")}}{
	Describe the visualisation data held.
	}
}}

\keyword{mpia}
\keyword{summary}
