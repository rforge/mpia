
\name{dimcalc_var}
\alias{dimcalc_var}

\title{
	Calculate the optimal number of Eigenvalues to be retained.
}

\description{

	This method calculates the optimal number of rooted Eigenvalues (of the singular value decomposition) to be
   kept in the MPIA factor reduction in such a way that 80\% of the total amount of stretch is achieved. The
   total amount of stretch expands the Eigenvectors to match the mapping defined by the original ('raw') 
   document-term matrix.

}
\usage{
	dimcalc_var()
}

\details{

	This method provides a mathematically grounded way to predict the optimal number of dimensions to keep in the truncation of
   the truncated singular value decomposition.

}

\value{
	Returns a function accepting the rooted Eigenvalues as parameter s.
}

\seealso{
    \code{\link[lsa:dimcalc_raw]{dimcalc_raw()}}
}

\examples{
dimcalc_var()(c(5,4,3,2,1))
}

\author{Fridolin Wild <wild@brookes.ac.uk>}
\references{Fridolin Wild (2016): Learning Analytics in R with SNA, LSA, and MPIA, Springer:Berlin.}

\keyword{ mpia }
\keyword{ eigenvalues }
\keyword{ truncation }
\keyword{ singular value decomposition }
\keyword{ svd }
\keyword{ dimensionality }
