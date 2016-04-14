
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

\author{Fridolin Wild <fridolin.wild@open.ac.uk>}
\references{Fridolin Wild (2013): Meaningful Purposive Interaction Analysis.}

\seealso{
	\code{\link{lsa::dimcalc_raw()}}
}

\examples{
dimcalc_var()(c(5,4,3,2,1))
}

\keyword{ mpia }
\keyword{ eigenvalues }
\keyword{ truncation }
\keyword{ singular value decomposition }
\keyword{ svd }
\keyword{ dimensionality }
