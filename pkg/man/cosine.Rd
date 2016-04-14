\name{cosine-methods}
\docType{methods}

\alias{cosine}
\alias{cosine-methods}
\alias{cosine,matrix,missing-method}
\alias{cosine,Performance,Performance-method}
\alias{cosine,Performance,Person-method}
\alias{cosine,Person,missing-method}
\alias{cosine,Person,Person-method}
\alias{cosine,vector,matrix-method}
\alias{cosine,vector,vector-method}

\title{Cosine measure}
\description{

Determine the cosine closeness of vectors in the social semantic space (for various input formats): the cosine proximity is a measure of the angle between vectors.

It is closer or equal to 1, where the angle is small or zero; close to or equal to 0 where the angle is close to orthogonal 
or othogonal; it is close to or equal to -1, where the vectors are close to or in fact pointing into opposite directions. 

Please note, that there is currently no known interpretation of the difference between the right and the straight angle
(in the strech-truncated social semantic space, the underlying classical vector space does not have such property).

}

\section{Methods}{

\describe{

\item{\code{signature(x = "matrix", y = "missing")}}{ Return cosine pairs (for columns of the matrix) as a proximity matrix. }

\item{\code{signature(x = "Performance", y = "Performance")}}{ Return cosine for the two performances. }

\item{\code{signature(x = "Performance", y = "Person")}}{ Return cosine between performance and person's position. }

\item{\code{signature(x = "Person", y = "missing")}}{ Return cosine for all performances to each other (as proximity matrix). }

\item{\code{signature(x = "Person", y = "Person")}}{ Return cosine for the two person's positions. }

\item{\code{signature(x = "vector", y = "matrix")}}{ Return cosine values between vector and all columns of the matrix. }

\item{\code{signature(x = "vector", y = "vector")}}{ Return cosine value between the two vectors. }

}}

\seealso{
	\code{\link{Person}}, \code{\link{Performance}}, \code{\link{lsa::cosine}}
}

\keyword{mpia}
\keyword{cosine}
\keyword{Person}
\keyword{Performance}
