\name{datasets}

\alias{essays.domain}
\alias{essays}
\alias{essays.scores}
\alias{essays.students}
\alias{d}
\alias{docs}
\alias{im}
\alias{space}

\title{ Data sets provided with the MPIA package }

\description{

The package comes with three data sets. As with all corpus data, the corpora provided in the Domain objects have to be utilised with care: they are all general purpose and may not contain professional vocabulary required for a particular application. If words are not contained in the vocabulary, they will simply be disregarded in any analysis (garbage in, garbage out!).

The first one, \code{generic.domain}, contains a small standard English corpus, suitable for many try-out applications.

The second one, \code{essays.domain}, contains a small German domain corpus \code{d}. Complementary essay data is contained in the three supplementary vectors \code{essays.scores} for scores of the essays (assigned by human raters), \code{essays.students} holds randomly assigned firstnames (and in brackets the number of the collection they are from), whereas \code{essays} contains character vectors with the actual essay text. These serve demonstration purposes only, simply call \code{demo("essay-scoring")} to see how they are used.

Finally, \code{docs}, \code{im}, and \code{space} contain the objects resulting from \code{demo("sna-lsa-mpia")}, for easier referencing in tutorials.

All these data sets are also used in the package tests.

}
\usage{
    data(essays.domain)
    data(essays)
    data(essays.scores)
    data(generic.domain)
    data(docs)
    data(im)
    data(space)
}

\details{

    Note that \code{generic.domain} contains an object \code{d}.

}

\value{
    Fetches the according data set from the package`s data directory.
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
