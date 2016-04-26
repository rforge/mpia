
\name{Visualiser-class}
\Rdversion{1.1}
\docType{class}

\alias{Visualiser-class}

\title{Class \code{"Visualiser"}}

\description{

	Every \code{Domain} uses a Visualiser to create cartographic plots of the social semantic
	network it holds: the visualized conceptual map can then be used as a projection
	surface to display locations, positions, and pathways of learners or their individual,
	textually represented learning activities.

}

\section{Fields}{
  \describe{

    \item{\code{domain}:}{Reverse reference from the visualiser to its domain}
    \item{\code{type}:}{The type of the current map plot: one of 'topographic', 'persp', 'wireframe', 'contour'}
    \item{\code{mode}:}{The focus of the current map plot: currently only 'terminology' is supported ('incidences' or 'both' would be the alternative)}
    \item{\code{netcoords}:}{The exact coordinates of the planar projection (before grid-tiling aggregation for surface elevation)}
    \item{\code{prestigeTerms}:}{The \code{prestige} scores for the terminology: symmetric matrix with scores for all term pairs}
    \item{\code{prestigeDocs}:}{The \code{prestige} scores for the incidences: symmetric matrix with scores for all document pairs}
    \item{\code{wireframe}:}{The relief contour: a wireframe of elevation levels (resulting from the grid-tiling aggregation of the \code{netcoords})}
    \item{\code{mapData}:}{Viewing transformation matrix required for projecting additional visual data into the display area (such as returned by e.g. \code{persp})}
    \item{\code{perspective}:}{logical, indicating whether the current plot is using perspective (or whether its plain), default is \code{TRUE}}
    \item{\code{version}:}{current version number of the Visualiser class (used for updating cached objects)}
    
  }
}

\section{Generics}{
  \describe{

    \item{summary}{\code{signature(object = "Visualiser")}: display the key characteristics of the visualisation data held. }

	}
}


\section{Class Methods}{
  \describe{
  
    \item{\code{initialize(domain, ...)}:}{ The constructor, no actual calculation is done in this step }


    \item{\code{calculateNetCoords(method)}:}{ Method used internally to calculate a position through planar projection. }
    \item{\code{calculateReliefContour(nrow, ncol)}:}{ Method used internally to calculate a smoothened wireframe with the elevation levels reflecting the grid square density of the network coordinates (see \code{calculateNetCoords}). }


    \item{\code{plotMap(method, rotated, name, contour, focus)}:}{ Display a perspective or plain plot with the knowledge cartographic map. }
    \item{\code{toponymy(gridsize, method, add, col)}:}{ Find interesting places on the map and label them. }
    \item{\code{topo.colors.pastel(n)}:}{ Helper method to create a colour palette of hyposometric tints. }
    \item{\code{labelFlag(x, y, label, border, bg, cex, box, col, marker.col)}:}{ Helper method to plot a label flag onto a particular location on the map. }


    \item{\code{plotPath(performanceList, col, alpha, label, component.labels, component.arrows, dot.cex, box, connect)}:}{ Plot a sequence of markers on the map, each indicating the exact location of a performance. If \code{connect} is set to \code{TRUE}, a \code{\link{xspline}} will be used to connect the locations. }
    \item{\code{plotPerformance(performance, polyMax, col, label, component.labels, component.arrows, dot.cex)}:}{ Plot a marker onto the exact location of a performance (and add markers for the top loading terms activated by it). If \code{component.arrows} is \code{TRUE}, arrows will be plotted starting from the location of the performance and pointing towards the position of the constituting key terms; if it is \code{FALSE}, a dot will be plotted (with \code{dot.cex} size). \code{polyMax} sets the number of component terms to be highlighted, per default 3. }

    \item{\code{summary()}:}{ Display summary statistics about the visualisation data held (use generic \code{summary} instead). }
    \item{\code{print()}:}{ Display short info about the object }
    \item{\code{show()}:}{ Display short info about the object }
    
    
    \item{\code{copy(shallow, domain)}:}{ This method is required for updating the Visualiser object to newer versions of the class without loosing its data }


    \item{\code{newDevice(name, pdf, filename)}:}{ Standardised interface to open a new plot on the device of choice regardless of operating system }
    \item{\code{closeDevice()}:}{ Standardised interface to close the plot on the device of choice regardless of operating system }


  }
}

\examples{

dmgr = DomainManager()
#d = dmgr$get("generic")
data(generic.domain)
dmgr$add(d, title="generic")

summary(d$visualiser)

plot(d)
toponymy(d)

}

\author{Fridolin Wild <fridolin.wild@open.ac.uk}
\references{Fridolin Wild (2013): Analysing Learning from Meaningful Purposive Interaction.}

\seealso{
	\code{\link{Domain}}, \code{\link{DomainManager}}, \code{\link{plot}}, \code{\link{toponymy}}
}

\keyword{mpia}

\keyword{Visualiser}
\keyword{visualization}
\keyword{visualisation}

\keyword{map}
\keyword{knowledge cartography}
\keyword{plot}
