plotPosition <- function ( person, ix, polyMax=3, col="black" ) {
   
   if (ix > length(person$performances)) stop("this index value does not exist")
   
   # -- snip
   termVectors = d$space$tk %*% diag(d$space$sk)
   mv = person$getMeaningVectors(ix)
   
   d = person$currentDomain
   
   if (is.null(d$netcoords)) {
      
   }
   
   # distribution in original space
   #table(round((as.textmatrix(d$space)),1))
   
   # cosine proximal term vecs (from current meaning vec): warning: very different!
   #cosines = cosine( mv, termVectors)
   #sort( rownames(d$space$tk)[which(cosines>0.3)] )
   
   dtm = crossprod(t(crossprod(t(d$space$tk), diag(d$space$sk))), t(mv))
   #sort( rownames(dtm)[ which(dtm>0.7) ] )
   ixs = which(dtm>0.7)
   
   if (is.na(ixs) || length(ixs)==0) {
    	warning("No terms were activated by this performance.")
      return(NULL)
   }
   
   
   termDescriptors = dtm[ ixs, ]
   names(termDescriptors) = rownames(dtm)[ixs]
   termDescriptors = sort( termDescriptors, dec=TRUE)
   
   # -- snap
   
   termDescriptors = termDescriptors[ 1:(min(length(termDescriptors),polyMax)) ]
   
   if (is.na(termDescriptors) || length(termDescriptors)==0) {
    	warning("No terms were activated by this performance.")
      return(NULL)
   }
   
   # labelFlags for the constituent term vectors
   tnc = d$netcoords[names(termDescriptors),]
   
   #pg = expandpoly(tnc, 1.2)
   #polygon( pg, col=hsv(30/60,0.3, 0.9, alpha=0.5), border="transparent" )
   
   pixelx = 1 / dev.size(units="px")[1]
   pixely = 1 / dev.size(units="px")[2]
   
   if (length(termDescriptors)>1) {
      
      wfp = (1/(sum(termDescriptors))) * (colSums(tnc*termDescriptors)) # weighted
      for (i in 1:nrow(tnc)) {
         cx = termDescriptors[ which( names(termDescriptors) == rownames(tnc)[i] ) [1] ]
         cxX = (tnc[i,1]-wfp["cx"])*0.1 #cxX
         cxY = (tnc[i,2]-wfp["cy"])*0.1 #cxY
         arrows( wfp["cx"], wfp["cy"], wfp["cx"] + cxX, wfp["cy"]+cxY, col=col, length=0.05, lwd=ceiling(log(cx+1)))
      }
      
      for (i in 1:nrow(tnc)) {
         cx = termDescriptors[ which( names(termDescriptors) == rownames(tnc)[i] ) [1] ]
         cx = log(cx+1)/2
         d$labelFlag(tnc[i,"cx"], tnc[i,"cy"],rownames(tnc)[i], col=col, box=FALSE, cex=cx)
      }
      
      weightedFocalPoint = wfp
      
   } else {
      
      segments( tnc["cx"]+2*pixelx, tnc["cy"]+2*pixely, tnc["cx"]-2*pixelx, tnc["cy"]-2*pixelx, col=col, lwd=3)
      segments( tnc["cx"]-2*pixelx, tnc["cy"]+2*pixely, tnc["cx"]+2*pixelx, tnc["cy"]-2*pixelx, col=col, lwd=3)
      weightedFocalPoint = tnc
      
      cx = termDescriptors
      cx = log(cx+1)/2
      d$labelFlag(tnc["cx"], tnc["cy"], names(termDescriptors), col=col, box=FALSE, cex=cx)
      
   }
   
   
   # labelFlag for focalPoint of the Performance
   #focalPoint = (1/nrow(tnc)) * (colSums(tnc)) # unweighted
   #performanceNames = unlist(lapply(person$performances, function(l) l$name))
   #d$labelFlag(focalPoint["cx"], focalPoint["cy"],performanceNames[1], col="red", box=FALSE)
   
   # labelFlag for weightedFocalPoint of the Performance
   performanceNames = unlist(lapply(person$performances, function(l) l$name))
   d$labelFlag(weightedFocalPoint["cx"], weightedFocalPoint["cy"]+5*pixely,performanceNames[ix], col=col, cex=1, box=FALSE) # cex=log(mean(termDescriptors)+1)
   
   invisible( c( weightedFocalPoint["cx"], weightedFocalPoint["cy"]))
   
} # function plotPosition()

plotPath <- function(person, col="red") {
   
	path = NULL
   
   for (i in 1:length(person$performances)) {
      path = rbind(path, plotPosition(person, i, polyMax=5, col=col))
   }
   
	#lines(path, col=col)
   if (nrow(path)>2) {
	   xspline(x=path[,1], y=path[,2], s=c(0,rep(-1,nrow(path)-2),0), border=col, lwd=3)
   } else if (nrow(path)==2) {
    	lines(path, col=col, lwd=3)
   }
   
}









from Person:


Person$methods(
plotPosition = function ( ix, polyMax=3, col="black" ) {
   
   if (ix > length(.self$performances)) stop("This index value does not exist")
   if (length(ix) > 1) stop("Only one at a time!")
   
   #termVectors = d$space$tk %*% diag(d$space$sk)
   #mv = .self$getMeaningVectors(ix)
   termDescriptors = .self$getActivatedTerms(ix)[[1]]
   
   if ( (!is.list(termDescriptors) || length(termDescriptors$labels)==0) ) {
      warning("No terms were activated by this performance.")
      return(invisible(FALSE))
   }
   
   termDescriptors$labels = termDescriptors$labels[ 1:(min(length(termDescriptors$labels),polyMax)) ]
   termDescriptors$tkix = termDescriptors$tkix[ 1:(min(length(termDescriptors$tkix),polyMax)) ]
   termDescriptors$values = termDescriptors$values[ 1:(min(length(termDescriptors$values),polyMax)) ]
   
   # labelFlags for the constituent term vectors
   tnc = .self$currentDomain$visualiser$netcoords[termDescriptors$tkix,]
   
   pixelx = 1 / dev.size(units="px")[1]
   pixely = 1 / dev.size(units="px")[2]
   
   weightedFocalPoint = NULL
   
   if (length(termDescriptors$labels)>1) {
      
      wfp = (1/(sum(termDescriptors$values))) * (colSums(tnc*termDescriptors$values)) # weighted
      for (i in 1:nrow(tnc)) {
         cx = termDescriptors$values[ i ]
         cxX = (tnc[i,1]-wfp["cx"])*0.1 #cxX
         cxY = (tnc[i,2]-wfp["cy"])*0.1 #cxY
         arrows( wfp["cx"], wfp["cy"], wfp["cx"] + cxX, wfp["cy"]+cxY, col=col, length=0.05, lwd=ceiling(log(cx+1)))
      }
      
      for (i in 1:nrow(tnc)) {
         cx = termDescriptors$values[i]
         cx = log(cx+1)/2
         .self$currentDomain$visualiser$labelFlag(tnc[i,"cx"], tnc[i,"cy"], termDescriptors$labels[i], col=col, box=FALSE, cex=cx)
      }
      
      weightedFocalPoint = wfp
      
   } else {
      
      segments( tnc["cx"]+2*pixelx, tnc["cy"]+2*pixely, tnc["cx"]-2*pixelx, tnc["cy"]-2*pixelx, col=col, lwd=3)
      segments( tnc["cx"]-2*pixelx, tnc["cy"]+2*pixely, tnc["cx"]+2*pixelx, tnc["cy"]-2*pixelx, col=col, lwd=3)
      weightedFocalPoint = tnc
      
      cx = termDescriptors$values[1]
      cx = log(cx+1)/2
      currentDomain$visualiser$labelFlag(tnc[1,"cx"], tnc[1,"cy"], termDescriptors$labels[1], col=col, box=FALSE, cex=cx)
      
   }
   
   # labelFlag for focalPoint of the Performance
   #focalPoint = (1/nrow(tnc)) * (colSums(tnc)) # unweighted
   #performanceNames = unlist(lapply(.self$performances, function(l) l$name))
   #d$labelFlag(focalPoint["cx"], focalPoint["cy"],performanceNames[1], col="red", box=FALSE)
   
   # labelFlag for weightedFocalPoint of the Performance
   performanceNames = unlist(lapply(.self$performances, function(l) l$name))
   currentDomain$visualiser$labelFlag(weightedFocalPoint["cx"], weightedFocalPoint["cy"]+5*pixely, performanceNames[ix], col=col, cex=1, box=FALSE) # cex=log(mean(termDescriptors)+1)
   
   invisible( c( weightedFocalPoint["cx"], weightedFocalPoint["cy"]))
   
}
) # method: plotPosition()
