# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
#       Class Domain
#
#       fridolin.wild@open.ac.uk
#       last update: August 7, 2013
#
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

Domain <- setRefClass( "Domain",

   fields = list(

		name = "character",

		space = "ANY", # LSAspace
		traces = "ANY",

		termProximities = "ANY",
		mode = "character", 
		proximityThreshold = "numeric",
		identityThreshold = "numeric",

		textmatrix = "ANY",
		processed = "logical",
		signature = "ANY",

		visualiser= "ANY",

		version="numeric"

   ),

   methods = list(

      initialize = function( name="", ... ) {

         callSuper(...)
         
         if (missing(name)) {
         	cat("domain name required. please add!\n")
         }

         name <<- name
         
         textmatrix <<- NULL
         processed <<- FALSE
         
         space <<- NULL
         signature <<- NULL
         
         termProximities <<- NULL
         mode <<- "terminology"
         proximityThreshold <<- 0.3 # was 0.35
         identityThreshold <<- 0.7
         
         traces <<- matrix(ncol=0, nrow=0)
         
         visualiser <<- new("Visualiser", .self)
         
         version <<- 0.59
         
      },

      print = function() {
         cat(paste("An object of class 'Domain' and name '",.self$getName(),"'.\n", sep=""))
      },

      show = function () {
         cat(paste("An object of class 'Domain' and name '",.self$getName(),"'.\n", sep=""))
      }

   )
) # Class Spaces

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# a fold in that returns context vectors

Domain$methods(
fold_in = function( docvecs ) {

   dqs = crossprod(t(crossprod(docvecs, space$tk)), solve(diag(space$sk)))
   
   class(dqs) = "docvector"
   if (is.matrix(dqs)) nrvecs = nrow(dqs) else nrvecs = 1
   
   if ( nrow(traces)==0 ) {
      traces <<- dqs
      #cat("[Domain] first traces added to Domain.\n")
      ix = 1:nrvecs
   } else {
      traces <<- rbind(traces, dqs )
      #cat("[Domain] additional traces added to Domain.\n")
      ix = (nrow(traces)-nrvecs+1):nrow(traces)
   }
   
   return(ix)

}) # add method: fold_in()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# a fold in that returns context vectors

Domain$methods(
addTrace = function( vecs ) {
   
   class(vecs) = "docvector"
   if (is.matrix(vecs)) nrvecs = nrow(vecs) else nrvecs = 1
   
   if ( nrow(traces)==0 ) {
      traces <<- vecs
      ix = 1:nrvecs
   } else {
      traces <<- rbind(traces, vecs )
      ix = (nrow(traces)-nrvecs+1):nrow(traces)
   }
   
   return(ix)
   
}) # method: addTrace()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# submit corpus to cRunch for space calculation: to be implemented

Domain$methods(
submit = function() {

   tmp = tempfile()
   save(textmatrix, file=tmp)
   cat("to be implemented")
   unlink(tmp)

   return(FALSE)
   
}
) # add method fromCorpus

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# copy field data, recreate objects and copy their field data

Domain$methods(
   copy = function( shallow=FALSE ) {

      def <- .refClassDef
      value <- new(def, name="") # added name parameter
      vEnv <- as.environment(value)
      selfEnv <- as.environment(.self)
      for (field in names(def@fieldClasses)) {
         if (shallow)
	         assign(field, get(field, envir = selfEnv), envir = vEnv)
         else {
            current <- get(field, envir = selfEnv)
            if (is(current, "envRefClass")) {
               if (field == "visualiser") { # added this
                  current <- current$copy(FALSE, .self)
               } else {
            		current <- current$copy(FALSE)
               }
            }
            assign(field, current, envir = vEnv)
         }
      }
      value
      
   }
) # method: copy()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# setSpace

Domain$methods(
setSpace = function( x ) {
   
   if (missing(x) || class(x)!="LSAspace") stop("  space required (of class 'LSAspace').")
   
   space <<- x
   signature <<- digest(space)
   invisible(TRUE)
   
}) # method: setSpace


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# get routines

Domain$methods(
getSpace = function() {
   return(.self$space)
}) # method: getSpace

Domain$methods(
getName = function() {
   return(.self$name)
}) # method: getName

Domain$methods(
getVocabulary = function() {
   return( rownames(space$tk) )
}) # method: getVocabulary


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# corpus: create textmatrix from corpus

Domain$methods(
corpus = function( x ) {
   
   if ( (is.character(x) && file.exists(x)) || (mode(x) == "character" && is.vector(x) && all(file.exists(x))) ) {
      
	   # if corpus is a file/directory or list of files/directories
      
      tm = Corpus(DirSource(x), readerControl=list(reader=readPlain(), language="en", load=TRUE, removePunctuation=TRUE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE))
      tm = tm_map(tm, tolower)
      tmorig = tm
      tm = tm_map(tm, stemDocument, language="en")
      dict = Dictionary(TermDocumentMatrix(tmorig, control=list(removePunctuation=TRUE, stopwords=FALSE, minWordLength=1, removeNumbers=TRUE)))
      dtm = TermDocumentMatrix(tm, control = list(
	      removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE,
	      minWordLength = 3, bounds = list(global=c(1,Inf))
      ))
      
      dtms = dtm
      sc = as.character( stemCompletion(rownames(dtms), dictionary=dict, type="shortest") )
      sc[which(is.na(sc))] = rownames(dtms)[which(is.na(sc))]
      
      dtmsold = dtms
      rownames(dtms) = sc
      if (any(duplicated(rownames(dtms)))) {
         
         dupes = which(duplicated(rownames(dtms)))
         for (i in dupes) {
            
            cat(paste("removing dupe for ", sc[i], "\n", sep=""))
            hits = which(sc == sc[i])
            target = hits[ which(! hits %in% which(duplicated(sc))) ]
            dtms[ target, ] = colSums(as.matrix(dtms[ hits[which(hits != target)], ]))
            
         }
         dtms = dtms[!duplicated(rownames(dtms)),]
      }
      class(dtms) = c("TermDocumentMatrix", "matrix", class(dtms))
      
      if (any(rownames(dtms) == "")) {
         cat("removing empty ones")
         dtms = dtms[-(which(rownames(dtms) == "")), ]
      }
      
      .selF$textmatrix = dtms
      processed <<- FALSE
      
      invisible(TRUE)

   } else if (any(class(x) == "Source")) {

      tm = Corpus(x, readerControl=list(reader=readPlain, language="en", load=TRUE, removePunctuation=TRUE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE))
      tm = tm_map(tm, tolower)
      
      # save full dictionary for stem completion
      dict = Terms(DocumentTermMatrix( tm, control=list(removePunctuation=TRUE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE)))
      
      # stemming
      tm = tm_map(tm, stemDocument, language="en")
      dtm = TermDocumentMatrix(tm, control = list(
         removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE,
         minWordLength = 3, bounds = list(global=c(1,Inf))
      ))

      # stem completion
      sc = as.character( stemCompletion(rownames(dtm), dictionary=dict, type="shortest") )
      sc[which(is.na(sc))] = rownames(dtm)[which(is.na(sc))]
      
      rownames(dtm) = sc
      if (any(duplicated(rownames(dtm)))) {
          
          dupes = which(duplicated(rownames(dtm)))
          for (i in dupes) {
              
              hits = which(sc == sc[i])
              target = hits[ which(! hits %in% which(duplicated(sc))) ]
              replvec = t(as.matrix( colSums(as.matrix(dtm[ hits, ])) ))
              rownames(replvec) = sc[target]
              dtm[ target,1:length(replvec) ] = replvec
              
          }
          dtm = dtm[!duplicated(rownames(dtm)),]
      }
      
      if (any(rownames(dtm) == "")) {
          cat("removing empty ones")
          dtm = dtm[-(which(rownames(dtm) == "")), ]
      }
      
      #dtm = as.matrix(dtm)
      #class(dtm) = "dgCMatrix"
      
      .self$textmatrix = dtm
      
      rm(dtm)
      gc()
      
      processed <<- FALSE
      
      invisible(TRUE)
      
   } else if (any(class(x) %in% c("TermDocumentMatrix","textmatrix"))) {

      # if corpus is a TermDocumentMatrix object
      textmatrix <<- x
      processed <<- FALSE
      signature <<- digest(textmatrix)
      
      invisible(TRUE)
      
   }
   
   invisible(FALSE)
   
}
) # add method corpus


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# wrapper for improved LSA

Domain$methods(
   spacify = function() { #

      tr = sum(.self$textmatrix * .self$textmatrix) # faster
      #was tr = Trace(as.matrix(.self$textmatrix) %*% as.matrix(t(.self$textmatrix)))
      
      space <<- lsa::lsa(.self$textmatrix, dims=dimcalc_raw())
      
      eigenv = space$sk^2
      eigenvsum = NULL
      for (i in 1:length(eigenv)) {
         eigenvsum[i] = sum(eigenv[1:i])
      }
      dims80 = which(eigenvsum > (0.8*tr))[1]
      
      space$tk <<- space$tk[, 1:dims80]
      space$dk <<- space$dk[, 1:dims80]
      space$sk <<- space$sk[1:dims80]
      
      signature <<- digest(space) # update signature to point to space signature
      processed <<- TRUE
         
      invisible(signature)
      
   }
) # method: lsa()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# calculateTermProximities(): calculate term to term proximities (cosine)

Domain$methods(
   calculateTermProximities = function( mode = .self$mode, normalise=FALSE, mincomp=ceiling(sqrt(nrow(.self$space$tk))) ) {

      if (mode=="terminology") {
         lsavecs = .self$space$tk %*% diag(.self$space$sk)
      } else if (mode=="incidence") {
         lsavecs = .self$space$dk %*% diag(.self$space$sk)
      } else if (mode=="both") {
         dvs = .self$space$dk %*% diag(.self$space$sk)
         tvs = .self$space$tk %*% diag(.self$space$sk)
         lsavecs = rbind(dvs,tvs)
      }
      
      cosines = cosine(t(lsavecs))
      
      if (normalise) {
          cmin = min(cosines)
          cmax = max(cosines)
          cosines = cosines - cmin / (cmax-cmin)
      }
      
      # free a bit of memory
      rm(lsavecs)
      gc()

      if ( any(is.na(cosines)) ) {
         l = length(which(is.na(cosines)))
         warning( paste("calculateTermProximities(): there are ",l," missing values in the resulting cosine proximity matrix!", sep=""))
      } # security check: were all vector pairs processed?
      
      # store backup for isolate identification
      termProximities <<- cosines
      
      # remove all proximities below the threshold
      # threshold could also be autocalculated? in a way that a certain amount of density is achieved?

      cosines[ which( cosines < .self$proximityThreshold ) ] = 0
      comps = component.dist(cosines,connected="weak")
      
      # precalc to see if isolates will be generated
      # subsequently attach isolates to closest term node(s) (see below)
      
      isolates = NULL
      bestMatchIx = NULL
      bestMatchVal = NULL

      isolateComps = which(comps$csize < mincomp)
      if (length(isolateComps > 0)) { # if there are isolate components

        for (i in 1:length(isolateComps)) {
            
            members = which(comps$membership == isolateComps[i])
            
            if (length(members)>1) {
                btwness = betweenness(cosines[members,members], gmode="graph")
                centernode = which(btwness == max(btwness))[1]
            } else {
                centernode = 1
            }
            
            isolates[i] = members[centernode]
            
            cosRow = .self$termProximities[isolates[i],]
            cosRow[members] = -1 # remove proximity to all members including self
            
            bmIx = which(cosRow == max(cosRow))
            bmVal = cosRow[bmIx]
            if (length(bmIx)>1) { bmIx = bmIx[1] }
            if (length(bmVal)>1) { bmVal = bmVal[1] }
            bestMatchIx[i] = bmIx
            bestMatchVal[i] = .self$proximityThreshold #bmVal
            
        }

        #cosines[ which( cosines < .self$proximityThreshold ) ] = 0
        #cosines[ which( cosines < 0 ) ] = 0

        # reattach isolate components with central node (highest betweenness, see above)
        # to best non-member matching node

        cosines[isolates, bestMatchIx] = bestMatchVal
        print(paste(length(isolates), " stray component(s) reattached.", sep=""))
        
      }

      # convert to sparse matrix
      termProximities <<- as(cosines, "sparseMatrix")

      # force garbage collection
      rm(cosines, comps, isolateComps, isolates, bestMatchIx, bestMatchVal)
      gc()
      
      invisible(TRUE)
      
   }
) # method: calculateTermProximities()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# extend generics with signature methods

setMethod("summary", signature=list(object="Domain"), function ( object, ... ) {

   object$show()
   cat("\n  space data\n")
   cat(paste("    space dimensionality (Tk): ",paste(dim(object$space$tk), collapse=", "),"\n", sep=""))
   cat(paste("    space dimensionality (Dk): ",paste(dim(object$space$dk), collapse=", "),"\n", sep=""))
   cat(paste("    singular values (Sk): ",length(object$space$sk),"\n", sep=""))
   cat(paste("    traces: ",nrow(object$traces),"\n", sep=""))
   cat(paste("    space signature: ",object$signature,"\n", sep=""))
   cat(paste("    termProximities: ",paste(dim(object$termProximities), collapse=", "),"\n", sep=""))
   
   cat("\n  visualisation data\n")
   cat(paste("    netcoords: ",paste(dim(object$visualiser$netcoords), collapse=", "),"\n", sep=""))
   cat(paste("    wireframe: ",paste(dim(object$visualiser$wireframe), collapse=", "),"\n", sep=""))
   cat(paste("    mapData: ",  paste(dim(object$visualiser$mapData), collapse=", "),"\n", sep=""))
   cat(paste("    version: ",  object$visualiser$version,"\n", sep=""))
   
   cat("\n  Class\n")
   cat(paste("    version: ",object$version,"\n", sep=""))
   cat("\n")
   
})

setMethod("plot", signature=list(x="Domain"),
function ( x, ... ) {
   
   x$visualiser$plotMap(...)
   
}) # plot

#if (!isGeneric("toponymy")) setGeneric("toponymy", function(x, ...) standardGeneric("toponymy") )

if (!isGeneric("toponymy")) setGeneric("toponymy", function(x, ...) standardGeneric("toponymy") )

setMethod("toponymy", signature=list(x="Domain"),
function ( x, ... ) {
   
   x$visualiser$toponymy(...)
   
}) # labels

dimcalc_var <- function () {
   function(s) {
      varsk = s^2/sum(s^2)
      varsk2 = NULL
      for (i in 1:length(varsk)) {
         varsk2[i] = sum(varsk[1:i])
      }
      dims80 = which(varsk2 > 0.8)[1]
      return(dims80)
   }
}


