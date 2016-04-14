# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
#       Class: Performance
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  


Performance <- setRefClass( "Performance",

   fields = list(

		name = "ANY",
		sourcetext = "ANY",
		logging = "logical",

		meaningvector = "ANY",
		terms = "ANY",
		domain = "ANY",

		purpose = "ANY"

   ),

   methods = list(
     
      # init
      initialize = function( text=NULL, purpose=NULL, domain=NULL, name=NULL, weighting=NULL, logging=TRUE ) {

         if (missing(domain) || is.null(domain) || class(domain)!="Domain") {
         	stop("Parameter 'domain' has to be an object of class 'Domain'.")
         } else {
	         domain <<- domain
         }
         
         logging <<- logging
         if (!is.logical(logging)) logging <<- FALSE
         if (logging) sourcetext <<- text else sourcetext <<- NULL
         
         .self$name = name
         
         purpose <<- purpose
         if (is.null(purpose)) purpose <<- ""

         # prepare for meaning vector calculation
         
         .self$meaningvector = NULL
         .self$terms = NULL

         space = domain$getSpace()

         docvecs = NULL
         
         if (!is.null(text)) docvecs = query(text, domain$getVocabulary())
         
         txt = gsub("[^[:alnum:]]", " ", text)
         txt = gsub("[[:space:]]+", " ", txt)
         txt = unlist(strsplit(txt, " ", fixed = TRUE))
         nrwords = length(txt)
         stopwords = sum( txt %in% stopwords() )
         
         if ( sum(docvecs)<10 && ( (nrwords-stopwords) - 0.2*(nrwords-stopwords)) > sum(docvecs)) {
            cat(paste("WARNING: of ~", nrwords," words (incl. ~", stopwords," potential stopwords) only ",
            	sum(docvecs), " were retained. The rest was possibly not part of the domain vocabulary?", "\n",sep=""))
         }
         
         # weighting
         if (!missing(weighting) && !is.null(docvecs)) {
	         docvecs = weighting(docvecs)
         }
         
         # calculate meaning vector
         if (!is.null(docvecs)) {
            meaningvector <<- domain$fold_in(docvecs)
            .self$terms = .self$getActivatedTerms()
            #if (is.na(.self$terms)) cat ("WARNING: No terms were activated by this performance.\n")
         }
         
         invisible(TRUE)
         
      }, # method: initialize()

      print = function() {
         cat(paste("An object of class 'Performance'.\n", sep=""))
      },

      show = function () {
         cat(paste("An object of class 'Performance'.\n", sep=""))
      }

   ) # methods
) # Class Performance

Performance$methods(
getPurpose = function() {
   return(purpose)
}) # method: getPurpose()

Performance$methods(
getDomain = function() {
   return(domain)
}) # method: getDomain()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# getMeaningVector(): resolve index value and return trace meaning vector stored in domain space

Performance$methods(
getMeaningVector = function() {
   mvec = t(domain$traces[meaningvector,])
   rownames(mvec) = name
   return( mvec )
}) # method: getMeaningVector()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# getActivatedTerms(): lsa multiply, return those terms that are above domain's proximity threshold

Performance$methods(
	getActivatedTerms = function(threshold = .self$domain$proximityThreshold) {

		#termVectors = .self$domain$space$tk %*% diag(.self$domain$space$sk) # was active, but seems not to be used!!
      mv = .self$getMeaningVector()
      
      if (nrow(mv) > 1) stop("currently only one text per performance supported!")

      # distribution in original space
      #table(round((as.textmatrix(d$space)),1))

      # cosine proximal term vecs (from current meaning vec): warning: very different!
      #cosines = cosine( mv, termVectors)
      #sort( rownames(d$space$tk)[which(cosines>0.3)] )

      dtm = crossprod(t(crossprod(t(.self$domain$space$tk), diag(.self$domain$space$sk))), t(mv))
      ixs = which(dtm>threshold)

      if ( all(is.na(ixs)) || length(ixs)==0) {
         
         #cat(paste("No terms were activated by this performance (domain proximity threshold = ", domain$proximityThreshold, ".\n", sep=""))
         return(NA)
         
      } else {

	      termDescriptors = dtm[ixs,]
	      names(termDescriptors) = rownames(dtm)[ixs]
   	   sortix = sort( termDescriptors, dec=TRUE, index.return=TRUE)
         termDescriptors = list(labels=names(termDescriptors[sortix$ix]), values=as.double(termDescriptors[sortix$ix]), tkix=ixs[sortix$ix])
      
      	return(termDescriptors)
         
      }

}) # method: getActivatedTerms()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# get / set stuff

Performance$methods(
setMeaningVector = function( vec ) {
   meaningvector <<- domain$addTrace(vec)
   invisible( TRUE )
}) # method: setMeaningVector()

Performance$methods(
getSourceText = function() {
   return( .self$sourcetext )
}) # method: getSourceText()

Performance$methods(
setName = function( value ) {
   .self$name = value
   #invisible( .self$name )
}) # method: setName()

Performance$methods(
getName = function() {
   return( .self$name )
}) # method: getName()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# setMethods for generics (existing ones)
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

#if (!isGeneric("inspect")) setGeneric("inspect", function(x, ...) standardGeneric("inspect") ) # if package tm or inspect not loaded

setMethod("summary", signature=list(object="Performance"),
function ( object, ... ) {
   
   cat( paste( "name: ", object$name, "\n", sep=""))
   
   source = object$getSourceText()
   if (!is.null(source)) cat (paste("source: '",substr(source,1,80),if(nchar(source)>80) "...", "'\n", sep=""))
   
   ts = object$getActivatedTerms(...)
   if (is.list(ts)) cat(paste("about: ", paste(ts$labels, collapse=", ", sep=""), "\n", sep="")) else cat("about: no terms activated\n")
   
}) # summary

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

setMethod("plot", signature=list(x="Performance"),
function ( x, ... ) {
   
   if (is.list(x) && all( unlist(lapply(x, function(e) class(e))) == "Performance") ) {
      x[[1]]$domain$visualiser$plotPath(x, ...)
   } else {
      x$domain$visualiser$plotPerformance(x, ...)
   }
   
}) # plot

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

setMethod("show", signature=list(object="Performance"),
function ( object ) {
   if (is.list(object)) {
      a = lapply(object, function(e) e$show())
      return(a) # was invisible
   } else {
      return(object$show())
   }
   
}
) # show

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

setMethod("print", signature=list(x="Performance"),
function ( x ) {
   if (is.list(x)) {
      a = lapply(x, function(e) e$print())
      return(a) # was invisible
   } else {
      return(x$print())
   }
   
}
) # print

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

#if (!isGeneric("names")) setGeneric("names", function(x) standardGeneric("names") )
#if (!isGeneric("names")) setGeneric("names", function(x, ...) standardGeneric("names", .Primitive("names")) )

setMethod("names", signature=list(x="Performance"),
function ( x ) {
   
   if (is.list(x)) {
      a = lapply( x, names )
   	return( a )
   } else {
		return(x$getName())
   }
   
}) # names

#if (!isGeneric("labels")) setGeneric("labels", function(x) standardGeneric("labels") )

#setMethod("labels", signature=list(x="Performance"),
#function ( x ) {
#
#   if (is.list(x)) {
#      a = lapply( x, names )
#   	return( unlist(a) )
#   } else {
#		return(x$getName())
#   }
#
#}) # names

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

#if (!isGeneric("names<-")) setGeneric("names<-", function(x, value) standardGeneric("names<-") )

setReplaceMethod("names", signature=list(x="Performance", value="character"),
function ( x, value ) {
	x$setName(value)
   return(x)
}) # names<-

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

setMethod("==", signature=list(e1="Performance", e2="Performance"),
function ( e1, e2 ) {
   
   if (e1$domain$signature != e2$domain$signature) stop ("Cannot compare: the performances are in different domains!")
   
   x = e1$getMeaningVector()
   y = e2$getMeaningVector()
   
   termVectors = e1$domain$space$tk %*% diag(e1$domain$space$sk)
   dtm = crossprod(t(crossprod(t(e1$domain$space$tk), diag(e1$domain$space$sk))), t(rbind(x,y)))
   cos = cosine(dtm)
   
   return( cos[2,1]>e1$domain$identityThreshold )
   
}) # ==

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

setMethod("+", signature=list(e1="Performance", e2="Performance"),
function ( e1, e2 ) {
   p = c(e1,e2)
   class(p) = "Performance"
	return(position(p))
   
}) # ==


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# new ones
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

if (!isGeneric("terms")) setGeneric("terms", function(x, ...) standardGeneric("terms") )

setMethod("terms", signature=list(x="Performance"),
function ( x, ... ) {
   
   if (is.list(x)) {
      
      l = 1
      ts = list()
      for (p in x) {
         t = p$getActivatedTerms(...)
         if (is.list(t)) ts[[l]]=t$labels else ts[[l]] = NA
         l = l + 1
      }
      if (is.list(ts)) return(ts) else return(NA)
      
   } else {
      ts = NULL
      ts = x$getActivatedTerms(...)
      if (is.list(ts)) return(ts$labels) else return(NULL)
   }
   
}) # terms


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

if (!isGeneric("position")) setGeneric("position", function(x, ...) standardGeneric("position") )

setMethod("position", signature=list(x="Performance"),
function ( x, ... ) {
	
   if (is.list(x)) {
      
      mvecs = NULL
      for (p in x) {
         mvecs = rbind( mvecs, p$getMeaningVector() )
      }
      
	  meaningvector = colSums( mvecs ) / nrow(mvecs)
      
      performance = Performance(name="", logging=FALSE, domain=p$domain)
   	  performance$setMeaningVector(meaningvector)
      performance$terms = performance$getActivatedTerms()
      return(performance)
   
   } else {
      return(x)
   }

}) # position

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

if (!isGeneric("overlap")) setGeneric("overlap", function(x, y) standardGeneric("overlap") )

setMethod("overlap", signature=list(x="Performance", y="missing"),
function ( x ) {
	
   if (is.list(x)) {
      t = terms(x)
      return( names( which( table(unlist(terms(x))) == length(x) ) ) )
   } else {
      stop("This is just a single performance, overlap needs at least two!")
   }
   
}) # overlap

setMethod("overlap", signature=list(x="Performance", y="Performance"),
function ( x, y ) {
	
   return( names( which( table(unlist( c(terms(x),terms(y)))) == 2 ) ) )
   
}) # overlap

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

if (!isGeneric("competences")) setGeneric("competences", function(x) standardGeneric("competences") )

setMethod("competences", signature=list(x="Performance"),
function ( x ) {
   
   ps = x
	if (!is.list(ps)) return(ps) else {
      
      #a = agnes( proximity(ps), diss=FALSE )
      #b = cutree(as.hclust(a), h= 1- x[[1]]$domain$identityThreshold) # added 1-

      a = hclust( as.dist((1+proximity(ps))/2), method="complete")
      b = cutree(as.hclust(a), h=(1+x[[1]]$domain$identityThreshold)/2)
      
      newps = list(NULL)
      d = unique(b)
      for ( i in 1:length(d) ) {
         
         if (length(which(b==d[i]))>1) clustps = ps[ which(b==d[i]) ] else clustps = ps[[ which(b==d[i]) ]]
         if (is.list(clustps)) {
            class(clustps) = "Performance"
            newps[[i]] = position( clustps )
            newps[[i]]$name = paste(unique(lapply(clustps, function(e) e$getName())), sep="", collapse=", ")
         } else {
            newps[[i]] = clustps
         }
         
      }
      
      class(newps) = "Performance"
      return(newps)
   }
   
}) # competences

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

if (!isGeneric("near")) setGeneric("near", function(object, to=NULL, ...) standardGeneric("near") )

setMethod("near", signature=list(object="Performance", to="Performance"),
function ( object, to, threshold=object$domain$proximityThreshold, ... ) {
   
   value = proximity( object, to )
   return( value>threshold )
   
}) # near


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -