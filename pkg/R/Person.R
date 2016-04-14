
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
#       Class: Person
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  

Person <- setRefClass( "Person",
                       
	fields = list(

      name="character",

      performances="ANY", # now: vector with Performance Objects; was: the Dk context vectors in the given space
		positions = "ANY",

      activityType="ANY",
      scores="ANY",
      timestamps="ANY",
      labels="ANY",

      currentDomain="ANY",
      logging="logical"

	),

	methods = list(

      # init
      initialize = function( name, domain=NULL ) {
         
         name <<- name

         if ( missing(domain) || is.null(domain) || class(domain)!="Domain" ) {
            domainmgr = new("DomainManager")
            currentDomain <<- domainmgr$get("generic")
         } else {
            currentDomain <<- domain
         } # remember the domain
         
         .self$performances = NULL
         positions <<- NULL
         
         activityType <<- NULL
         scores <<- NULL
         timestamps <<- NULL

         logging <<- TRUE

         invisible(NULL)
      },
                             
      print = function () {
   	   cat(paste("A person with name '", name, "' and ", length(.self$performances), " textual traces.\n", sep=""))
      },

      show = function () {
	      cat(paste("A person with name '", name, "' and ", length(.self$performances), " textual traces.\n", sep=""))
      }

	) # methods
) # Class Person


# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# MPIA get/set methods

Person$methods(
  setCurrentDomain = function(dom){
    currentDomain <<- dom
    invisible(TRUE)
  }
)

Person$methods(
   getDomains = function(){
      domains = NULL
      for (p in .self$performances) {
      	domains = c(domains, p$getDomain())
      }
      return(domains)
   }
)

Person$methods(
getPurposes = function(){
   purposes = NULL
   for (p in .self$performances) {
      purposes = c(purposes, p$getPurpose())
   }
   return(purposes)
}
)


Person$methods(
	getName = function(){
   	return(name)
	}
)

Person$methods(
   setName = function( value ){
      .self$name = value
   }
)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# logging of 'raw' texts

Person$methods(
  setSourceLogging = function(x=TRUE){
    if (missing(x)) stop("[Person] Error: setSourceLogging() requires TRUE (=activate) or FALSE (=deactivate) values for whether to store the raw texts internally in $txt.")
    logging <<- x
    invisible(TRUE)
  }
)

Person$methods(
	getSourceTexts = function(){
      sources = NULL
		for (p in .self$performances) {
         sources = c(sources, p$getSourceText())
      }
		return( sources )
	}
)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# getMeaningVectors: return space vectors using the space traces rowindex values

Person$methods(
getMeaningVectors = function( ix=NULL ){

   docvecs = NULL
   n = NULL
   l = 1
   for (p in .self$performances) {
      
      if (p$domain$signature == currentDomain$signature) {
         if ( missing(ix) || (l %in% ix) ) {
            mv = p$getMeaningVector()
            docvecs = rbind(docvecs, mv)
            n = c(n, rownames(mv))
         }
      } else {
         cat(paste("Ignoring performance (with name='",p$name,"', domain='",p$domain$name,"'): not in current domain.\n", sep=""))
      }
      l = l + 1
      
   }
   rownames(docvecs) = n
   return( docvecs )
   
})

# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# getActivatedTerms()

Person$methods(
	getActivatedTerms = function( ix=1:length(.self$performances) ){

      if (is.null(ix)) ix = 1:length(.self$performances)
      #cat(paste("fetching terms activated for requested ", ix, " performances.\n", sep=""))

      acts = list()
      l = 1
      for (p in .self$performances[ix]) {
      	acts[[l]] = p$getActivatedTerms()
         l = l+1
      }
      
      #cat(l)
      
		return( acts )
      
	}
) # method: getActivatedTerms()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# perform activities
# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -


# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# perform()

Person$methods(
   perform = function( txt, purpose=NULL, activity=NULL, score=NULL, when=Sys.time(), label=NULL ) {
      
      if (!is.null(label)) title = label
      else if (!is.null(purpose)) title = paste(length(.self$performances)+1, ": ", purpose, "(", activity,")", sep="")
      else title = "nameless"
      
      cat(paste("Person '", .self$name, "' performed a meaningful activity (", activity, ").\n", sep=""))
      
      performance = Performance( text=txt, purpose=purpose, domain=.self$currentDomain, logging=logging, name=title)
      .self$performances = c(.self$performances, performance )
      
      activityType <<- c(activityType, activity)
      scores <<- c(scores, score)
      
      if (!is.character(when)) when = as.character((strptime(when, "%Y-%m-%d %H:%M:%S")))
      timestamps <<- c(timestamps, when)
      
      if (all(is.na(performance$terms))) {
         cat (paste("WARNING: No terms active above threshold (=",currentDomain$proximityThreshold,").\n", sep=""))
      } else {
         cat (paste("about: ",paste(performance$terms$labels, collapse=", ", sep=""),".\n", sep=""))
      }
      
      invisible(performance)
      
   }
) # method: perform

# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# read()

Person$methods(
	read = function( txt=NULL, performance=NULL, purpose=NULL, when=Sys.time(), label=NULL ) {

      if (!missing(performance)) {
         
         if (!is.null(label)) cat("WARNING: ignoring label, since performance handed over.\n")
         .self$performances = c(.self$performances, performance )
         
         activityType <<- c(activityType, "read")
         scores <<- c(scores, NULL)
         
         if (!is.character(when)) when = as.character((strptime(when, "%Y-%m-%d %H:%M:%S")))
         timestamps <<- c(timestamps, when)
         p = performance
         
      } else {
      
         if (!is.null(label)) title=label
         else if (!is.null(purpose)) title = paste(length(.self$performances)+1, ": text read for ", purpose, sep="")
         else title = "text read"
         
         p = .self$perform(txt, purpose, activity="read", when=when, label=label )
         
      }
      invisible(p)

	}
) # method: read()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# write

Person$methods(
   write = function( txt, purpose=NULL, score=NULL, when=Sys.time(), label=NULL ) {
    
      if (!is.null(label)) title = label
      else if (!is.null(purpose)) title = paste(length(.self$performances)+1, ": text written for ", purpose, sep="")
      else title = "text written"
    
      p = .self$perform(txt, purpose, activity="write", score=score, when=when, label=label )
      invisible(p)
     
  }
) # method: write()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# positioning
# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# position(): return Performance object with current position, internally store in positions

Person$methods(
	position = function( when=NULL ) {
   
      if ( is.null(.self$performances) || length(.self$performances)==0 ) {
         stop("[Person] position(): Cannot calculate current position as there are no performances tracked (yet).")
      }

      title = paste("Position of ", name, "", sep="")
      performance = Performance(name=title, logging=FALSE, domain=currentDomain)

      if (!is.null(when)) {
         
      	if (!is.character(when)) when = as.character((strptime(when, "%Y-%m-%d %H:%M:%S")))
	    when = as.POSIXct(when)
   	    ix = which(as.POSIXct(timestamps) <= when)
         
	    if (length(ix)==0) stop (paste("[Person] position(): No performances found up to ", strftime(when, "%B %d, %Y (%H:%M)"),".", sep=""))
         
   	    cat(paste("~ position(): taking into account ", length(ix), " item(s) up to (including) ", strftime(when, "%B %d, %Y (%H:%M)"),".\n", sep=""))
         
        ps = .self$performances[ix]
        mvecs = NULL
        for (p in ps) {
	         if (p$domain$signature == .self$currentDomain$signature) {
             	mvecs = rbind( mvecs, p$getMeaningVector() )
           } else {
               cat("~ position(): ignoring performace (not in current Domain)\n")
           }
        }
         
      } else {
         
         mvecs = as.matrix(.self$getMeaningVectors())
         
      }

      meaningvector = colSums( mvecs ) / nrow(mvecs)
      performance$setMeaningVector(meaningvector)

      positions <<- c(positions, performance)
      return(performance)
   
   }
) # method: position()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# lastPosition(): return Performance object with current position, internally store in positions

Person$methods(
   lastPosition = function() {
      
      if (is.null(positions) || length(positions)==0) {
         last = .self$position()
      } else {
         last = .self$positions[[ length(.self$positions) ]]
      }
      return(last)
      
   }
) # method: lastPosition()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# path(): return Performance object with current position, internally store in positions

Person$methods(
	path = function( ix=NULL, from=NULL, to=Sys.time() ) {
      
      # add sorting by timestamps!!
      
      if (!missing(ix) && !is.null(ix)) {
         
         p = performances(.self)[ix]
         
			resort = sort(.self$timestamps[ix], dec=FALSE, index.return=TRUE)
         p = p[resort$ix]
         class(p) = "Performance"
         
         return(p)
         
      } else if( !missing(to) ) {

         resort = sort(.self$timestamps, dec=FALSE, index.return=TRUE)
         tstmps = .self$timestamps[resort$ix]
         
         #timestamps = .self$timestamps
         
         if (!is.character(to)) to = as.character((strptime(to, "%Y-%m-%d %H:%M:%S")))
         to = as.POSIXct(to)
         
         if (missing(from)) {
            #ix = which(as.POSIXct(.self$timestamps) <= to)
            ix = which(as.POSIXct(tstmps) <= to)
            p = performances(.self)[resort$ix][ix]
            class(p)="Performance"
	         return(p)
         } else {
            if (!is.character(from)) from = as.character((strptime(from, "%Y-%m-%d %H:%M:%S")))
            from = as.POSIXct(from)
            #ix = which( which(as.POSIXct(.self$timestamps)<=to) %in% which(as.POSIXct(.self$timestamps)>=from) )
            ix = which( which(as.POSIXct(tstmps)<=to) %in% which(as.POSIXct(tstmps)>=from) )
            p = performances(.self)[resort$ix][ix]
            class(p) = "Performance"
	         return(p)
         }
         
      } else {
         
         resort = sort(.self$timestamps, dec=FALSE, index.return=TRUE)
         p = performances(.self)[resort$ix]
         class(p)="Performance"
         return(p)
         
      }
	}
) # method: path()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# setMethods for generics (existing ones)
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

#if (!isGeneric("inspect")) setGeneric("inspect", function(x, ...) standardGeneric("inspect") ) # , where=asNamespace("tm")

setMethod("summary", signature=list(object="Person"),
function ( object, ... ) {
   
	ats = object$getActivatedTerms()
   l = 1
   for (termlist in ats) {
      
		cat (paste("Performance ",l,": '", names(object$performances[[l]]), "'.\n\n", sep=""))
      
      source = object$performances[[l]]$getSourceText()
      cat (paste("  source: '",substr(source,1,80),if(nchar(source)>80) "...", "'\n", sep=""))
      
      if (is.list(termlist)) {
         cat (paste("  about: ",paste(termlist$labels[1:min(8, length(termlist$labels))], collapse=", ", sep=""), if(length(termlist$labels)>8) ", ...","\n", sep=""))
         
      } else cat("  about: no terms activated\n")
      
      cat (paste("  purpose: '", object$performances[[l]]$purpose, "'\n\n", sep=""))
      
      l = l+1
   }
   
}) # summary

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

setMethod("==", signature=list(e1="Person", e2="Person"),
function ( e1, e2 ) {
   
   x = e1$position()$getMeaningVector()
   y = e2$position()$getMeaningVector()
   
   if (e1$currentDomain$signature != e2$currentDomain$signature) stop ("The persons are in different current domains!")
   
   termVectors = e1$currentDomain$space$tk %*% diag(e1$currentDomain$space$sk)
   dtm = crossprod(t(crossprod(t(e1$currentDomain$space$tk), diag(e1$currentDomain$space$sk))), t(rbind(x,y)))
   
   cos = cosine(dtm)
   
	return( cos[2,1]>e1$currentDomain$identityThreshold )
   
}) # ==

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

setMethod("names", signature=list(x="Person"),
function ( x ) {

   if (is.list(x)) {
      a = sapply( x, names )
   	return( a )
   } else {
		return(x$getName())
   }

}) # names

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

# and for 'names(x) <-'
setReplaceMethod("names", signature=list(x="Person", value="character"),
function ( x, value ) {
	x$setName(value)
   return(x)
}) # names<-

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

setMethod("plot", signature=list(x="Person"),
function ( x, ... ) {
   if (is.list(x)) {
   	  for (p in 1:length(x)) {
      	plot(x[[p]]$position(), ...)
      }
   } else {
	   plot(x$position(), ...)
   }

}) # plot

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

setMethod("[", signature=list(x="Person", i="ANY"),
function ( x, i ) {
   if (length(i)==1) {
		return(x$performances[[i]])
   } else {
      ps = x$performances[i]
      class(ps) = "Performance"
      return(ps)
   }
   
}) # "["

setMethod("show", signature=list(object="Person"),
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

setMethod("print", signature=list(x="Person"),
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
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# setMethods for generics (new ones)
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

if (!isGeneric("performances")) setGeneric("performances", function(x, ...) standardGeneric("performances") )

setMethod("performances", signature=list(x="Person"),
function ( x, purpose, activity, ... ) {

   if (!is.null(x$performances)) {
      
      ps = x$performances
      ix = NULL
      if (!missing(purpose))
	      for (l in 1:length(ps)) {
            if (ps[[l]]$purpose == purpose) ix = c(ix, l)
         }
      else if (!missing(activity)) {
         ats = x$activityType
         for (l in 1:length(ats)) {
   	      if (ats[l] == activity) ix = c(ix, l)
	      }
      }
      else ix = 1:length(ps)
      
      ps2 = ps[ix]
	   class(ps2) = "Performance"
	   if (is.list(ps2)) return(ps2) else return(invisible(NULL))
   } else {
      return(invisible(NULL))
   }
   
}) # performances

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

if (!isGeneric("competences")) setGeneric("competences", function(x) standardGeneric("competences") )

setMethod("competences", signature=list(x="Person"),
function ( x ) {

   ps = performances(x)
   return( competences(ps))
   
}) # competences

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

if (!isGeneric("path")) setGeneric("path", function(x, ...) standardGeneric("path") )

setMethod("path", signature=list(x="Person"),
function ( x, ... ) {
   p = x$path(...)
   if (is.list(p)) class(p) = "Performance"
	return(x$path(...))
}) # path

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

if (!isGeneric("position")) setGeneric("position", function(x, ...) standardGeneric("position") )

setMethod("position", signature=list(x="Person"),
function ( x, ... ) {
   if (is.list(x)) {
      ps = list()
      for (l in 1:length(x)) {
         ps[[l]] = position(x[[l]])
      }
      class(ps) = "Performance"
      if (length(ps)>0) return(ps) else return(NULL)
   } else {
		return(x$position(...))
   }
}) # position

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

if (!isGeneric("terms")) setGeneric("terms", function(x, ...) standardGeneric("terms") )

setMethod("terms", signature=list(x="Person"),
function ( x, ... ) {

   ps = x$performances
   ts = list()
   l = 1
   if (is.list(ps)) {
      for (p in ps) {
         ls = p$getActivatedTerms(...)
         if (is.list(ls)) {
          	ts[[l]] = ls$labels
         } else ts[[l]] = NA
         names(ts)[l] = p$getName()
         l = l+1
      }
   }
   if (is.list(ts)) return(ts) else return(FALSE)
   
}) # terms

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

if (!isGeneric("near")) setGeneric("near", function(object, to=NULL, ...) standardGeneric("near") )

setMethod("near", signature=list(object="Person", to="Person"),
function ( object, to, threshold=object$currentDomain$proximityThreshold, ... ) {
   
   value = proximity( object, to )
   return( value>threshold )
   
}) # near

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

setMethod("near", signature=list(object="Performance", to="Person"),
function ( object, to, threshold=object$domain$proximityThreshold, ... ) {

   value = proximity( object, to$position() )
   return( value>threshold )
   
}) # near

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

setMethod("near", signature=list(object="Person", to="Performance"),
function ( object, to, threshold=object$currentDomain$proximityThreshold, ... ) {
   
   value = proximity( to, object )
   return( value>threshold )
   
}) # near


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
