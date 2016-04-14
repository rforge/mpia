# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
#       Class HumanResourceManager
#       
#       fridolin.wild@open.ac.uk
#       last update: August 7, 2013
#       
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

HumanResourceManager <- setRefClass( "HumanResourceManager",
                              
   fields = list(
   	envir="environment",
		people="ANY",
		groups="ANY",
		domains="ANY",
		currentDomain="ANY"
   ),

   methods = list(

   	initialize = function( domainmanager=NULL, domain=NULL, environment=parent.frame() ) {
         
   		envir <<- environment
         people <<- NULL
         groups <<- NULL
         
         domains <<- domainmanager
         
         if (missing(domain) || is.null(domain)) {
         	currentDomain <<- domains$get("generic")
         } else {
            if (class(domain)=="Domain") currentDomain <<- domain
            else if (is.character(domain)) currentDomain <<- domains$get(name=domain)
         }
         
		},

		print = function() {
         cat("  a HumanResourceManager.")
      },

		show = function() {
         cat(paste("  a HumanResourceManager caring for ", length(people), " people.", sep=""))
      }

   ) # methods

) # Class Person

HumanResourceManager$methods(
	ls = function( environment=parent.frame()) {
   	return( objects(env=environment)[sapply(objects(env=environment),function(x){class(get(x))}) == "Person"] )
	}
) # add method ls

# collect method: to pick up Persons from any environment and move them into the scope
# of the Human Resource Manager: move objects in environment, to prevent double indexing

HumanResourceManager$methods(
  collect = function( varname, name=NULL, environment=parent.frame() ) {
    
    # if name is given, try to resolve variable name
    if (!is.null(name)) {
      persons = objects(env=environment)[sapply(objects(env=environment),function(x){class(get(x))}) == "Person"]
      if (any(persons==name)) varname=persons[which(persons==name)]
    }
    
    # if variable name is not given, get all, otherwise get the specified one
    if ( missing(varname) ) {
      return( sapply( objects(env=environment)[sapply(objects(env=environment),function(x){class(get(x))})=="Person"], function(e){eval.parent(parse(text=e))}) )
    } else if (length(varname)==1) {
      return( eval.parent(parse(text=varname)) )
    } else {
      return( sapply( varname, function(e){eval.parent(parse(text=e))}) )
    } # all
    
  }
) # add method collect people from environment

HumanResourceManager$methods(
	add = function( name, domain=.self$currentDomain ) {
      #d = domains$get(name=domain)
   	people <<- c(people, new("Person", name, domain))
	   return(people[[length(people)]])
	}
) # method: add person

HumanResourceManager$methods(
   remove = function( id, name ) {
      if (!missing(id)) {
      	people <<- people[-id]
      } else if (!missing(name)) {
         n = 1
         id = NULL
         for (p in people) {
            if (p$name == name) id = n
            n = n+1
         }
         if (!is.null(id)) people <<- people[-id]
      } else {
         invisible(FALSE)
      }
      invisible(TRUE)
   }
) # method: remove person

HumanResourceManager$methods(
	getPersonByName = function( name=NULL ) {
		if (missing(name)) stop("need a name to find a person.")
      for (p in people) {
         if (p$name == name) return(p)
      }
	}
) # method: getPersonByName

HumanResourceManager$methods(
	all = function() {
		return(people)
	}
) # method: all

HumanResourceManager$methods(
	last = function() {
   	return(people[[length(people)]])
	}
) # add method all

HumanResourceManager$methods(
   flushPositions = function( domain=.self$currentDomain ) {

      removeTraces = NULL
      for (p in people) {

         if (!is.null(p$positions)) {
            
            # remove positions also from domain trace!
            
            l = 1
            toremove = NULL
            for (pos in p$positions) {
               if (pos$domain$signature == domain$signature) {
               	removeTraces = c(removeTraces,pos$meaningvector)
	               #cat(paste("will remove trace ", pos$meaningvector, " in domain ", pos$domain$name), "\n", sep="")
	               toremove = c(toremove, l)
               }
	            l = l + 1
            }
            
            if (!is.null(toremove)) {
               p$positions = p$positions[-toremove]
               #cat(paste("would remove persons positions now", toremove,"\n", sep=""))
            } else {
               cat("No traces for this person.\n")
            }
            
         } else {
            cat("Person has no positions.\n")
         }

      } # for all people
      
      #cat(paste("These traces would be removed: ", paste(removeTraces, collapse=" ", sep=""), "\n", sep=""))
      if (!is.null(removeTraces)) domain$traces = domain$traces[-removeTraces,]
      
   }
) # method: flushPositions()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# groups: group all persons, store in $groups

HumanResourceManager$methods(
	findGroups = function() {
   
      positions = NULL
      for (p in .self$people) positions = c(positions, p$position())
      
      prox = matrix(0, nrow=length(positions), ncol=length(positions))
		for (i in 1:length(positions)) {
         for (l in 1:length(positions)) {
            if (l==i) prox[l,i] = 1
            else {
               prox[l,i] = proximity(positions[[l]], positions[[i]])
            }
         } # for l
      } # for i

      rownames(prox) = names(ppl)
      colnames(prox) = rownames(prox)
      
      #a = agnes( prox, diss=FALSE )
      #b = cutree(as.hclust(a), h= 1- .self$currentDomain$identityThreshold) # added 1-
      
      a = hclust( as.dist((1+prox)/2), method="complete")
      b = cutree(as.hclust(a), h=(1+.self$currentDomain$identityThreshold)/2)

      .self$groups = list(NULL)
      d = unique(b)
      for ( i in 1:length(d) ) {
			
         if (length(which(b==d[i]))>1) clustps = .self$people[ which(b==d[i]) ] else clustps = .self$people[[ which(b==d[i]) ]]
         if (is.list(clustps)) {
            .self$groups[[i]] = Person( name=paste(unique(lapply(clustps, function(e) e$getName())), sep="", collapse=", "), domain=.self$currentDomain )
            .self$groups[[i]]$performances = unlist( lapply(clustps, function(e) return(e$performances)) )
         } else {
            .self$groups[[i]] = clustps
         }
         
      } # for all persons
      cat(paste("Identified ", length(.self$groups), " group(s) among the ", length(.self$people), " persons.\n"))
      
      return( .self$groups )

	}
) # method: groups

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# setMethods for generics (new ones)
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

if (!isGeneric("performances")) setGeneric("performances", function(x, ...) standardGeneric("performances") )

setMethod("performances", signature=list(x="HumanResourceManager"),
function ( x, ... ) {
   
   if (!is.null(x$people)) {
      ps = list()
      for (p in x$people) {
         ps = c(ps, performances(p, ...))
      }
	   class(ps) = "Performance"
	   if (is.list(ps)) return(ps) else return(invisible(NULL))
   } else {
      return(invisible(NULL))
   }
   
}) # performances

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

if (!isGeneric("competences")) setGeneric("competences", function(x) standardGeneric("competences") )

setMethod("competences", signature=list(x="HumanResourceManager"),
function ( x ) {
   
   if (!is.null(x$people)) {
      return( competences(performances(x)) )
   } else {
      return(invisible(NULL))
   }
   
}) # competences

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

if (!isGeneric("groups")) setGeneric("groups", function(x, ...) standardGeneric("groups") )

setMethod("groups", signature=list(x="HumanResourceManager"),
function ( x, ... ) {
   
   if (!is.null(x$people)) {
      return( x$findGroups(...) )
   } else {
      return(invisible(NULL))
   }
   
}) # groups

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

setMethod("names", signature=list(x="HumanResourceManager"),
function ( x ) {
   
   if (!is.null(x$people)) {
      return( unlist(lapply(x$people, function(e) return(e$getName()))) )
   } else {
      return(invisible(NULL))
   }
   
}) # names