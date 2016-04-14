# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
#       Class DomainManager
#
#       fridolin.wild@open.ac.uk
#       last update: August 7, 2013
#
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

DomainManager <- setRefClass( "DomainManager",

	fields = list(

		domains="ANY",
		signatures="ANY",

      tempdir="character",
		caching="logical",
		remoting="logical"

	),

	methods = list(

		initialize = function( domain=NULL ) {
         
         domains <<- vector("list", length=0)
         
         signatures <<- NULL
         
         ### NOT CONFORM!!! has to be in tempdir!!!
         ### add a setCacheDir() method to ease access!!!
         
         mpiapath = path.package("mpia", quiet=TRUE)
         if (is.null(mpiapath)) {
          	# mpiapath = "~/Documents/werkstatt/mpia-package"
            mpiapath = tempdir()
         }
         tempdir <<- path.expand(paste(mpiapath, "/cache/", sep=""))
         
         caching <<- TRUE
         remoting <<- TRUE
         
         if (!missing(domain)) {
         	.self$get(name=domain)
         }
         
		},

		print = function() {
   		cat("Object of class 'DomainManager'")
		},

		show = function () {
		   cat(paste("Object of class 'DomainManager' with ", length(domains), " domains currently in memory.\n", sep=""))
		}

	)
) # Class DomainManager


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# check status of a given domain (true=exists)

DomainManager$methods(
status = function(id=NULL) {
   
   if (id %in% signatures) {
      
      cat("This domain is already in memory.\n")
      return (TRUE)
      
   } else {
      
      cached = dir(tempdir)
      if (id %in% cached) {
         cat ("Domain is in local cache.")
         return (TRUE)
      }
      
      if (remoting) {
      
         con = url(paste("http://crunch.kmi.open.ac.uk/people/~fwild/services/getSpaceStatus.rws?spaceid=", id,sep=""))
         available = as.logical(readLines(con))
         close(con)
         
         if (available) {
            cat("Domain is available on cRunch.\n")
            return(TRUE)
         }
         
      } # if remoting
      
   }
   
   return(FALSE)
   
}
) # add method status


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# cache domains to local storage

DomainManager$methods(
materialise = function( id ) {

   ids = id

   for (id in ids) {
   
      if ((length(signatures)>0) && (id %in% signatures)) {
         
         domain = domains[[ which(id == signatures) ]]

         n = as.character(domain$getName())
         tmp2 = paste( tempdir, n, sep="")
         tmp = paste( tempdir, id, sep="")
         
         existed = ""
         if (file.exists(tmp) || file.exists(tmp2)) existed = "overwriting " else existed="stored to "
         
         save(id, file=tmp2)
         save(domain, file=tmp)
         
         dims = "none"
         if (!is.null(domain$visualiser$netcoords)) {
            dims = dim(domain$visualiser$netcoords)[1]
         }
         cat(paste("Domain (name='", n, "') ", existed, "local cache (nr of netcoords=",dims,").\n", sep=""))
         invisible(TRUE)
         
      } else {
         stop(paste("DomainManager$materialise(): Could not find domain with id '", id, "'.\n", sep=""))
         invisible(FALSE)
      }
      
   } # for all ids handed over
   
}) # method: materialise()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# update Domain objects in cache to latest class definition

DomainManager$methods(
	upgrade = function( force=FALSE ) {
      
      d = Domain$new(name="")
      v = Visualiser$new(d)
      
      version = d$version
      vver = v$version
      rm(d)
      rm(v)
      
      if (length(domains)==0) stop("upgrade(): No domains in memory")
      
      ids = NULL
      
      for (l in 1:length(domains)) {
         d = domains[[l]]
			if ( force || d$version != version || d$visualiser$version != vver ) {
            
            ids = c(ids, d$signature)
            #cat(paste( "Updating domain '",d$name,"' from version ", d$version," to ", version, "\n", sep=""))
            #summary(d)
            d2 = d$copy(shallow=FALSE)
            d2$version = version
            d2$visualiser$version = vver
            
            cat(paste( "Updated domain '",d2$name,"' from version ", d$version," to version ", d2$version, " (visualiser from version ", d$visualiser$version, " to ", d2$visualiser$version, ").\nSummary of new object: ", sep=""))
            summary(d2)
            
            domains[[l]] <<- d2
            if (caching) {
               materialise(d2$signature)
            }

         } # if outdated version or forced
      } # for all domains

      invisible(ids)
      
   }
) # method: updateClasses


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# delete all files in local cache directory

DomainManager$methods(
	flushCache = function() {
      cachedFiles = dir(tempdir, full.names=TRUE)
      if (length(cachedFiles)>0) {
         unlink(cachedFiles)
         cat("Flushed cache.\n")
         invisible(TRUE)
      } else {
         cat("Cache was already empty.\n")
         invisible(FALSE)
      }
	}
) # method: flushCache()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# remove domains from memory

DomainManager$methods(
   flush = function( domain ) {
      if (is.null(domains) || length(domains)==0) {
         cat("Nothing to flush: no domains in memory.\n")
      } else {
         if (missing(domain)) {
         	cat("Removing all domains from memory.\n")
            domains <<- NULL
            signatures <<- NULL
         } else {
            cat("Removing specified domain(s) from memory.\n")
            ix = which(signatures == domain$signature)
            domains <<- domains[-ix]
            signatures <<- signatures[-ix]
            a = gc()
         }
         
      } # if domains in memory
   }
) # method: flushCache()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# get: find a particular domain object by signature or by Name

DomainManager$methods(
	get = function(name="generic", id=NULL) {
      
      if (!missing(id)) {
         
         if (id %in% signatures) {
            
            cat("Domain already in memory (found by id).\n")
            domain = domains[[ which(.self$signatures == id) ]]
	         return( domain )
            
	      } else {

            #cat ("Domain is not in memory, checking registry ...\n")
            
	         # check locally, then online for space with signature id
            domain = .self$retrieve(id=id)
            
            if (class(domain)=="Domain") {
             
               #cat("Domain found in cache.")
	            domains <<- c(domains, domain)
   	         signatures <<- c(signatures, domain$signature)

         	   # non sense, since it comes from cache:
               # if (caching) .self$materialise(domain$signature)
            	
	            # return it
			      return( .self$get(id=id) )
               
            } else {
               stop("[DomainManager] get(): cannot find such domain in the registry (by id).")
            }
            
         } 
         
   	} else if (!missing(name)) {
         
         n = NULL
			for (d in domains) {
            n = c(n, d$name)
         }
         if (!is.null(domains) && (length(domains)>0) && name %in% n) {
            
            cat("Domain already in memory (found by name).\n")
            domain = domains[[ which(n==name) ]]
	         return( domain )
            
         } else {
            
            #cat(paste("Domain is not in memory under this name: '", name, "', checking cache...\n", sep=""))
            
            domain = .self$retrieve(name=name)
            if (class(domain)=="Domain") {
               
               #cat("~ get(): found with class 'Domain'!\n")
               
	            domains <<- c(domains, domain)
   	         signatures <<- c(signatures, domain$signature)
               
               #cat(paste("signature was: ", domain$signature, "\n", sep=""))

               # return it
			      return( domains[[ length(domains) ]] )
               
            } else {
               stop("[DomainManager] get(): cannot find such domain in the registry (by name).")
            }
            
         } # checking cache
         
      } else {
         
         warning( "get(): No name or id given to identify the desired domain, assuming 'generic' ...\n" )
	      return( get(name="generic") )
         
      }
      
	}
) # method: get()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# retrieve Domain from cRunch

DomainManager$methods(
	retrieve = function( id=NULL, name=NULL ) {
      
      if (!missing(id)) {
         
         #cat("~ retrieve(): checking local cache...")
         cached = dir(tempdir)
         if ((length(cached)>0) && id %in% cached) {
            
            cat("Domain found in local cache (by id).\n")
	         a = load( paste(tempdir, id, sep="") )
 				if (a != "domain") stop ("[DomainManager] retrieve(): error while loading domain object from cache: cache file did not contain object 'domain'.")
            
            return(domain)
            
         } else {
            
            cat("retrieve(): Domain not found in local cache: checking on cRunch (to be implemented)...")
            # this is where a server could be contacted.
            #load("~/Documents/werkstatt/knowledge-cartography/data/space80pcnt.RData")
            #domain = new("Domain", name)
      	   #domain.setSpace(space)
            #cat("  found one.\n")
	         invisible(FALSE)
            
         }
         
      } else if (!missing(name)) {
         
         #cat(paste("~ retrieve(): checking local cache (by name='",name,"')...", sep=""))
         
         cached = dir(tempdir)
         if ((length(cached)>0) && name %in% cached) {
                        
            a = load( paste(tempdir, name, sep="") )
            if (a != "id") stop ("error while loading domain identifier object from cache: cache file did not contain object 'id'.")
            
            fn = paste(tempdir, id, sep="")
            if (!file.exists(fn)) stop ("error while loading domain object from cache: cache file does not exist.")
            a = load(fn)
            if (a != "domain") stop ("error while loading domain object from cache: cache file did not contain object 'domain'.")
            if (class(domain) != "Domain") stop("error while loading domain object from cache: object is not of class 'Domain'.")
            
            cat(paste("Domain found in local cache (file=",id,").\n", sep=""))
 	         return(domain)
            
         } else {
            cat ("Domain not found.\n")
		      invisible(FALSE)
         }
         
      } else {
         warning("No name or ID given, assuming generic...\n")
         return ( retrieve(name="generic") )
      }
      
      return(FALSE)

	}
) # method: retrieve()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# create new space for a domain

DomainManager$methods(
	add = function(x, title="generic") {
      
      domain = NULL
      if (class(x) == "Domain") {
         
			domain = x
         id = domain$signature
         
      } else {
         
         domain = Domain$new(name=title)
         if ( class(x) == "LSAspace") {
            
            success = domain$setSpace(x)
            if (!success) stop("DomainManager$add(): x is an LSAspace, but adding failed.")
            id = domain$signature
            
         } else {
            
            # see if Domain.corpus can handle it
            added = domain$corpus(x)
            if ( added ) {
               id = domain.lsa()
            } else {
               stop ("DomainManager$add(): x is not a corpus, adding failed.")
            }
            
         }
         
      } # not a domain, but space or corpus

      if (!is.null(domain)) {
         
         domains[[(length(domains)+1)]] <<- domain
         signatures <<- c(signatures, id)
         cat(paste("Adding a new domain with signature=", id, "\n", sep=""))
         invisible(id)
         
      } else {
         
         stop("DomainManager$add(): adding failed: x is not a Domain, LSAspace, or Corpus object.")
         
      }
      
	}
) # method: add


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# all domains

DomainManager$methods(
	all = function() {
		return(domains)
	}
) # method: all

DomainManager$methods(
   ls = function() {
      ns = NULL
      for (d in domains) {
         ns = c(ns, d$getName())
      }
      return(ns)
   }
) # method: ls()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# last domain

DomainManager$methods(
last = function() {
   d = domains[[ length(domains) ]]
   return( d )
}
) # method: last

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -