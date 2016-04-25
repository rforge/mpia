

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# testing DomainManager

cat("-  -  - DomainManager tests -  -  -\n\n")


cat("~~~ TEST init:\n")
e = tryCatch({
   
	   dmgr = new("DomainManager")
   	cat("    Cache directory: ", paste(dmgr$tempdir, "\n", sep=""))
   },
	error=function(e) e,
	finally=cat(paste("~~~ passed.\n", sep=""))
)
if (inherits(e, "error")) stop (paste("~~~ !!! FAILED:", e, "\n", sep=""))
cat("\n")


cat("~~~ TEST flushing cache:\n")
	tryCatch({
      
		f = dmgr$flushCache()
      if (f) f = "filled" else f="empty"
	},
	error=function(e) stop ("~~~ !!! FAILED !!!\n"),
	finally = cat(paste("~~~ passed.\n", sep=""))
)
cat("\n")


cat("~~~ TEST provoking domain not found error:\n")
tryCatch({
   
   	dmgr$get("madeupname")
	   show(dmgr) # 0 domains in memory
   
	},
	error=function(e) cat("~~~ passed.\n"),
	finally=function(e) stop("~~~ !!! FAILED !!!\n")
)
cat("\n")


cat("~~~ TEST materialisation:\n")
tryCatch({

      data(generic.domain)
      space = d$space
      id = dmgr$add(space, title="generic")
      dmgr$materialise(id)
      show(dmgr) # 1 domains in memory
   
	},
	error=function(e) stop ("~~~ !!! FAILED !!!\n"),
	finally=cat("~~~ passed.\n")
)
cat("\n")


cat("~~~ TEST loading domain by name:\n")
tryCatch({
   
	   dmgr = new("DomainManager")
	   gc()
	   dmgr$get("generic")
		show(dmgr) # 1 domain in memory
	},
	error=function(e) stop ("~~~ !!! FAILED !!!\n"),
	finally=cat("~~~ passed.\n")
)
cat("\n")


cat("~~~ TEST adding a second space (businessfull):\n")
tryCatch({
   
      data(essays.domain)
      id = dmgr$add(essays.domain, title="essays")
      dmgr$materialise(id)
      
      dmgr = new("DomainManager")
      dmgr$get(name="essays")
   
      test = dir(dmgr$tempdir) # should show at least one file with the id

	},
	error=function(e) stop ("~~~ !!! FAILED !!!\n"),
	finally={
      if (!(id %in% test)) {
         stop ("Domain Manager: space materialisation test failed. cannot write to cache directory?")
      } else {
         cat ("~~~ passed. \n")
      }
	}
)
cat("\n")

cat("~~~ TEST upgrading of all Domains in memory:\n")

tryCatch({
   
   dmgr = DomainManager()
   
   d = dmgr$get("generic")
   dmgr$flush(d)
   rm(d)
   
   dmgr = DomainManager()
   
   d = dmgr$get("generic")
   d = dmgr$get("essays")
   ids = dmgr$upgrade(force=TRUE)
   
   if (length(ids)==0) stop("No domains upgraded, should have been two!")
   
	},
	error=function(e) stop ("~~~ !!! FAILED !!!\n"),
	finally=cat("~~~ passed.\n")
)
cat("\n")

# clean up memory
rm(dmgr)
rm(space)
rm(spaceorig)
rm(id)
rm(test)
gc()