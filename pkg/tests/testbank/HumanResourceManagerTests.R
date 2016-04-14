# -  -  -  -  -  -  -  -  -  -  -  -
# test HumanResourceManager

cat("\n\n")
cat("-  -  - HumanResourceManager tests -  -  -\n")

cat("~~~ TEST adding people, listing, adding performances:\n")
tryCatch({

	   dmgr = DomainManager()
   
      ppl = new("HumanResourceManager", domainmanager=dmgr)
      ppl$all()

      #ppl$ls()
      #ppl$collect()$fw
      #ppl$collect(name="fw")

      joanna = ppl$add(name="joanna")
      ppl$add(name="max")

      ppl$all()
      ppl$last()

      joanna$read("this is a business management risk text.")
      max = ppl$getPersonByName("max")
      max$write("this is a text with a giraffe not sure if the word is in risk business change though")

	},
	error=function(e) stop ("~~~ !!! FAILED !!!\n"),
	finally=cat("~~~ passed. \n")
)
cat("\n")

cat("~~~ TEST flushing positions in a particular domain:\n")
tryCatch({
   
      a = max$position()
      b = joanna$position()
      c = (max == joanna)
   
      before = dim(ppl$currentDomain$traces)
	   beforej = length(joanna$positions)
      ppl$flushPositions()
   	after = dim(ppl$currentDomain$traces)
      afterj = length(joanna$positions)
   
	   if (before[1]==after[1]) stop ("position traces were not removed!")
	   if (beforej==afterj) stop ("joanna's position traces were not removed!")
   
      before = dim(ppl$currentDomain$traces)
   
      max$currentDomain = dmgr$get("businessfull")
      max$write("risk management text")
      a = max$position()
      max$currentDomain = dmgr$get("generic")
      ppl$flushPositions()
      after = dim(ppl$currentDomain$traces)
   
   	if (before[1]!=after[1]) stop ("position traces were not removed!")
      
	},
	error=function(e) stop ("~~~ !!! FAILED !!!\n"),
	finally=cat("~~~ passed. \n")
)
cat("\n")

cat("~~~ TEST performances(person), terms(person), terms(performances(person)[[1]]), names(person):\n")
tryCatch({
   
      a = performances(fridolin)[[3]]
      names(a)
      names(a) <- "TEST"
      c = names(a)
   	if (c!="TEST") stop("setting name for performance failed")
      
      names(fridolin) = "joanna"
      if (names(fridolin) != "joanna") stop("setting name for person failed")
   	names(fridolin) = "fridolin"
   
   	if ( terms(fridolin)[[1]][1] != "business" ) stop("getting terms for person failed.")
   	if ( terms(a)[3] != "understanding") stop ("getting terms for performance failed.")
      
   },
   error=function(e) stop ("~~~ !!! FAILED !!!\n"),
   finally=cat("~~~ passed. \n")
)
cat("\n")


cat("~~~ TEST path by timestamp:\n")
tryCatch({

   fridolin = Person(name="fridolin")
   
   fridolin$write("management test business", when="2011-12-31 01:00:00", label="1")
   fridolin$write("management risk business", when="2011-12-31 00:00:00", label="2")
   fridolin$write("money learning change improve wealth social", when="2012-12-31 00:00:00", label="3")
   fridolin$write("management test business", when="2012-01-31 05:00:00", label="4")
   
	order1 = unlist(lapply( path(fridolin), function(e) e$name ))
   order2 = unlist(lapply( performances(fridolin), function(e) e$name ))
   
   if (all(order2 == order1)) stop("timestamp sorting of full path failed.")
   
   if ( !all( unlist(lapply( path(fridolin, ix=4:2), function(e) e$name )) == unlist(lapply( path(fridolin, ix=2:4), function(e) e$name ))))
   	stop ("timestamp sorting for partial index failed.")
   
   if ( !all( unlist(lapply( path(fridolin, from="2012-01-01 00:00:00", to=Sys.time()), function(e) e$name )) == c(4,3)))
   	stop("timestamp sorting for from/to index failed.")
   
   
},
error=function(e) stop ("~~~ !!! FAILED !!!\n"),
finally=cat("~~~ passed. \n")
)
cat("\n")

