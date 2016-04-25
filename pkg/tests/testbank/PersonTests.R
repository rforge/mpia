
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# test Person

cat(" -  -  - Person tests -  -  -\n")

cat("~~~ TEST adding a person fw and readings/writings:\n")
tryCatch({
   
      dmgr = new("DomainManager")
      d = dmgr$get("generic")
      
      fw = new("Person", domain=d, name="fridolin")
      
      fw$read("this is a demo text in business for management purposes", purpose="exam")
      fw$read("this is a second text for demonstration of purposes in management all in business", purpose="exam 2", when = "2013-01-04 11:11:11")
      fw$read("documents can be very different from each other, no matter what domain", purpose="exam", label="label1.txt")
      fw$read("everything can be very different from each other, no matter what domain", purpose="exam", when="2013-03-03 11:11:02")
      fw$write("latent semantic analysis is a strange thing to do. strange awkward and probably forbidden.", purpose="exam")
      fw$write("this is yet another test")
      fw$read("this is yet another test")
      
      l = length(fw$performances)
      mvs = fw$getMeaningVectors()
	   cosines = round(cosine( diag(fw$currentDomain$space$sk) %*% t(mvs) ),1)
   
   	if (l!=7 || dim(mvs)!=c(7,177) || dim(cosines)!=c(7,7) || cosines[4,3]!= 0.8) stop ("Person test did not yield expected values.")
   
	},
	error=function(e) stop ("~~~ !!! FAILED !!!\n"),
	finally=cat("~~~ passed. \n")
)
cat("\n")


cat("~~~ TEST adding a person hans (without domain = searching for generic) and readings/writings:\n")
tryCatch({
   
      hans = new("Person", name="hans")

      hans$read("domain specific texts can be a pain in the neck", purpose="exam")
      hans$write("and this is yet another text with a bit of management, sales, retail stuff in it.", score=1, purpose="exam")
      hans$read("and this is yet double a bit of management, sales, retail stuff in it.", purpose="exam")
      hans$read("free business cars are fantastic, but only when red.", purpose="exam")
      hans$write("there is nothing like a bike that i love more than the colour red.", purpose="exam")

      l = length(hans$performances)
      if (l!=5) stop("not the expected number of readins/writings found.")
   
	},
	error=function(e) stop ("~~~ !!! FAILED !!!\n"),
	finally=cat("~~~ passed. \n")
)
cat("\n")


cat("~~~ TEST adding performances and get their activated terms:\n")
tryCatch({

      fw = new("Person", domain=d, name="fridolin")
      
      fw$read("this is a demo text in business for management purposes", purpose="exam")
      fw$read("this is a second text for demonstration of purposes in management all in business", purpose="exam 2", when = "2013-01-04 11:11:11")
      fw$read("using strange words in documents can involve risk", purpose="exam", label="label1.txt")
      fw$read("business can be very different from each other, no matter whether they are in the same domain or differ", purpose="exam", when="2013-03-03 11:11:02")
      fw$write("change management domain risk.", purpose="exam")
      fw$write("this is yet another test")
      fw$read("this is yet another test")

      l = fw$getActivatedTerms()
	   if (any(is.na(l))) {
      	stop("somehow performances got lost, shouldn't have happend.")
   	}
   
   },
   error=function(e) stop ("~~~ !!! FAILED !!!\n"),
   finally=cat("~~~ passed. \n")
)
cat("\n")



cat("~~~ TEST adding a performance in a different domain and testing whether this is ignored by getMeaningVectors():\n")
tryCatch({
   
      fw$currentDomain = dmgr$get("essays")
      fw$write("this is a test with risk involved", label="odd one")
      fw$currentDomain = dmgr$get("generic")
      
      if ( nrow(fw$getMeaningVectors()) != 7) {
         stop("the odd one was not ignored")
      }
      
   },
   error=function(e) stop ("~~~ !!! FAILED !!!\n"),
   finally=cat("~~~ passed. \n")
)
cat("\n")


cat("~~~ TEST paths:\n")
tryCatch({

      cat(paste("path length before: ",length(path(fw)), ".\n", sep=""))
      #cat(fw$timestamps)
      Sys.sleep(2)
      now = Sys.time()
      Sys.sleep(2)
      fw$write("business management risk change")
      cat(paste("path length after adding: ",length(path(fw)), ".\n", sep=""))
      a = fw$path(from=now, to=Sys.time())
      cat(paste("path length for from/to selection: ",length(a), ".\n", sep=""))
      if ( length(a) != 1) {
         stop("path 'from' to 'to' failed.")
      }
   	rm(a)

      },
      error=function(e) stop ("~~~ !!! FAILED !!!\n"),
      finally=cat("~~~ passed. \n")
)
cat("\n")




