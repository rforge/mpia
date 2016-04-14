
# -  -  -  -  -  -  -  -  -  -  -  -
# test introspection

cat("\n\n-  -  - introspection tests -  -  -\n")

cat("~~~ TEST inspect person for traces:\n")
tryCatch({
   
   	fw = Person(name="fridolin")
   	fw$write("this is a test")
      summary(fw)
      pos = fw$position()
      summary(pos)
   
	},
	error=function(e) stop ("~~~ !!! FAILED !!!\n"),
	finally=cat("~~~ passed. \n")
)
cat("\n")

