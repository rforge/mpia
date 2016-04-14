
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# legacy

lsaCosine <- function(space, type="documents", filter=NULL, docvectors=NULL) {
   
   if (inherits(space,"LSAspace")) {
      
      cos = NULL
      if (type=="terms") {
         
         if(length(filter)>0) {
            space$tk <- try(space$tk[filter,],T)
            if(inherits(space$tk,"try-error")) stop("[lsaCosine] Error filtering terms: filter does not match term labels or indices.")
         }
         mat = space$tk %*% diag(space$sk)
         mat = t(mat)
         
      } else if(type=="documents") {
         
         if(length(filter)>0) {
            space$dk <- try(space$dk[filter,],T)
            if(inherits(space$dk,"try-error")) stop("[lsaCosine] Error filtering documents: filter does not match document labels or indices.")
            mat = diag(space$sk) %*% t(space$dk)
         } else if (!is.null(docvectors)){
            cat("   [lsaCosine] calculating with external docvectors.\n")
            cat(paste( "   [lsaCosine] docvecs dim = (", paste(dim(docvectors), collapse=", ", sep=""), ")\n", sep=""))
            mat = diag(space$sk) %*% t(docvectors)
         }
         
      } else { stop("[lsaCosine] Error: missing or invalid type parameter. Allowed options are 'terms' or 'documens'.") }
      
      return ( cosine(mat) )
      
   } else {
      stop("[lsaCosine] Argument mismatch: 'space' needs to be an object of class 'LSAspace'.")
   } # if no space provided
   
} # function lsaCosine()

