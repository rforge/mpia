# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
#       Proximity
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# proximity signature functions

setGeneric("proximity", function(a, b, ...) standardGeneric("proximity") )

setMethod("proximity", signature=list(a="Performance", b="Performance"),
  function( a, b, method="cosine" ) {
  
    if (method=="cosine") {
      return( cosine(a,b) )
    } else {
       stop("[proximity.Performance] The only method currently supported is 'cosine'.")
    }
  
  } # proximity.Performance
) # setMethod

setMethod("proximity", signature=list(a="Performance", b="missing"),
function( a, b, method="cosine") {
   
   if (method!="cosine") stop("Only cosine is implemented.")
   
   if (is.list(a)) {

      cosines = matrix(0, nrow=length(a), ncol=length(a))
      for (x in 1:length(a)) {
         for (y in 1:length(a)) {
         	cosines[x,y] = cosine(a[[x]], a[[y]])
         }
      }
      return( cosines )
   } else {
      return(1)
   }
   
} # proximity.Performance
) # setMethod

setMethod("proximity", signature=list(a="Person", b="Person"),
function( a, b, method="cosine") {
  
	if (method=="cosine") {
    
		return( cosine(a,b) )
     
  } else {
    stop("[proximity.Person] The only method currently supported is 'cosine'.")
  }
  
}) # proximity.person

setMethod("proximity", signature=list(a="Performance", b="Person"),
function( a, b, method="cosine") {
   
	if (method=="cosine") {
		return( cosine(a,b) )
   } else {
      stop("[proximity.Person(Performance)] The only method currently supported is 'cosine'.")
   }
   
}) # proximity.person


setMethod("proximity", signature=list(a="HumanResourceManager", b="missing"),
function( a, b, method="cosine") {
   
	if (method=="cosine") {
      
      positions = NULL
      for (p in a$people) positions = c(positions, p$position())
      
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
		
      return(prox)
      
   } else {
      stop("[proximity.Person(Performance)] The only method currently supported is 'cosine'.")
   }
   
}) # proximity.person


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# cosine signature functions


setGeneric("cosine", function( x, y, ... ) standardGeneric("cosine") )

setMethod("cosine", signature=list(x="Person", y="missing"),
function( x ) {

   p = NULL
   p = cosine( diag(x$currentDomain$space$sk) %*% t(x$getMeaningVectors()) )
   return(p)
   
}) # cosine.person

setMethod("cosine", signature=list(x="Person", y="Person"),
function( x,y ) {
   
   if (x$currentDomain$signature != y$currentDomain$signature) stop("Persons are not in same current domain!")
   
   p = NULL
   
   a = x$position()
   b = y$position()
   
   p = cosine( a, b )
   return(p)
   
}) # cosine.person

setMethod("cosine", signature=list(x="Performance", y="Person"),
function( x,y ) {

	if (x$domain$signature != y$currentDomain$signature) stop("Person and Performance are not in the same domain!")
   p = NULL
   
   a = diag(x$domain$space$sk) %*% t(x$getMeaningVector())
   b = diag(x$domain$space$sk) %*% t(y$getMeaningVectors())
   
   p = cosine( as.vector(a), t(as.matrix(b)) )
   return(p)
   
}) # cosine.Person to Performance

setMethod("cosine", signature=list(x="Performance", y="Performance"),
function( x,y ) {

	if (x$domain$signature != y$domain$signature) stop("Performances are not in the same domain!")
   p = NULL
   
   a = diag(x$domain$space$sk) %*% t(x$getMeaningVector())
   b = diag(x$domain$space$sk) %*% t(y$getMeaningVector())
   
   p = cosine( as.vector(a), as.vector(b) )
   return(p[1,1])
   
}) # cosine: two Performances

setMethod("cosine", signature=list(x="matrix", y="missing"),
function( x, ... ) {
   
   if (ncol(x)>1){
   
      co = array(0, c(ncol(x), ncol(x)))
      f = colnames(x)
      dimnames(co) = list(f, f)
      for (i in 2:ncol(x)) {
         for (j in 1:(i - 1)) {
            co[i, j] = cosine(x[, i], x[, j])
         }
      }
      co = co + t(co)
      diag(co) = 1
      return(as.matrix(co))
      
   } else {
      return(1)
   }
   
}) # cosine

setMethod("cosine", signature=list(x="vector", y="vector"),
function( x, y, ... ) {
   
   return(crossprod(x, y)/sqrt(crossprod(x) * crossprod(y)))
   
}) # cosine

# sort out signature probs?

setMethod("cosine", signature=list(x="vector", y="matrix"),
function( x, y, ... ) {
   cat("hu?")
   co = NULL
	for (i in 1:nrow(y)) {
      co = c(co, cosine(x, y[i,]))
   }
   return (co)
   
}) # cosine

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

if (!isGeneric("near")) setGeneric("near", function(object, to=NULL, ...) standardGeneric("near") )

setMethod("near", signature=list(object="HumanResourceManager", to="Person"),
function ( object, to, ... ) {
   
   nearp = list()
   for (p in object$people) {
      if ( near(to, p) ) nearp = c(nearp, p)
   }
   class(nearp) = "Person"
   return(nearp)
   
}) # near

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

setMethod("near", signature=list(object="HumanResourceManager", to="Performance"),
function ( object, to, return.performances=FALSE, ... ) {
   
   nearp = list()
   for (p in object$people) {
      if ( near(to, p) ) nearp = c(nearp, p)
   }
   class(nearp) = "Person"
   return(nearp)
   
}) # near

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
