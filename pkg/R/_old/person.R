
# package: MPIA
# created by f.wild@open.ac.uk
# on June 18, 2013

setClass("Person",
  representation(
    name = "character",
    DateTimeStamp = "POSIXct",
    id = "character"
    #,"VIRTUAL"
  )
) # Class person
 
if (!isGeneric("name")) {
  if (is.function("name"))
    fun <- name
  else
    fun <- function(object) standardGeneric("name")
  setGeneric("name", fun)
}
setMethod("name", "Person", function(object) object@name)
setGeneric("name<-", function(x, value) standardGeneric("name<-"))
setReplaceMethod("name", "Person", function(x, value) {
  x@name <- value
  x
})

if (!isGeneric("DateTimeStamp")) {
  if (is.function("DateTimeStamp"))
    fun <- DateTimeStamp
  else
    fun <- function(object) standardGeneric("DateTimeStamp")
  setGeneric("DateTimeStamp", fun)
}
setMethod("DateTimeStamp", "Person", function(object) object@DateTimeStamp)
setGeneric("DateTimeStamp<-", function(x, value) standardGeneric("DateTimeStamp<-"))
setReplaceMethod("DateTimeStamp", "Person", function(x, value) {
  x@DateTimeStamp <- value
  x
})

if (!isGeneric("id")) {
  if (is.function("id"))
    fun <- id
  else fun <- function(object) standardGeneric("id")
  setGeneric("id", fun)
}
setMethod("id", "Person", function(object) object@id)
setGeneric("id<-", function(x, value) standardGeneric("id<-"))
setReplaceMethod("id", "Person", function(x, value) {
  x@ID <- value
  x
})

setMethod("initialize",
          "Person",
          function(.Object, name=NULL) {
            name(.Object) = name
            id(.Object) = 1
            DateTimeStamp(.Object) = now()
          }
)

###

persons <- setClass("persons", slots = c(ids="numeric"))
setMethod("initialize",
          "persons",
          function(.Object) {
            .Object
          }
)

setMethod("add", "persons",
          function(.Object, name=NULL, id=NULL) {
            if(nargs() > 0) {
 
              if(is.null(id)) {
                id = (max(.Object@ids)+1)
              } 
              .Object@ids = c(.Object@ids, id)
              
              if(is.null(name)) {
                name = paste(person, id, sep=".")
              } 
              .Object@names = c(.Object@names, name)
              
            } # if nargs > 0
          } # function
          
) # setMethod add



t1 <- person(name = "fridolin")


setMethod("[[<-", c("stampedEnv", "character", "missing"),
          function(x, i, j, ..., value) {
            ev <- as(x, "environment")
            ev[[i]] <- value  #update the object in the environment
            x@update <- Sys.time() # and the update time
            x})

setMethod("[", "track",
          function(x, i, j, ..., drop) {
            x@x <- x@x[i]; x@y <- x@y[i]
            x
          })

t1 <- new("person", name = "fridolin")

