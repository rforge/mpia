
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
# mpiaEntity root class

setClass(
  Class="mpiaEntity",
  representation(entities="vector", id="numeric"),
  validity=function(object){
    cat("~~~ mpiaEntity: inspector ~~~ \n")
    if ( length(object@entities)==0 ){
      warning ("[mpiaEntities: no entities so far")
    } else {}
    return(TRUE)
  }
) # class mpiaEntities

setMethod (
  f="initialize",
  signature="mpiaEntity",
  definition=function(.Object){ # , entities
    cat ("~~~~~ mpiaEntities: initializator ~~~~~ \n")
    #if(!missing(entities)){
    #  colnames(traj) <- paste("T",times,sep="")
    #  rownames(traj) <- paste("I",1:nrow(traj),sep="")
    #  .Object@times <- times
    #  .Object@traj <- traj
    #  validObject(.Object) # call of the inspector
    #}
    if (length(.Object@entities)==0) {
      .Object@id = 0
    } else {
      .Object@id = length(.Object@entities)+1
    }
    cat (paste("registering entity:", id, "\n", sep=""))
    enties = c(.Object@entities, .Object)
    cat ("~~~~~ //mpiaEntities: initializator ~~~~~ \n")
    return(.Object)
  }
)


setMethod ("print","Trajectories",
           function(x,...){
             cat("*** Class Trajectories, method Print *** \n")
             cat("* Times ="); print (x@times)
             cat("* Traj = \n"); print (x@traj)
             cat("******* End Print (trajectories) ******* \n")
           }
)

setMethod("show","Trajectories",
          function(object){
            cat("*** Class Trajectories, method Show *** \n")
            cat("* Times = "); print (object@times)
            nrowShow <- min(10,nrow(object@traj))
            ncolShow <- min(10,ncol(object@traj))
            cat("* Traj (limited to a matrix 10x10) = \n")
            if(length(object@traj)!=0){
              print(formatC(object@traj[1:nrowShow,1:ncolShow]),quote=FALSE)
            }else{}
            cat("******* End Show (trajectories) ******* \n")
          }
)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
# Person class

