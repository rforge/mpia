

# BMI examples


setClass("BMI", representation(weight="numeric", size="numeric"))
setMethod("show", "BMI",
          function(object){cat("BMI=",object@weight/(object@size^2)," \n ")}
)

(myBMI <- new("BMI",weight=85,size=1.84))

(herBMI <- new("BMI",weight=62,size=1.60))

setValidity("BMI",
            function(object){if(object@size<0){return("negative Size")}else{return(TRUE)}}
)

new("BMI",weight=85,size=-1.84)

setClass("BMIplus",representation(sex="character"),contains="BMI")
he <- new("BMIplus",size=1.76,weight=84,sex="Male")


# --  --  -- 

setClass(
  Class="Trajectories",
  representation(times="numeric",traj="matrix"),
  validity=function(object){
    cat("~~~ Trajectories: inspector ~~~ \n")
    if(length(object@times)!=ncol(object@traj)){
      stop ("[Trajectories: validation] the number of temporal measurements does not correspond to the number of columns in the matrix")
    }else{}
    return(TRUE)
  }
) # class trajectories

new(Class="Trajectories",times=1:2,traj=matrix(1:2,ncol=2))
new(Class="Trajectories",times=1:3,traj=matrix(1:2,ncol=2))

setMethod (
  f="initialize",
  signature="Trajectories",
  definition=function(.Object,times,traj){
    cat ("~~~~~ Trajectories: initializator ~~~~~ \n")
    if(!missing(traj)){
      colnames(traj) <- paste("T",times,sep="")
      rownames(traj) <- paste("I",1:nrow(traj),sep="")
      .Object@times <- times
      .Object@traj <- traj
      validObject(.Object) # call of the inspector
    }
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

setGeneric (
  name= "countMissing",
  def=function(object){standardGeneric("countMissing")}
)
setMethod(
  f= "countMissing",
  signature= "Trajectories",
  definition=function(object){
    return(sum(is.na(object@traj)))
  }
)

new(Class="Trajectories",times=c(1,2,4,8),traj=matrix(1:8,nrow=2))

# nicer constructor!

tr <- trajectories <- function(times,traj){
  cat ("~~~~~ Trajectories: constructor ~~~~~ \n")
  new (Class="Trajectories",times=times,traj=traj)
}
trajectories(time=c(1,2,4),traj=matrix(1:6,ncol=3))

# missing args

trajectories <- function(times,traj){
  if(missing(times)){times <- 1:ncol(traj)}
  new(Class="Trajectories",times=times,traj=traj)
}

trajectories(traj=matrix(1:8,ncol=4))

##

showMethods(class="Trajectories")



setGeneric("setTimes<-",function(object,value){standardGeneric("setTimes<-")})

setReplaceMethod(
  f="setTimes",
  signature="Trajectories",
  definition=function(object,value){
    object@times <- value
    return (object)
  }
)

setReplaceMethod(
  f="setTimes",
  signature="Trajectories",
  definition=function(object,value){
    object@times <- value
    validObject(object)
    return(object)
  }
)

setMethod(
  f= "[",
  signature="Trajectories",
  definition=function(x,i,j,drop){
    if(i=="times"){return(x@times)}else {}
    if(i=="traj"){return(x@traj)}else {}
  }
)

trajStLouis["times"]

setReplaceMethod(
  f="[",
  signature="Trajectories",
  definition=function(x,i,j,value){
    if(i=="times"){x@times<-value}else{}
    if(i=="traj"){x@traj<-value}else{}
    validObject(x)
    return (x)
  }
)



###

setMethod(
  f="print",
  signature="TrajPartitioned",
  definition=function(x,...){
    callNextMethod() ###
    cat("the object also contains",length(x@listPartitions),"partition")
    cat("\n ***** Fine of print (TrajPartitioned) ***** \n")
    return(invisible())
  }
)

