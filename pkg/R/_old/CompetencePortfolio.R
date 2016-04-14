# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
#       Class CompetencePortfolio
#
#       fridolin.wild@open.ac.uk
#       last update: August 14, 2013
#
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

CompetencePortfolio <- setRefClass( "CompetencePortfolio",

	fields = list(
		persons="ANY"
	),

	methods = list(

      initialize = function( hrmgr ) {
         
         if (missing(hrmgr) || class(hrmgr) != "HumanResourceManager") {
            stop("  [CompetencePortfolio] init(): requires a Human Resource Manager object to act upon.")
         } else {
            persons <<- hrmgr
         }
         
      },

      print = function() {
         cat("  a CompetencePortfolio.")
      },

      show = function() {
         cat(paste("  a CompetencePortfolio inspecting the Performances of ", length(persons$all()), " persons.", sep=""))
      }


	)
) # Class CompetencePortfolio

CompetencePortfolio$methods(
performances = function( varname, name=NULL ) {
   
   performances = NULL
	for (p in persons$all()) {
      for (performance in p$performances) {
         performances = c(performances, performance)
      }
   }
   #cat(paste("a vector of ", length(performances)," performances.\n", sep=""))
   return(performances)
   
}) # method: performances()

CompetencePortfolio$methods(
all = function() {
   
   perfs = .self$performances()
   pu = NULL
   for (performance in perfs) {
      pu = c(pu, performance$getPurpose())
   }
   if (any(pu=="")) pu[which(pu=="")] = "generic"
   return(unique(pu))
   
}) # method: all()

CompetencePortfolio$methods(
position = function( purpose=NULL ) {
   
   perfs = .self$performances()
   pu = NULL
   for (performance in perfs) {
      pps = performance$getPurpose()
      if (pps == "") pps = "generic"
      if (pps == purpose) pu = rbind(pu, performance$getMeaningVector())
   }
   
   pu = colSums( pu ) / nrow(pu)
   title = paste("Position for competence '", purpose,"'", sep="")
   p = Performance(name=title, logging=FALSE, domain=performance$domain)
   p$setMeaningVector(pu)
   
   return(p)
   
}) # method: all()