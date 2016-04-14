# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
#       Class: mpia
#
#       fridolin.wild@open.ac.uk
#       last update: August 14, 2013
#
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

mpia <- setRefClass( "mpia",

   fields = list(
      hrmgr = "HumanResourceManager"
      portfolio = "CompetencePortfolio",
      domainmgr = "DomainManager",
   ),

   methods = list(

      # init
      initialize = function() {
         
         domainmgr <<- new("DomainManager")
         hrmanager <<- new("HumanResourceManager", domainmgr)
         portfolio <<- new("CompetencePortfolio", hrmanager)
         
         invisible(NULL)
         
      }

   ) # methods
) # Class Performance

mpia$methods(
persons = function() {
   return(hrmanager.all())
}) # method: persons()

mpia$methods(
competences = function() {
   return(portfolio.all())
}) # method: competences()

mpia$methods(
domains = function() {
   return(domainmgr.all())
}) # method: domains()


lw_lighten <- function ( quotient=1 ) {
   return( function(e) { e*quotient } )
}
