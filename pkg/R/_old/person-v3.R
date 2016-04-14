
library(R.oo)
library(lsa)
library(tm)

setwd("~/Documents/werkstatt/mpia-package/R/")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
# testing Meaning

source("Meaning.R")
m = new("Meaning", "this is a business test for managing purposes", purpose="exam", domain="business")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
# test Person

source("Person.R")

fw = new("Person", money=20, name="fridolin")
hans = new("Person", money=0, name="hans")
hans$borrow(fw, 10)

fw$read("this is a demo text in business for management purposes", purpose="exam")
fw$read("this is a second text for demonstration of purposes in management all in business", purpose="exam")
fw$read("documents can be very different from each other, not matter what domain", purpose="exam")
fw$read("everything can be very different from each other, no matter what domain", purpose="exam")
hans$read("domain specific texts can be a pain in the neck", purpose="exam")
dim(fw$textmatrix)

#plot.space
#plot.person ...

# -  -  -  -  -  -  -  -  -  -  -  -  
# test PersonManager()

source("PersonManager.R")

p = new("PersonManager")
p$ls()
p$elements()$fw$money
p$elements()[[1]]$name
p$elements("fw")$money
p$elements(name="fw")$money
sapply(p$elements(c("fw", "hans"))[1:2], function(e) { e$money} )
