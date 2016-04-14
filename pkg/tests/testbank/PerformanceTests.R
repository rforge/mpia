

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# testing Performance

cat(" -  -  - Performance tests -  -  -\n")

dmgr = new("DomainManager")
d = dmgr$get("generic")

m = new("Performance", "this is a business test for managing purposes", purpose="exam", domain=d)
if (mean(m$getMeaningVector()) > 0) print ("Performance: isolated 'New' passed.") else stop("Performance: isolated 'New' failed.")

# clean up
rm(dmgr)
rm(d)
rm(m)
gc()
