

#load("../../extended-landauer.RData")
data(mpia.space)
dmgr = DomainManager()
id = dmgr$add(space, title="minimpia")
ids = dmgr$upgrade(force=TRUE)

d = dmgr$get("minimpia")


#load("../../docs.RData")
data(mpia.docs)

#load("../../im.RData")
data(mpia.im)

dmsg = c( 1,2,10,3,6,11,4,7,8,9,12,5 )
cbind( docs[dmsg,1], as.matrix(colnames(im)))

ppl = HumanResourceManager(domainmanager=dmgr, domain=d)
for (p in rownames(im)) {
   assign(tolower(p), ppl$add(name=p))
   for (pf in which(im[p,]>0)) {
      get(tolower(p))$write(docs[dmsg,][pf,2], label=docs[dmsg,][pf,1], purpose=colnames(im)[pf])
      
   }
}

thedocs = cbind( as.matrix(colnames(im)),docs[dmsg,2] )
rownames(thedocs) =  docs[dmsg,1]
colnames(thedocs) = c("Training", "Document Title")
thedocs = thedocs[sort(rownames(thedocs), index.return=TRUE)$ix, ]

