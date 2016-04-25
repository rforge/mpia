
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# demo: social network analysis
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

# 1) create a demo data set

im = matrix(0, nrow=9, ncol=12)

rownames(im) = c("Paul", "Joanna", "Maximilian", "Peter", "Christina", "Simon", "Ida", "Thomas", "Alba")

colnames(im) = c("OU-CS", "UR-Informatik", "MOOC-PED", "MOOC-TEL", "MOOC-Math", "OU-PED", "MOOC-ocTEL",
"MOOC-LAK", "OU-Statistics", "Facebook-Statistics", "Facebook-TEL", "Linkedin-CS")

im[1, ] = c(1,1,0,0,0,0,0,0,0,0,0,1) # paul
im[2, ] = c(0,0,1,0,0,1,0,0,0,0,1,0) # joanna
im[3, ] = c(0,0,0,0,1,0,0,0,0,1,0,0) # max
im[4, ] = c(0,0,0,1,0,1,1,1,0,0,0,0) # peter
im[5, ] = c(0,0,1,1,0,1,1,1,0,0,0,0) # christina
im[6, ] = c(0,1,0,0,0,0,0,0,0,0,0,1) # simon
im[7, ] = c(0,0,0,0,0,0,0,0,1,1,0,0) # ida
im[8, ] = c(0,0,0,0,0,0,0,0,1,1,0,0) # thomas
im[9, ] = c(0,0,0,1,0,0,1,1,0,0,0,0) # alba

# 2) plot sociograms

par(mar=c(3,2,3,2))
par(mfrow=c(2,2))
par(cex.lab=0.6)

plot(network(im%*%t(im)), displaylabels=T, vertex.cex=2, main="Affiliations") # three communities

colors = c(rep("red",nrow(im)),rep("white",ncol(im)))
plot(network(im), displaylabels=T, vertex.col=colors, vertex.cex=2, main="Incidences") # the communities are: TEL, CS, Math

# 3) manipulate data: merge courses

TEL = c(	"OU-CS", "UR-Informatik", "Linkedin-CS", "MOOC-PED","OU-PED", "MOOC-TEL","MOOC-ocTEL","MOOC-LAK", "Facebook-TEL")
STATS = c("MOOC-Math", "OU-Statistics", "Facebook-Statistics")
im_new = cbind(
	rowSums( im[, TEL] ),
	rowSums( im[, STATS] )
)
colnames(im_new) = c("ALL-TEL", "ALL-STATS")

# 4) plot sociograms (again)

plot(network(im_new %*% t(im_new), directed=FALSE), displaylabels=TRUE, vertex.cex=2, main="Affiliations (manipulated)")

colors = c(rep("red",nrow(im)),rep("white",ncol(im)))
plot(network(im_new>0, directed=FALSE), displaylabels=TRUE, vertex.col=colors, vertex.cex=2, main="Incidences (manipulated)")


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# demo: latent semantic space
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -


# create a demo data set

docs = matrix(nrow=0, ncol=2)
colnames(docs) = c("id", "title")

docs = rbind( docs, c("c1", "a web interface for social media applications")) # web social interface
docs = rbind( docs, c("c2", "Review of access time restrictions on web system usage")) # web access review system time user
docs = rbind( docs, c("c3", "Content management system usage of the HTML 5 interface")) # interface system html management
docs = rbind( docs, c("c4", "Error spotting in HTML: social system versus software system")) # social 2x system html
docs = rbind( docs, c("c5", "Barriers to access and time spent in social mobile apps")) # access time social

docs = rbind( docs, c("m1", "The generation of random unordered trees")) # trees
docs = rbind( docs, c("m2", "A survey of divisive clustering along the intersection of partial trees")) # trees clustering intersection
docs = rbind( docs, c("m3", "Width and height of trees in using agglomerative clustering with Agnes")) # trees clustering agglomerative
docs = rbind( docs, c("m4", "Agglomerative clustering algorithms: a review")) # clustering agglomerative review

docs = rbind( docs, c("p1", "The intersection of learning and organisational knowledge sharing"))
docs = rbind( docs, c("p2", "A transactional perspective on teaching and learning"))
docs = rbind( docs, c("p3", "Innovations in online learning: moving beyond no significant difference"))
docs = rbind( docs, c("p4", "Tacit knowledge management in organisational learning"))
docs = rbind( docs, c("p5", "Knowledge building: theory, pedagogy, and technology"))

# create doc term matrix

docs2 = docs[,2]
docs2 = tolower(docs2)
docs2 = gsub("[^[:alnum:]]", " ", docs2)
docs2 = gsub("[[:space:]]+", " ", docs2)
docs2 = lapply(docs2, function (e) { unlist(strsplit(e, " ")) })
data(stopwords_en)
docs2 = lapply(docs2, function (e) { e[! (e %in% stopwords_en) ] })
tabs = lapply(docs2, function(e){sort(table(e), dec=T)})
tabs2 = lapply(tabs, function(e) { data.frame(docs = "", terms = names(e), Freq = e, row.names = NULL) })
for (i in 1:nrow(docs)) { tabs2[[i]][,1]= docs[i,1] }

dtm = t(xtabs(Freq ~ ., data = do.call("rbind", tabs2)))
dtm = dtm[-which(rowSums(dtm)<=1),] # at least in more than a single document
dtm

# create lsa space with 3 dimensions

space = lsa(dtm, dims=3)
dtm2 = as.textmatrix(space)
class(dtm2) = c("matrix", "textmatrix")

# get a bit of info on which cells are properly filled (>=0.3)

round(((dtm2>=0.3) * 1) * dtm2,1)

# measure proximity between docs

prox = cosine(dtm2)

round(prox) # shows 3 clear clusters

# compare with original proximity in pure vector space

cosine(dtm)

# visualise difference between these tables

par(mfrow=c(1,2))
par(mar=c(2,2,1,1))

image(round(1-cosine(dtm),1), col=gray(1:10/10), xaxt="n", yaxt="n")
axis(1, at=seq(0/ncol(dtm), 1, by=1/(ncol(dtm)-1))[1:ncol(dtm)], labels=colnames(dtm), cex.axis=0.7)
axis(2, at=seq(0/ncol(dtm), 1, by=1/(ncol(dtm)-1))[1:ncol(dtm)], labels=colnames(dtm), cex.axis=0.7)

image(round(1-cosine(dtm2),1), col=gray(1:10/10), xaxt="n", yaxt="n")
axis(1, at=seq(0/ncol(dtm2), 1, by=1/(ncol(dtm2)-1))[1:ncol(dtm2)], labels=colnames(dtm2), cex.axis=0.7)
axis(2, at=seq(0/ncol(dtm2), 1, by=1/(ncol(dtm2)-1))[1:ncol(dtm2)], labels=colnames(dtm2), cex.axis=0.7)

# plot it using the plot proposal from the seminal paper on LSA (Deerwester et al., 1989)

cf = 1
xmax = max(c(space$tk[,1], space$dk[,1]))
xmin = min(c(space$tk[,1], space$dk[,1]))
ymax = max(c(space$tk[,2], space$dk[,2]))
ymin = min(c(space$tk[,2], space$dk[,2]))
zmax = max(c(space$tk[,3], space$dk[,3]))
zmin = min(c(space$tk[,3], space$dk[,3]))
ymin = ymin - 0.05
xmin = xmin - 0.05
zmin = zmin - 0.05
ymax = ymax + 0.05
xmax = xmax + 0.05
zmax = zmax + 0.05
mdisty = (ymax-ymin)*0.02
mdistx = (xmax-xmin)*0.02
mdistz = (zmax-zmin)*0.02

plot(space$tk[,1], space$tk[,2], ylim=c(ymin, ymax), xlim=c(xmin, xmax), xlab="factor 1", ylab="factor 2", pch=1, cex=cf, lwd=cf, cex.axis=cf, cex.lab=cf, col="red")
points(space$dk[,1], space$dk[,2], pch=2, col="blue")
text(space$dk[,1]+mdistx, space$dk[,2]-mdisty, labels=rownames(space$dk), col="blue", cex=0.7 )
text(space$tk[,1]-mdistx, space$tk[,2]+mdisty, labels=rownames(space$tk), col="red", cex=cf )
abline(h=0, lty="dotted", lwd=cf)
abline(v=0, lty="dotted", lwd=cf)


# plot with all 3 factors in a perspective plot (and label)

par(mar=c(1,0.1,0.1,0.1))
p = persp(
   x=-1:1,y=-1:1,
   z=matrix(
   c(
      -1,0,1,
      -1,0,1,
      1,0,-1
   ),3,3),
   col="transparent", border="transparent",
   xlim = range(c( space$dk[,1], space$tk[,1])),
   ylim = range(c( space$dk[,2], space$tk[,2])),
   zlim = range(c( space$dk[,3], space$tk[,3])),
   theta=35, phi=20,
   xlab="dim 1", ylab="dim 2", zlab="dim 3",
   expand=0.5, scale=F,
   axes=TRUE, nticks=10, ticktype="simple"
)

points( trans3d(space$dk[,1], space$dk[,2], space$dk[,3], pmat=p), bg="darkgray", col="darkgray", pch=22, cex=1)
points( trans3d(space$tk[,1], space$tk[,2], space$tk[,3], pmat=p), bg="black", col="black", pch=21, cex=1)

text(trans3d(space$tk[,1]-mdistx, space$tk[,2]-mdisty, space$tk[,3]-mdistz, pmat=p), rownames(space$tk), col="black", cex=0.8)
text(trans3d(space$dk[,1]-mdistx, space$dk[,2]-mdisty, space$dk[,3]-mdistz, pmat=p), rownames(space$dk), col="darkgray", cex=0.8)


# fold-in demo

query("Review of the html user interface of the system", rownames(dtm))



# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# mpia space

dmgr = DomainManager()
id = dmgr$add(space, title="memospace")
d = dmgr$get("memospace")

# create data (binding training description -> person attending)

dmsg = c( 1,2,10,3,6,11,4,7,8,9,12,5 )
cbind( docs[dmsg,1], as.matrix(colnames(im)))

thedocs = cbind( docs[dmsg,1], as.matrix(colnames(im)), docs[dmsg,2])
ppl = HumanResourceManager(domainmanager=dmgr, domain=d)
for (p in rownames(im)) {
   assign(tolower(p), ppl$add(name=p))
   for (pf in which(im[p,]>0)) {
      #get(tolower(p))$write(docs[dmsg,][pf,2], label=docs[dmsg,][pf,1], purpose=colnames(im)[pf])
      get(tolower(p))$write(thedocs[pf,3], label=thedocs[pf,1], purpose=colnames(im)[pf])
   }
}

thedocs2 = cbind( as.matrix(colnames(im)),docs[dmsg,2] )
rownames(thedocs2) =  docs[dmsg,1]
colnames(thedocs2) = c("Training", "Description")
thedocs2 = thedocs2[sort(rownames(thedocs2), index.return=TRUE)$ix, ]

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# visual analysis: first demo

par(mfrow=c(2,2))

plot(d, method="persp", rotated=TRUE)
par(mar=c(1,1,1,1))
title(main="The MPIA space")
toponymy(d, method="all", add=TRUE, col="black")

plot(d, method="persp", rotated=TRUE)
par(mar=c(1,1,1,1))
title(main="Peter's path (of performance records)")
toponymy(d, method="all", add=TRUE, col="black")
plot(path(peter), col="white")

# two performance records are identical!
peter[1] == peter[3]

plot(d, method="persp", rotated=TRUE)
par(mar=c(1,1,1,1))
title(main="Peter's competences and position")
plot(competences(peter), col="red", component.labels=FALSE, connect=FALSE, alpha=1)
plot(position(peter), col="orange")

plot(d, method="persp", rotated=TRUE)
par(mar=c(1,1,1,1))
title(main="Peter's learning path (in shades of gray)")
pf = path(peter)
cs = gray(seq(1,0.5,length.out=length(pf) ))
for (p in 1:length(pf)) {
	plot(pf[[p]], col=cs[p])
}

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# investigate positions and paths

d$visualiser$closeDevice()
d$visualiser$newDevice()

plot.new()
par(mar=c(0,0,0,0))
toponymy(d, method="all", add=FALSE, col="darkgray", grid.col="black")

plot(peter, col="darkgreen")
plot(christina, col="darkgreen")
plot(path(maximilian), col="orange")
plot(maximilian, col="orange")
plot(path(joanna), col="red")
plot(joanna, col="red")
plot(path(alba), col="purple")
plot(alba, col="purple")
plot(ida, col="lightgreen")
plot(simon, col="brown")

proximity(simon, alba)
near(simon, alba)

proximity(simon, joanna)
near(simon, joanna)

near(ppl, christina)

replacements = sapply(near(ppl, christina), names)
round( proximity(ppl)['Christina', replacements], 1 )

overlap(path(christina), path(alba))
overlap(path(christina), path(peter))

overlap(competences(christina), competences(alba))
overlap(competences(christina), competences(peter))

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# detect potential competences

plot.new()
toponymy(d, method="all", add=FALSE, col="darkgray", grid.col="black")

plot(competences(christina), col="red", connect=FALSE)
plot(competences(peter), col="green", connect=FALSE)
plot(competences(simon), col="yellow", connect=FALSE)

plot(competences(ppl), col="purple", alpha=0.3, connect=FALSE)

# show cluster dendrogram

ps = performances(ppl)
a = agnes( proximity(ps), diss=FALSE )
plot(a, which.plots=2, main="Cluster dendrogram over all meaningvectors", xlab="Meaningvectors", sub="", cex.main=1.2)
abline(h=1-d$identityThreshold, lty="dashed", col="darkgreen", lwd=1.5)
text(x=11, y=1-d$identityThreshold+0.2, adj=0.5, "cutoff", col="darkgreen", cex=1)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# detect groups

plot(d, method="topographic", rotated=FALSE)
toponymy(d, method="all", col="black")

groups(ppl)

# show how group competences differ
plot(competences(ppl), col="green", connect=FALSE)

# show group positions
for (i in 1:length(ppl$groups)) {
   plot(position(ppl$groups[[i]]), col="pink")
}
