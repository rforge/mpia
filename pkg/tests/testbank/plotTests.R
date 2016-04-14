
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# DEV PLOT PERSON

dmgr = DomainManager()
d = dmgr$get("generic")
ids = dmgr$upgrade(force=TRUE)
dmgr$flush(d)

dmgr = DomainManager()
d = dmgr$get("generic")

source("testbank/PersonGenerateTraces.R")

plot(d, method="topographic", rotated=FALSE)
toponymy(d, method="mountains")

pf = path(fridolin)
cs = gray(seq(0.5,0,length.out=length(pf) ))
for (p in 1:length(pf)) {
	plot(pf[[p]], col=cs[p])
}

plot(fridolin, col="red")
plot(path(fridolin), col="green", label=FALSE)
plot(position(fridolin), col="yellow")

plot(fridolin[1:3], col="green", label=FALSE)
plot(position(fridolin[1:3]), col="green", label=TRUE)
plot(position(path(fridolin, 1:3)), col="red", label=FALSE)

terms(fridolin[3] + fridolin[2])
plot(fridolin[1]+fridolin[2]+fridolin[3])

plot(path(ou), col="green", label=FALSE)

# important thought: probably better to calculate a visualisation of the doc netcoords as a projection plane
# this way, the positions are ordered by similarity to the performance meaning vectors cosine proximities
# rather than the term cosine proximities -> gives more clear picture of what the actual utterance is similar too
# otherwise there is a risk that all person positions are in the centre - and all performance positions are
# close to the centre as well, if the texts are long enough to cover enough terms

# descriptor labels could then be put down the same way: taking the terms activated by the most central docs per grid cell (dtm)
# and putting them down to the closest loading doc or the focal point -> repeating of term labels is allowed then

# or do a dendrogram cutoff over an agnes and add the resulting centroid points to the netcoords calculation?
# -> this one has been implemented in the generic function "competence"


