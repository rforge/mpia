
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# essays

#dmgr = DomainManager()
#dmgr$tempdir = "~/Documents/werkstatt/mpia-package/cache/"
#d = dmgr$get("essayscoring")

dmgr = DomainManager()
d = dmgr$get("businessgeneric")

plot(d, method="topographic")
toponymy(d, method="mountains")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# add student performances (in this case: essays from exams)

ppl = HumanResourceManager(domainmanager=dmgr, domain=d)

data(essays)
data(essays.students)
data(essays.scores)

for (i in 1:length(essays)) {
   p = ppl$add( name=essays.students[i] )
   p$perform( essays[i], activity="exam", purpose="exam", score=essays.scores[i])
}

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# group detection

d$identityThreshold = 0.5 # lower identity threshold to get larger groups

groups(ppl)
lapply(ppl$groups, function(e) e$getName())

# plot group positions

for (i in 1:length(ppl$groups)) {
   plot(position(ppl$groups[[i]]), col="pink", component.labels=FALSE, label=TRUE)
}

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# detect competences

plot(d, method="persp", rotated=TRUE)
toponymy(d, method="mountains")
plot(competences(ppl), col="green", connect=FALSE, label=F, component.labels=FALSE)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# plot positions of the students

plot(d, method="topographic")
toponymy(d, method="gridprestige")

for (i in 1:length(ppl$people)) {
   plot(position(ppl$people[[i]]), col="green", component.labels=FALSE, label=TRUE)
}


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# evaluate scoring accuracy #broken (goldstandards not included in corpus!)!!

#essay2essay = proximity(performances(ppl))
#rownames(essay2essay) = rownames(essays.scores)
#colnames(essay2essay) = rownames(essays.scores)
#goldstandard = c( "data6_golden_01.txt", "data6_golden_02.txt", "data6_golden_03.txt" )
#machinescores=NULL
#for (i in 1:nrow(essay2essay)) machinescores[i] = max( essay2essay[goldstandard, i] )
#names(machinescores) = rownames(essay2essay)
#cor.test(humanscores[,1], machinescores, exact=FALSE, method="spearman", alternative="two.sided")

