
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# essays

dmgr = DomainManager()
data(essays.domain)
essays.domain

plot(essays.domain, method="topographic")
toponymy(essays.domain, method="mountains")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# add student performances (in this case: essays from exams)

ppl = HumanResourceManager(domainmanager=dmgr, domain=essays.domain)

data(essays)
data(essays.students)
data(essays.scores)

for (i in 1:length(essays)) {
   p = ppl$add( name=essays.students[i] )
   p$perform( essays[i], activity="exam", purpose="exam", score=essays.scores[i])
}

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# group detection

essays.domain$identityThreshold = 0.5 # lower identity threshold to get larger groups

groups(ppl)
lapply(ppl$groups, function(e) e$getName())

# plot group positions

for (i in 1:length(ppl$groups)) {
   plot(position(ppl$groups[[i]]), col="pink", component.labels=FALSE, label=TRUE)
}

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# detect competences

plot(essays.domain, method="persp", rotated=TRUE)
toponymy(essays.domain, method="mountains")
plot(competences(ppl), col="green", connect=FALSE, label=F, component.labels=FALSE)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# plot positions of the students

plot(essays.domain, method="topographic")
toponymy(essays.domain, method="gridprestige")

for (i in 1:length(ppl$people)) {
   plot(position(ppl$people[[i]]), col="green", component.labels=FALSE, label=TRUE)
}

