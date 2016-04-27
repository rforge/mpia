
library(mpia)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# restituting messed up character encoding

#corpus = "~/Documents/mpia/essays"
corpus = "~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/essays"

fcorpus = dir(corpus, recursive=TRUE, full.names=TRUE)
fnessays = basename( dir(corpus, recursive=TRUE) )
fenc = NULL
content = NULL
umlauts = NULL
output = NULL
for (f in 1:length(fcorpus)) {
    
    #output = c(output, paste(f, ". trying iso\n", sep=""))
    fn = fcorpus[f]
    readStuff = NULL
    
    tryCatch( {
        fc = file(fn, encoding="iso-8859-1")
        readStuff = paste( readLines(fc, warn=FALSE), collapse=" ")
        close(fc)
    }, error=function(e) output = c(output, paste(f, ". error readLines ISO:",e,"\n", sep="")),
    warning=function(e) output = c(output, paste(f, ". warning readLines ISO:",e,"\n", sep=""))
    )
    
    umlauts[f] = grepl("[äöüÄÖÜß]", readStuff)
    
    if (!umlauts[f]) {
        #output = c(output, paste(f, ". trying utf8\n", sep=""))
        tryCatch( {
            fc = file(fn, encoding="UTF8")
            readStuff = paste( readLines(fc, warn=FALSE), collapse=" ")
            close(fc)
        }, error=function(e) output = c(output, paste(f, ". error readLines UTF8:",e,"\n", sep="")),
        warning=function(e) output = c(output, paste(f, ". warning readLines UTF8:",e,"\n", sep="")))
        umlauts[f] = grepl("[äöüÄÖÜß]", readStuff)
    }
    
    if (!umlauts[f]) output = c(output,paste(f, ". no umlauts after utf8\n", sep=""))
    
    fenc[f] = Encoding(readStuff)
    content[f] = readStuff
}
lost=which(!umlauts)

essays.content = cbind(fnessays, content)

write.csv2(essays.content, file="~/Documents/mpia/essays.content.csv", row.names=FALSE)
write(essays.content, file="~/Documents/mpia/essays.content.RData")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# create document term matrix

a = proc.time()

setwd("~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/")

essays.content = read.csv2(file="essays.content.csv", stringsAsFactors=FALSE)

tm = Corpus(
        VectorSource(essays.content[,2]),
        readerControl=list(
            reader=readPlain, language="de",
            load=TRUE, removePunctuation=TRUE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE
        )
    )

tm = tm_map(tm, function(e) return(gsub("[^a-zA-Z0-9äöüÄÖÜß]", " ", e)))
tm = tm_map(tm, tolower)

# save full dictionary for stem completion
dict = Terms(DocumentTermMatrix(
	tm,
	control=list(removePunctuation=TRUE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE)
))

length(dict)

# stemming
tm = tm_map(tm, stemDocument, language="ger")
dtm = TermDocumentMatrix(tm, control = list(
    removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE,
    minWordLength = 3, bounds = list(global=c(1,Inf))
))

dim(dtm)

# stem completion
sc = as.character( stemCompletion(rownames(dtm), dictionary=dict, type="shortest") )
sc[which(is.na(sc))] = rownames(dtm)[which(is.na(sc))]

rownames(dtm) = sc
if (any(duplicated(rownames(dtm)))) {
    
    dupes = which(duplicated(rownames(dtm)))
    for (i in dupes) {
        
        #cat(paste("removing dupe for ", sc[i], "\n", sep=""))
		hits = which(sc == sc[i])
        target = hits[ which(! hits %in% which(duplicated(sc))) ]
        replvec = t(as.matrix( colSums(as.matrix(dtm[ hits, ])) ))
        rownames(replvec) = sc[target]
        dtm[ target,1:length(replvec) ] = replvec
        
    }
    dtm = dtm[!duplicated(rownames(dtm)),]
}
class(dtm) = c("TermDocumentMatrix", class(dtm))

dim(dtm)

if (any(rownames(dtm) == "")) {
    cat("removing empty ones")
    dtm = dtm[-(which(rownames(dtm) == "")), ]
}

dim(dtm)


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# check for outliers: plot freq dist with chosen upper/lower threshold window

freqs = rowSums(as.matrix(dtm))

# commented for performance benchmark: just uncomment!

#plot(sort(freqs, dec=T), type="l", log="xy", ylab="frequency (log-scaled)", xlab="terms (log-scaled)")

#greater10 = length(which(freqs > ncol(dtm)/10))
#greater25 = length(which(freqs > ncol(dtm)/4))
#greater50 = length(which(freqs > ncol(dtm)/2))

#justonce = length(which(freqs>1))
#justtwice = length(which(freqs>2))
#just5 = length(which(freqs>5))

#rect(greater25,ncol(dtm)/4, justtwice, 2, lty="dotted", col=rgb(0.3,0.3,0.3, alpha=0.1))

#text(greater10+20, 10+ncol(dtm)/10, paste(greater10, "> 10%", sep=" "), col="darkgreen", cex=0.7)
#abline(h=ncol(dtm)/10, lty="dashed", col="darkgreen")

#text(greater25+15, 20+ncol(dtm)/4, paste(greater25, "> 25%", sep=" "), col="darkgreen", cex=0.7, font=2)
#abline(h=ncol(dtm)/4, lty="dashed", col="darkgreen")

#text(greater50+2, 20+ncol(dtm)/2, paste(greater50, "> 50%", sep=" "), col="darkgreen", cex=0.7)
#abline(h=ncol(dtm)/2, lty="dashed", col="darkgreen")

#abline(v=justonce, lty="dotted", col="darkred")
#text(justonce-400, 1, paste(justonce,">1",sep=" "), col="darkred", cex=0.7)

#abline(v=justtwice, lty="dotted", col="darkred")
#text(justtwice-300, 2, paste(justtwice,">2",sep=" "), col="darkred", cex=0.7, font=2)

#abline(v=just5, lty="dotted", col="darkred")
#text(just5-50, 4, paste(just5,">5",sep=" "), col="darkred", cex=0.7)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# filtering by threshold

lower = which(freqs>1)
dtm = dtm[lower,]

freqs = rowSums(as.matrix(dtm))
upper = which(freqs < ncol(dtm)/4)
dtm = dtm[upper,]

empty = as.integer( which(colSums(as.matrix(dtm))==0) )
dtm = dtm[,-(empty)]

# t2t size:
nrow(dtm)^2 * 8 / 1024 / 1024
# 15.97449 MB

dtm = as.matrix(dtm)

proc.time() - a

class(dtm) = "textmatrix"

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# lsa

a = proc.time()
space = lsa(dtm, dims=dimcalc_raw())
proc.time()-a


# superfast
tr = sum(dtm*dtm)
tr
# 20887

tr * 0.8
# 16709.6

#save(space, file="~/Documents/mpia/space-essaysonly-raw.RData")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# find ideal truncation (recalc tr or use from above)

# check if svd was calculated with enough dimensions (here: true, since dimcalc_raw() was used)
if ( ! tr*0.8<sum(space$sk^2) ) stop("svd has too few factors!")

# find cutoff
threshold = tr * 0.8
r = 0
for (i in 1:length(space$sk)) {
    r = r + (space$sk[i]^2)
    if (r >= threshold) {
        cutoff = i-1
        break()
    }
}

cutoff
# 109 for 0.8

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# plot eigenvalue distribution

# with 80/20 area under curve
qplot(1:length(space$sk),  space$sk*space$sk, geom="path", ylab="eigenvalues", xlab="index") + geom_vline(xintercept=cutoff, col="darkgray", linetype=2) + geom_area(aes(1:cutoff,  (space$sk*space$sk)[1:cutoff]), bg=hsv(0.3,0.8,0.8, alpha=0.5)) + geom_area(aes(cutoff:length(space$sk),  (space$sk*space$sk)[cutoff:length(space$sk)]), bg=hsv(0.6,0.8,0.8, alpha=0.5)) +
geom_text(aes(cutoff+80,400,label = "20%"), cex=5, col=hsv(0.6,0.8,0.8, alpha=0.8)) +
geom_text(aes(cutoff-80,400,label = "80%"), cex=5, col=hsv(0.3,0.8,0.8, alpha=0.8))


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# truncate

space$tk = space$tk[, 1:cutoff]
space$dk = space$dk[, 1:cutoff]
space$sk = space$sk[ 1:cutoff ]



class(dtm) = "dgCMatrix"
dtm = as.matrix(dtm)

dtm = Matrix(dtm)

source("~/Documents/werkstatt/lsa-package/lsa/pkg/lsa/R/svdlibc.R")

a = proc.time()
space = lsa_sparse(dtm, ndim=min(ncol(dtm), nrow(dtm)))
proc.time()-a


a = proc.time()
space = lsa_sparse(dtm, ndim=cutoff)
proc.time()-a



#-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# add to DomainManager

dmgr = DomainManager()
#dmgr$tempdir = "~/Documents/werkstatt/mpia-package/cache/"
dmgr$tempdir = "~/mpia/cache/"

d = Domain(name="essays")
d$setSpace(space)
dmgr$add(d)

# calculate visual data

d$proximityThreshold = 0.3
d$identityThreshold = 1 # was 0.7
d$calculateTermProximities()

d$visualiser$calculateNetCoords()
d$visualiser$calculateReliefContour()
dmgr$materialise(d$signature)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# plot

dmgr = DomainManager()
dmgr$tempdir = "~/Documents/werkstatt/mpia-package/cache/"
d = dmgr$get(name="essays")

pdf(width=(21-8.8)/2.54, height=(29.7-10.4-1)/2.54, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-essayscoring/map-final-empty.pdf")
plot(d, method="topographic")
dev.off()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# learner performance records in mpia space

scorefiles = dir("~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/essays.scores/", full.names=TRUE)
humanscores=NULL
for (i in 1:length(scorefiles)) {
	humanscores = rbind(humanscores, read.table(scorefiles[i], col.names=c("file","score"),row.names="file"))
}

set.seed(22031977)
firstnames = c(
	readLines(dir("~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/names/", full.names=TRUE)[1], warn=F),
	readLines(dir("~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/names/", full.names=TRUE)[2], warn=F)
)
student.names = firstnames[sample(length(firstnames),nrow(essays.content))]


pdf(width=(21-8.8)/2.54, height=(29.7-10.4-1)/2.54, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-essayscoring/map-final-students.pdf")

ppl = HumanResourceManager(domainmanager=dmgr, domain=d)
d$traces = matrix(ncol=0, nrow=0)

gc()
plot(d, method="topographic")

cs = c("firebrick", "chartreuse4", "darkorchid4", "black", "deeppink3", "orange4", "cadetblue4", "goldenrod2", "deepskyblue2", "antiquewhite3")

oldds = ""
essays = NULL
essays.scores = NULL

for (i in 1:nrow(essays.content)) {
    
    ds = strsplit(substr(essays.content[i,1], 5, nchar(essays.content[i,1])), "[_\\.]")[[1]][1]
    if (ds != oldds) { n = 1;oldds=ds} else n = n+1
    
    essays.scores[i] = humanscores[essays.content[i,1],1]
    
    student.names[i] = paste( student.names[i], " (", ds,")", sep="" )
    p = ppl$add( name=student.names[i] )
    
    essays[i] = gsub("[^a-zA-Z0-9äöüÄÖÜß]", " ", essays.content[i,2])
    p$perform( essays[i], activity="exam", purpose="exam", score=essays.scores[i])
    #plot(p, col=cs[as.integer(ds)], label=FALSE, component.labels=FALSE, component.arrows=F, dot.cex=1)
    print(i)
    
}

dev.off()


# analysis

lorin = ppl$people[[474]]
lorin$name
performances(lorin)

lorin[1]$getSourceText()
terms(lorin[1])

# let's look at the underlying space: how many weakly (!) activated terms?

d$proximityThreshold = 0
activated = lorin[1]$getActivatedTerms()
length(which(activated$values<0.3))
table(round(activated$values,1))
d$proximityThreshold = 0.3

# compare

lorin$scores

brandise = ppl$people[[440]]
brandise$scores
overlap(lorin[1], brandise[1])

linnell = ppl$people[[449]]
linnell$scores
overlap(lorin[1], linnell[1])

dall = ppl$people[[473]]
dall$scores
overlap(lorin[1], dall[1])


# plot all persons

pdf(width=(21-8.8)/2.54, height=(29.7-10.4-1)/2.54, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-essayscoring/map-final-students2.pdf")

plot(d, method="topographic")
collection = as.integer( gsub("_", "", substr(essays.content[,1], 5, 6)) )
cs = c("firebrick", "chartreuse4", "darkorchid4", "black", "deeppink3", "orange4", "cadetblue4", "goldenrod2", "deepskyblue2", "black")
for (p in 1:length(ppl$people)) plot(ppl$people[[p]], col=cs[collection][p], label=FALSE, component.labels=FALSE, component.arrows=F, dot.cex=1)

dev.off()

# legend

rightorder = unique( as.integer( gsub("_", "", substr(essays.content[,1], 5, 6)) ) )
plot(c(1,3,1,1,2,2,2,3,3), c(1,3,2,3,1,2,3,1,2), pch=21, col="black", bg=cs[rightorder], cex=3, xlim=c(0,4), ylim=c(0,4), ylab="", xlab="", xaxt="n", yaxt="n", frame=FALSE)
text(c(1.4,3.4,1.4,1.4,2.4,2.4,2.4,3.4,3.4), c(1,3,2,3,1,2,3,1,2), labels=paste("collection ", rightorder, sep=""))


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# competence analysis

pdf(width=(21-8.8)/2.54, height=(29.7-10.4-1)/2.54, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-essayscoring/map-final-competence-coll1.pdf")

ppl = HumanResourceManager(domainmanager=dmgr, domain=d)
d$traces = matrix(ncol=0, nrow=0)
gc()

plot(d, method="topographic")

set.seed(22031977)
firstnames = c(
readLines(dir("~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/names/", full.names=TRUE)[1], warn=F),
readLines(dir("~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/names/", full.names=TRUE)[2], warn=F)
)
student.names = firstnames[sample(length(firstnames),nrow(essays.content))]

oldds = ""
essays = NULL
essays.scores = NULL
for (i in 1:102) {
    
    if (i!=41) {
    
        ds = strsplit(substr(essays.content[i,1], 5, nchar(essays.content[i,1])), "[_\\.]")[[1]][1]
        if (ds != oldds) { n = 1;oldds=ds} else n = n+1
        
        essays.scores[i] = humanscores[essays.content[i,1],1]
        
        student.names[i] = paste( student.names[i], " (", ds,")", sep="" )
        p = ppl$add( name=student.names[i] )
        
        essays[i] = gsub("[^a-zA-Z0-9äöüÄÖÜß]", " ", essays.content[i,2])
        p$perform( essays[i], activity="exam", purpose="exam", score=essays.scores[i])
        plot(p, col= cs[as.integer(ds)], label=FALSE, component.labels=FALSE, component.arrows=F, dot.cex=1) # gray(1-(essays.scores[i]/5)/8)
        print(i)
        
    } # empty ones removed
    
}

# competence analysis for this cluster

d$identityThreshold = 1
comps = competences(ppl)
terms(comps)


#coll1 = lapply(ppl$people[1:102], function(e) e[1])
#class(coll1) = c("Performance")
#comps = competences(coll1)
#terms(comps)

plot(comps, col="green", dot.cex=3, label=F, component.arrows=F, component.labels=F, connect=F)

dev.off()


# group analysis for collection 1
# not used!!

d$identityThreshold = 0.7
groups(ppl)

for (g in 1:length(ppl$groups)){

   gmemb = unlist(strsplit(ppl$groups[[g]]$name, ", "))
   gstuds = which(names(ppl) %in% gmemb)
   print(paste("group", g, ":", sep=" "))
   print( paste( unlist( lapply(ppl$people[gstuds], function(e) e$scores) ), collapse=","))
   #print( paste( lapply(ppl$people[gstuds], function(e) paste(terms(e[1]), collapse=",") )))
   print( terms(position(ppl$groups[[g]])) )

}

# overlap between score groups

scores =  unlist( lapply(ppl$people, function(e) e$scores) )
scterms = matrix(ncol=2,nrow=length(unique(scores)))
colnames(scterms) = c("scoregroup", "terms")
rownames(scterms) = rep("", nrow(scterms))
for (i in unique(scores)) {
    
    gstuds = which(i == scores)
    print(paste("score group", i, ":", sep=" "))
    scterms[i/5+1,1] = i
    pfs = unlist( lapply(ppl$people[gstuds], performances) )
    class(pfs) = "Performance"
    pfs2 = position(pfs)
    print( paste( terms(pfs2), collapse=", " ))
    scterms[i/5+1,2] =paste( terms(pfs2), collapse=", " )
    
}
scterms


# not used!!
# competence analysis for all

ppl = HumanResourceManager(domainmanager=dmgr, domain=d)
d$traces = matrix(ncol=0, nrow=0)
gc()

plot(d, method="topographic")

activated = terms(performances(ppl))
toIgnore = which( unlist(lapply(activated, function (e) {all(is.na(e))})) )

oldds = ""
essays = NULL
essays.scores = NULL
for (i in 1:nrow(essays.content)) {
    
    if (! i %in% toIgnore) {
        
        ds = strsplit(substr(essays.content[i,1], 5, nchar(essays.content[i,1])), "[_\\.]")[[1]][1]
        if (ds != oldds) { n = 1;oldds=ds} else n = n+1
        
        essays.scores[i] = humanscores[essays.content[i,1],1]
        
        student.names[i] = paste( student.names[i], " (", ds,")", sep="" )
        p = ppl$add( name=student.names[i] )
        
        essays[i] = gsub("[^a-zA-Z0-9äöüÄÖÜß]", " ", essays.content[i,2])
        p$perform( essays[i], activity="exam", purpose="exam", score=essays.scores[i])
        plot(p, col= cs[as.integer(ds)], label=FALSE, component.labels=FALSE, component.arrows=F, dot.cex=1) # gray(1-(essays.scores[i]/5)/8)
        print(i)
        
    } # empty ones removed
    
}

d$identityThreshold = 0.
comps = competences(ppl)
terms(comps)

plot(comps, col="green", dot.cex=3, label=F, component.arrows=F, component.labels=F, connect=F)


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# scoring all of them in the full essay space (not used!!)

dmgr = DomainManager()
dmgr$tempdir = "~/Documents/werkstatt/mpia-package/cache/"
d = dmgr$get(name="essays")
space = d$space

setwd("~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/")

essays.content = read.csv2(file="essays.content.csv", stringsAsFactors=FALSE)


# golds

seeddir = "essays.base/seed/"

fcorpus = c( dir(seeddir, recursive=TRUE, full.names=TRUE) )
length(fcorpus)

fnessays = basename( c( dir(seeddir, recursive=TRUE)) )
length(fnessays)

gold = basename( dir(seeddir, recursive=TRUE) )
fenc = NULL
content = NULL
umlauts = NULL
output = NULL
for (f in 1:length(fcorpus)) {
    
    fn = fcorpus[f]
    readStuff = NULL
    
    tryCatch( {
        fc = file(fn, encoding="iso-8859-1")
        readStuff = paste( readLines(fc, warn=FALSE), collapse=" ")
        close(fc)
    }, error=function(e) output = c(output, paste(f, ". error readLines ISO:",e,"\n", sep="")),
    warning=function(e) output = c(output, paste(f, ". warning readLines ISO:",e,"\n", sep=""))
    )
    
    umlauts[f] = grepl("[äöüÄÖÜß]", readStuff)
    
    if (!umlauts[f]) {
        tryCatch( {
            fc = file(fn, encoding="UTF8")
            readStuff = paste( readLines(fc, warn=FALSE), collapse=" ")
            close(fc)
        }, error=function(e) output = c(output, paste(f, ". error readLines UTF8:",e,"\n", sep="")),
        warning=function(e) output = c(output, paste(f, ". warning readLines UTF8:",e,"\n", sep="")))
        umlauts[f] = grepl("[äöüÄÖÜß]", readStuff)
    }
    
    if (!umlauts[f]) output = c(output,paste(f, ". no umlauts after utf8\n", sep=""))
    
    fenc[f] = Encoding(readStuff)
    content[f] = readStuff
}
lost=which(!umlauts)
thegolds = cbind(fnessays, content)

ppl = HumanResourceManager(domainmanager=dmgr, domain=d)
goldpers = ppl$add(name="goldstandards")

for (i in 1:nrow(thegolds)) {
    goldpers$perform( thegolds[i,2], activity="exam", purpose="exam")
}

mv = goldpers$getMeaningVectors()
goldsdtm = crossprod(t(crossprod(t(space$tk), diag(space$sk))), t(mv))
colnames(goldsdtm) = thegolds[,1]

# mix with essays

fulldtm = cbind(as.textmatrix(d$space), goldsdtm)
e2e = lsa::cosine(fulldtm)

rownames(e2e)[1:nrow(d$space$dk)] = essays.content[-empty,1]
colnames(e2e)[1:nrow(d$space$dk)] = essays.content[-empty,1]

e2e[1:10,1:10]
nre = ncol(as.textmatrix(d$space))

rhos = rep(NULL,10)
for (nr in c(1:3,5:10)) {
    
    # calc by mean cosine
    
    thekey = paste( "data",nr, "_", sep="")
    theix = which( substr(rownames(e2e),1,nchar(thekey)) == thekey)
    goldnameskey = paste( "data",nr, "_golden", sep="")
    goldsix = which( substr(rownames(e2e),1,nchar(goldnameskey)) == goldnameskey)
    
    meancos = e2e[goldsix,theix]
    goldsix2 = which( substr(colnames(meancos),1,nchar(goldnameskey)) == goldnameskey)
    meancos = colSums( meancos[,-goldsix2] ) /3

    humanscores = read.csv(paste("essays.scores/corpus.", nr, ".grades", sep=""), sep=" ", header=FALSE, row.names="V1")
    machinescores = meancos
    #print( cor.test( humanscores[names(machinescores),1], machinescores, exact=FALSE, method="spearman", alternative="two.sided") )

    # calc by overlap
    
    goldmvs = mv[goldsix-nre,]
    goldposition = colSums( goldmvs ) / 3
    
    goldtermvec = crossprod(t(crossprod(t(space$tk), diag(space$sk))), (goldposition))
    goldtermixs = which(goldtermvec>d$proximityThreshold)
    goldterms = rownames(space$tk[goldtermixs,])
    
    theevecs = space$dk[theix[which(theix<nre)],]
    rownames(theevecs) = essays.content[-empty,1][theix[which(theix<nre)]]
    theetermvecs = crossprod(t(crossprod(t(space$tk), diag(space$sk))), t(theevecs))

    goldoverlap = NULL
    for (z in 1:nrow(theevecs)) {
        essaytermsvec = theetermvecs[,z]
        essaytermixs = which(essaytermsvec>d$proximityThreshold)
        essayterms = rownames(space$tk)[essaytermixs]
        allterms = c(goldterms, essayterms)
        goldoverlap[z] = length( which( table(allterms) == 2 ) )
    }
    machinescores = goldoverlap
    names(machinescores) = rownames(theevecs)
    print( cor.test( humanscores[names(machinescores),1], machinescores, exact=FALSE, method="spearman", alternative="two.sided") )
    
    rhos[nr] = cor( humanscores[names(machinescores),1], machinescores, method="spearman")

}


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# scoring by overlap (example as in diss on 10.3.1)

tutor = ppl$add( name="tutor" )
tutor$perform( solutions[1], activity="gold 1" )
tutor$perform( solutions[2], activity="gold 2" )
tutor$perform( solutions[3], activity="gold 3" )

p = ppl$add( name="student" )
p$perform( essays[1], activity="essay (1)" )

length( overlap( p[1], position(goldpers) ) )





# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# visual accuracy


dmgr = DomainManager()
dmgr$tempdir = "~/Documents/werkstatt/mpia-package/cache/"
d = dmgr$get(name="essays")

#d$proximityThreshold = 0.3
#d$identityThreshold = 1 # was 0.7
#d$calculateTermProximities()

#d$visualiser$calculateNetCoords()
#d$visualiser$calculateReliefContour()

lsavecs = d$space$tk %*% diag(d$space$sk)
t2t = cosine(t(lsavecs))

# t2t - termProximites
sum(t2t - d$termProximities)/(nrow(t2t)*ncol(t2t)-length(diag(t2t)))
# 0.01549215

map = d$visualiser$netcoords

distances = matrix(ncol=nrow(map), nrow=nrow(map))
for (i in 1:nrow(map)) {
    for (n in 1:nrow(map)) {
        distances[i,n] = sqrt( (map[i,1]-map[n,1])^2 + (map[i,2]-map[n,2])^2)
    }
}

t2tnorm = (t2t-min(t2t)) / (max(t2t) - min(t2t))

(sum((1-t2tnorm) - distances)) / (nrow(t2t)*ncol(t2t)-length(diag(t2t)))

