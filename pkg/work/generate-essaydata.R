
library(tm)
library(lsa)
library(mpia)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# base corpus

corpus = "~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/essays.base"
#corpus = "~/essays.base/"

fcorpus = dir(corpus, recursive=TRUE, full.names=TRUE)
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


tm = Corpus(
#DirSource(corpus, recursive=TRUE, encoding="UTF8"), # "ISO-8859-1"
   VectorSource(content),
   readerControl=list(
      reader=readPlain, language="de",
      load=TRUE, removePunctuation=TRUE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE
   )
)

tm = tm_map(tm, function(e) return(gsub("[^a-zA-Z0-9äöüÄÖÜß]", " ", e)))

tm = tm_map(tm, tolower)
tmorig = tm

dict = Terms(DocumentTermMatrix(
   tmorig,
   control=list(removePunctuation=TRUE, stopwords=FALSE, minWordLength=1, removeNumbers=TRUE)
))

dict[1:100]

#dict = Dictionary(TermDocumentMatrix(tmorig, control=list(removePunctuation=TRUE, stopwords=FALSE, minWordLength=1, removeNumbers=TRUE)))

tm = tm_map(tm, stemDocument, language="ger")

dtm = TermDocumentMatrix(tm, control = list(
removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE,
minWordLength = 3, bounds = list(global=c(1,Inf))
))

dtms = dtm

dim(dtms)

sc = as.character( stemCompletion(rownames(dtms), dictionary=dict, type="shortest") )
sc[which(is.na(sc))] = rownames(dtms)[which(is.na(sc))]

dtmsold = dtms

rownames(dtms) = sc
if (any(duplicated(rownames(dtms)))) {
   
   dupes = which(duplicated(rownames(dtms)))
   for (i in dupes) {
      
      cat(paste("removing dupe for ", sc[i], "\n", sep=""))
		hits = which(sc == sc[i])
      target = hits[ which(! hits %in% which(duplicated(sc))) ]
      replvec = t(as.matrix( colSums(as.matrix(dtms[ hits, ])) ))
      rownames(replvec) = sc[target]
      dtms[ target,1:length(replvec) ] = replvec
      
   }
   dtms = dtms[!duplicated(rownames(dtms)),]
}
class(dtms) = c("TermDocumentMatrix", class(dtms))

dim(dtms)
dim(dtmsold)

if (any(rownames(dtms) == "")) {
   cat("removing empty ones")
   dtms = dtms[-(which(rownames(dtms) == "")), ]
}

dim(dtms)

save(dtms, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-essayscoring/dtms-essaybase-full.RData")


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# check for outliers: minimum global frequency of 2,3,6

freqs = rowSums(as.matrix(dtms))

# plot freq dist with chosen upper/lower threshold window

plot(sort(freqs, dec=T), type="l", log="xy", ylab="frequency (log-scaled)", xlab="terms (log-scaled)")

greater10 = length(which(freqs > ncol(dtms)/10))
greater25 = length(which(freqs > ncol(dtms)/4))
greater50 = length(which(freqs > ncol(dtms)/2))
justonce = length(which(freqs>1))
justtwice = length(which(freqs>2))
just5 = length(which(freqs>5))
just10 = length(which(freqs>10))

rect(greater25,ncol(dtms)/4, justtwice, 2, lty="dotted", col=rgb(0.3,0.3,0.3, alpha=0.1))

text(greater10+30, 50+ncol(dtms)/10, paste(greater10, "> 10%", sep=" "), col="darkgreen", cex=0.7, font=2)
abline(h=ncol(dtms)/10, lty="dashed", col="darkgreen")

text(greater25+13, 150+ncol(dtms)/4, paste(greater25, "> 25%", sep=" "), col="darkgreen", cex=0.7)
abline(h=ncol(dtms)/4, lty="dashed", col="darkgreen")

text(greater50, 300+ncol(dtms)/4, paste(greater50, "> 50%", sep=" "), col="darkgreen", cex=0.7)
abline(h=ncol(dtms)/2, lty="dashed", col="darkgreen")

abline(v=justonce, lty="dotted", col="darkred")
text(justonce-7000, 1, paste(justonce,">1",sep=" "), col="darkred", cex=0.7)

abline(v=justtwice, lty="dotted", col="darkred")
text(justtwice-5000, 2, paste(justtwice,">2",sep=" "), col="darkred", cex=0.7)
#text(justtwice+6500, 2.5, paste(length(freqs)-justtwice,"<2",sep=""), col="darkred", cex=0.7)

abline(v=just5, lty="dotted", col="darkred")
text(just5-2200, 4, paste(just5,">5",sep=" "), col="darkred", cex=0.7)
#text(just5+4000, 6, paste(length(freqs)-just5,"<5",sep=""), col="darkred", cex=0.7)

abline(v=just10, lty="dotted", col="darkred")
text(just10-1500, 8, paste(just10,">10",sep=" "), col="darkred", cex=0.7, font=2)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# filtering by threshold

lower = which(freqs>1)
dtmsred = dtms[lower,]

freqs2 = rowSums(as.matrix(dtmsred))
upper = which(freqs2 < ncol(dtms)/4)
length( upper )

dtmsred = dtmsred[upper,]

save(dtmsred, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-essayscoring/dtms-essaybase-full-globfreq1up25pctdown.RData")

# t2t size:
nrow(dtmsred)^2 * 8 / 1024 / 1024 / 1024
# = [1] 2.64427 GB

dtm = as.matrix(dtmsred)
save(dtm, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-essayscoring/dtm-trimmed.RData")


# slow
#tr = sum(diag(dtm %*% t(dtm)))
#tr = Trace(dtm %*% t(dtm))

# superfast
tr = sum(dtm*dtm)
tr
# 360437

tr80 = tr * 0.8
# 288349.6

class(dtm) = "textmatrix"

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# lsa

space = lsa(dtm, dims=dimcalc_raw())

save(space, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-essayscoring/space-raw.RData")
# save(space, file="~/mpia/examples-essayscoring/space-raw.RData")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# find ideal truncation (recalc tr or use from above)

tr = sum(space$sk^2)

# threshold
threshold = tr * 0.8

# find cutoff
r = 0
for (i in 1:length(space$sk)) {
   r = r + (space$sk[i]^2)
   if (r >= threshold) {
      cutoff = i-1
      break()
   }
}

cutoff
# 729 for 0.8


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
# plot eigenvalue distribution

#par(mfrow=c(2,1))
#plot(space$sk*space$sk, type="l", ylab="singular values", ylim=c(0,max(space$sk)+1), xlab="index")
#grid(10,10)

# with 80/20 area under curve
qplot(1:length(space$sk),  space$sk*space$sk, geom="path", ylab="eigenvalues", xlab="index") + geom_vline(xintercept=cutoff, col="darkgray", linetype=2) + geom_area(aes(1:cutoff,  (space$sk*space$sk)[1:cutoff]), bg=hsv(0.3,0.8,0.8, alpha=0.5)) + geom_area(aes(cutoff:length(space$sk),  (space$sk*space$sk)[cutoff:length(space$sk)]), bg=hsv(0.6,0.8,0.8, alpha=0.5)) +
geom_text(aes(cutoff+80,400,label = "20%"), cex=5, col=hsv(0.6,0.8,0.8, alpha=0.8)) +
geom_text(aes(cutoff-80,400,label = "80%"), cex=5, col=hsv(0.3,0.8,0.8, alpha=0.8))

# with sk and sk^2
#qplot(1:length(space$sk),  space$sk*space$sk, geom="path", ylab="eigenvalues = s^2", xlab="index") + geom_path(aes(1:length(space$sk), space$sk), col="red", linetype = 2)


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# truncate

space$tk = space$tk[, 1:cutoff]
space$dk = space$dk[, 1:cutoff]
space$sk = space$sk[ 1:cutoff ]


#-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# materialise

# add to DomainManager

dmgr = DomainManager()
#dmgr$tempdir = "~/Documents/werkstatt/mpia-package/cache/"
dmgr$tempdir = "~/mpia/cache/"

d = Domain(name="essayscoringfull")
d$setSpace(space)
dmgr$add(d)
dmgr$materialise(d$signature)

# calculate visual data

d$proximityThreshold = 0
d$identityThreshold = 0.7
d$calculateTermProximities()
dmgr$materialise(d$signature)

# trim termProximities

#d = dmgr$get(name="essayscoringfull")
d$proximityThreshold = 0.3
d$identityThreshold = 0.7
d$termProximities[which(d$termProximities<d$proximityThreshold)] = 0

d$visualiser$calculateNetCoords()
d$visualiser$calculateReliefContour()
plot(d, method="topographic")

dmgr$materialise(d$signature) # warning: will overwrite termProximities!!



# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# essays

scorefiles = dir("~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/essays.scores/", full.names=TRUE)
humanscores=NULL
for (i in 1:length(scorefiles)) {
	humanscores = rbind(humanscores, read.table(scorefiles[i], col.names=c("file","score"),row.names="file"))
}

#dmgr = DomainManager()
#dmgr$tempdir = "~/Documents/werkstatt/mpia-package/cache/"
#d = dmgr$get("essayscoring")

ppl = HumanResourceManager(domainmanager=dmgr, domain=d)

fnessays = basename( dir("~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/essays", recursive=TRUE) )
fessays = dir("~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/essays", recursive=TRUE, full.names=TRUE)

set.seed(22031977)
firstnames = c(
	readLines(dir("~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/names/", full.names=TRUE)[1], warn=F),
	readLines(dir("~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/names/", full.names=TRUE)[2], warn=F)
)
essays.students = firstnames[sample(length(firstnames),length(fnessays))]

oldds = ""
essays = NULL
essays.scores = NULL
for (i in 1:length(fnessays)) {
  
   ds = strsplit(substr(fnessays[i], 5, nchar(fnessays[i])), "[_\\.]")[[1]][1]
   if (ds != oldds) { n = 1;oldds=ds} else n = n+1
   
   essays.scores[i] = humanscores[fnessays[i],1]
   
   essays.students[i] = paste( essays.students[i], " (", ds,")", sep="" )
   p = ppl$add( name=essays.students[i] )
   
   essays[i] = paste( gsub("[^a-zA-Z0-9äöüÄÖÜß]", " ", readLines(fessays[i], warn=F)), collapse=" ")
   p$perform( essays[i], activity="exam", purpose="exam", score=essays.scores[i])
   
}

save(essays.scores, file="~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/data/essays.scores.rda", compress="bzip2") # ascii=TRUE
save(essays.students, file="~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/data/essays.students.rda", compress="bzip2") # ascii=TRUE
save(essays, file="~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/data/essays.rda", compress="bzip2") # ascii=TRUE


#

#hs = humanscores[ sort(humanscores[,1], index.return=TRUE)$ix, ]
#names(hs) = rownames(humanscores)[ sort(humanscores[,1], index.return=TRUE)$ix ]


