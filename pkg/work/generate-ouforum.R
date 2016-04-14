
library(tm)
library(lsa)
library(mpia)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# base corpus

corpus = "~/Documents/werkstatt/mpia-package/ou-forum/corpus"
setwd(corpus)

h808 = read.csv("h808.forum-posts.csv", sep=";")

# extract content
forum = h808[,c("userhash", "userroles", "subject", "replace", "format")]
students = which(forum$userroles =="{Student}")
content = do.call( paste, c(forum[students,c("subject", "replace")], sep=" "))

# html cleaning
content = gsub("<[^>]*>", " ", content, perl = TRUE)
content = gsub("&#([[:digit:]]*);", " ", content)
content = gsub("&([[:alpha:]]*);", " ", content)

tm = Corpus(
    VectorSource(content),
    readerControl=list(
        reader=readPlain, language="en",
        load=TRUE,
        removePunctuation=TRUE,
        stopwords=TRUE,
        minWordLength=1,
        removeNumbers=TRUE
    )
)

# sanitise: strip spaces, lower case, remove stopwords, remove numbers

tm = tm_map(tm, stripWhitespace)
tm = tm_map(tm, tolower)
tm = tm_map(tm, removeWords, stopwords("english"))
tmorig = tm

# stemming

dict = Terms(DocumentTermMatrix(
    tmorig, control = list(removePunctuation=TRUE, minWordLength=2, removeNumbers=TRUE, bounds=list(global=c(1,Inf)))
))

dict[1:100]

tm = tm_map(tm, stemDocument, language="en")
dtm = TermDocumentMatrix(
       tm, control = list(removePunctuation=TRUE, minWordLength=2, removeNumbers=TRUE, bounds=list(global=c(1,Inf))
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

save(dtms, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-ouforum/dtms-h808-full.RData")

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

rect(greater10,ncol(dtms)/10, just10, 10, lty="dotted", col=rgb(0.3,0.3,0.3, alpha=0.1))

text(greater10+30, 50+ncol(dtms)/10, paste(greater10, "> 10%", sep=" "), col="darkgreen", cex=0.7, font=2)
abline(h=ncol(dtms)/10, lty="dashed", col="darkgreen")

text(greater25-3, 150+ncol(dtms)/4, paste(greater25, "> 25%", sep=" "), col="darkgreen", cex=0.7)
abline(h=ncol(dtms)/4, lty="dashed", col="darkgreen")

text(greater50, 300+ncol(dtms)/4, paste(greater50, "> 50%", sep=" "), col="darkgreen", cex=0.7)
abline(h=ncol(dtms)/2, lty="dashed", col="darkgreen")

abline(v=justonce, lty="dotted", col="darkred")
text(justonce-4000, 1, paste(justonce,">1",sep=" "), col="darkred", cex=0.7)

abline(v=justtwice, lty="dotted", col="darkred")
text(justtwice-3000, 2, paste(justtwice,">2",sep=" "), col="darkred", cex=0.7)
#text(justtwice+6500, 2.5, paste(length(freqs)-justtwice,"<2",sep=""), col="darkred", cex=0.7)

abline(v=just5, lty="dotted", col="darkred")
text(just5-2200, 4, paste(just5,">5",sep=" "), col="darkred", cex=0.7)
#text(just5+4000, 6, paste(length(freqs)-just5,"<5",sep=""), col="darkred", cex=0.7)

abline(v=just10, lty="dotted", col="darkred")
text(just10-1500, 8, paste(just10,">10",sep=" "), col="darkred", cex=0.7, font=2)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# filtering by threshold

lower = which(freqs>10)
dtmsred = dtms[lower,]

freqs2 = rowSums(as.matrix(dtmsred))
upper = which(freqs2 < ncol(dtms)/10)
length( upper )

dtmsred = dtmsred[upper,]

#save(dtmsred, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-ouforum/dtms-h808-globfreq2up25pctdown.RData")

# t2t size
nrow(dtmsred)^2 * 8 / 1024 / 1024 / 1024

dtm = as.matrix(dtmsred)

# this may have resulted in empty columns
any(colSums(dtm) == 0)
dropped = which(colSums(dtm)==0)
if ( any(colSums(dtm) == 0) ) dtm= dtm[,-which(colSums(dtm)==0)]

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# document pruning

freqs = colSums(dtm)

# plot document length (in terms) with chosen upper/lower threshold window
plot(sort(freqs, dec=T), type="l", log="xy", ylab="length in terms (log-scaled)", xlab="documents (log-scaled)")

greater200 = length(which(freqs > 200))
just50 = length(which(freqs>50))
rect(greater200,200, just50, 50, lty="dotted", col=rgb(0.3,0.3,0.3, alpha=0.1))

text(greater200-60, 150, paste(greater200, "> 200", sep=" "), col="darkgreen", cex=0.7)
abline(h=200, lty="dashed", col="darkgreen")

abline(v=just50, lty="dotted", col="darkred")
text(just50-1000, 30, paste(just50,">50",sep=" "), col="darkred", cex=0.7, font=2)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# filtering by threshold

lower = which(freqs>50)
dtmsred = dtm[,lower]

freqs2 = colSums(as.matrix(dtmsred))
upper = which(freqs2 < 200)
length( upper )

dtmsred = dtmsred[,upper]

dtm = dtmsred

any(rowSums(dtm) == 0)
if ( any(rowSums(dtm) == 0) ) dtm= dtm[-which(rowSums(dtm)==0),]

save(dtm, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-ouforum/dtm-trimmed4.RData")


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# determine number of eigendimensions

tr = sum(dtm*dtm)
tr
# 360437

tr80 = tr * 0.8
# 288349.6

class(dtm) = "textmatrix"

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# lsa

space = lsa(dtm, dims=500)

#save(space, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-ouforum/spacesmall-500.RData")
save(space, file="~/mpia/examples-ouforum/spacesmall-500.RData")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# find ideal truncation

# tr80 should smaller than this:
sum(space$sk^2)
tr*0.8
tr*0.8<sum(space$sk^2)

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
# 452 for 0.8


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

d = Domain(name="h808small")
d$setSpace(space)
dmgr$add(d)
dmgr$materialise(d$signature)

d = dmgr$get("h808")

# calculate visual data

d$proximityThreshold = 0.7
d$identityThreshold = 1
d$calculateTermProximities()

dmgr$materialise(d$signature)
gc()

d$visualiser$calculateNetCoords()
d$visualiser$calculateReliefContour()
plot(d, method="topographic")

dmgr$materialise(d$signature)
gc()

# persistance
dmgr$materialise(d$signature)


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# add student data

ppl = HumanResourceManager(dmgr, d)
plot(d, method="topographic")
toponymy(d, method="mountains")

# flush traces
d$traces = matrix(ncol=0, nrow=0)

studenthash = h808[students,]$userhash
table(studenthash)

# student 1
sel = "713201398b938aea26c892c1a0058079"
selCont = which(studenthash==sel)
p1 = ppl$add( name="student1" )

for (i in c(1:length(selCont))) {
    text = content[selCont[i]]
    nrterms = sum( query(text, rownames(d$space$tk)))
    if (nrterms>50 && nrterms<200) p1$perform( text, activity="discussion", purpose="h808")
}

plot(p1, col="red", label=TRUE)

# student 2

sel = "c14f67060ba26ab1cd6b0a0d30b81065"
selCont = which(studenthash==sel)
p2 = ppl$add( name="student2" )

for (i in 1:length(selCont)) {
    text = content[selCont[i]]
    p2$perform( text, activity="discussion", purpose="h808")
}

plot(position(p2), col="green", label=TRUE)

# student 3

sel = "fe66bf25a29c457badd96d02ce9de762"
selCont = which(studenthash==sel)
p3 = ppl$add( name="student3 )

for (i in 1:length(selCont)) {
    text = content[selCont[i]]
    p3$perform( text, activity="discussion", purpose="h808")
}

plot(position(p3), col="blue", label=TRUE)

# student 4

sel = "8ec89307d74a61d92b5e54b35ec8b56d"
selCont = which(studenthash==sel)
p4 = ppl$add( name="student4" )

for (i in 1:length(selCont)) {
    text = content[selCont[i]]
    p4$perform( text, activity="discussion", purpose="h808")
}

plot(position(p4), col="yellow", label=TRUE)


# student 5

sel = "516f26a12f3e96a24bbb6f13463b12de"
selCont = which(studenthash==sel)
p5 = ppl$add( name="student5" )

for (i in 1:length(selCont)) {
    text = content[selCont[i]]
    p5$perform( text, activity="discussion", purpose="h808")
}

plot(position(p5), col="purple", label=TRUE)


