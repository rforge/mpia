
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# full matrix from pubmed

# performance benchmark (full)

load("~/Documents/denkfabrik/dissertation/workodrome-II/examples-medical-old/tdm_full.RData")
tdm
dtm = attr(tdm, "Data")

class(dtm) = "dgCMatrix"
dtm = as.matrix(dtm)

tr = sum(dtm * dtm)
# 15036729

dtm = Matrix(dtm)

source("~/Documents/werkstatt/lsa-package/lsa/pkg/lsa/R/svdlibc.R")

gc()

a = proc.time()
space = lsa_sparse(dtm, ndim=min(ncol(dtm), nrow(dtm)))
proc.time()-a

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
# ? for 0.8

a = proc.time()
space = lsa_sparse(dtm, ndim=cutoff)
proc.time()-a





# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -


# filtering with voc list from cardsort

cardsorts = read.csv("~/examples-medical/cardsort.csv")
voc = as.character(cardsorts$name)

tix = which(rownames(dtm) %in% voc)
thedocs = which( colSums(dtm[tix,]) ==  4 )
thedocs = which( colSums(dtm[tix,]) >0 )

dtm = dtm[,thedocs]
dtm = dtm[-which(rowSums(dtm)==0),]


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# pre-filtered matrix from pubmed (different: was calculated on augur?!!)

load("~/examples-medical/dtm_full.Rdata")
tr = sum(dtm * dtm)
# 70688712
dim(dtm)


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -



# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# medical student essays

a = proc.time()

setwd("~/Documents/werkstatt/mpia-package/medical/")
#setwd("~/mpia/examples-medical/")

medical = read.csv("csv/safe-prescribing-all.csv", header=TRUE)
colnames(medical) = c("meta", "text", "a","b","c","d","e", "f","ug","grade")

medical$grade = tolower(as.character(medical$grade))
medical$grade[which(medical$grade=="excellenet")] = "excellent"
medical$grade[which(medical$grade=="far")] = "fair"
medical$grade[which(medical$grade=="")] = "unrated"
table(medical$grade)

medical$ug[which(medical$grade=="unrated")] = "x"
medical$ug = tolower(medical$ug)=="x"

medical[,letters[1:6]] = (tolower(as.matrix(medical[,letters[1:6]]))!="")*1

content = medical$text[(!medical$ug)]

# now matricise!

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

tm = tm_map(tm, function(e) return(gsub("[^[:alnum:]]", " ", e)))
tm = tm_map(tm, stripWhitespace)
tm = tm_map(tm, tolower)

tm = tm_map(tm, removeWords, stopwords("english"))
tm = tm_map(tm, stemDocument, language="en")

dtm = TermDocumentMatrix(
    tm, control = list(removePunctuation=TRUE, minWordLength=2, removeNumbers=TRUE, bounds=list(global=c(1,Inf))
))

dtm = as.matrix(dtm)

cardsorts = read.csv("cardsort.csv")
voc = as.character(cardsorts$name)

voc %in% rownames(dtm)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# plot freq dist with chosen upper/lower threshold window

freqs = rowSums(dtm)

# commented for performance benchmark: just uncomment!!

#plot(sort(freqs, dec=T), type="l", log="xy", ylab="frequency (log-scaled)", xlab="terms (log-scaled)")

#greater10 = length(which(freqs > ncol(dtm)/10))
#greater25 = length(which(freqs > ncol(dtm)/4))
#greater50 = length(which(freqs > ncol(dtm)/2))
#justonce = length(which(freqs>1))
#justtwice = length(which(freqs>2))
#just5 = length(which(freqs>5))
#just10 = length(which(freqs>10))

#rect(greater10,ncol(dtm)/10, just10, 10, lty="dotted", col=rgb(0.3,0.3,0.3, alpha=0.1))

#text(greater10, ncol(dtm)/10, paste(greater10, "> 10%", sep=" "), col="darkgreen", cex=0.7, font=2)
#abline(h=ncol(dtm)/10, lty="dashed", col="darkgreen")

#text(greater25, ncol(dtm)/4, paste(greater25, "> 25%", sep=" "), col="darkgreen", cex=0.7)
#abline(h=ncol(dtm)/4, lty="dashed", col="darkgreen")

#text(greater50, ncol(dtm)/2, paste(greater50, "> 50%", sep=" "), col="darkgreen", cex=0.7)
#abline(h=ncol(dtm)/2, lty="dashed", col="darkgreen")

#abline(v=justonce, lty="dotted", col="darkred")
#text(justonce, 1, paste(justonce,">1",sep=" "), col="darkred", cex=0.7)

#abline(v=justtwice, lty="dotted", col="darkred")
#text(justtwice, 2, paste(justtwice,">2",sep=" "), col="darkred", cex=0.7)

#abline(v=just5, lty="dotted", col="darkred")
#text(just5, 4, paste(just5,">5",sep=" "), col="darkred", cex=0.7)

#abline(v=just100, lty="dotted", col="darkred")
#text(just10, 10, paste(just10,">100",sep=" "), col="darkred", cex=0.7, font=2)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# filtering terms by threshold

freqs = rowSums(dtm)

lower = which(freqs>1)
dtmsred = dtm[lower,]

#freqs2 = rowSums(dtmsred)
#upper = which(freqs2 < ncol(dtm)/10)
#length( upper )
#dtmsred = dtmsred[upper,]

dim(dtmsred)

sum((voc %in% rownames(dtmsred))*1)

dtm = dtmsred

proc.time() - a

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# filtering docs by threshold

#freqs = colSums(dtmsred)
#lower = which(freqs>50)
#dtmsred = dtmsred[,lower]

#freqs2 = colSums(dtmsred)
#upper = which(freqs2 < 60)
#length( upper )

#dtmsred = dtmsred[,upper]
#dim(dtmsred)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

#dtm = dtmsred

#save(dtm, file="dtm-medicalforum.RData")

# t2t size
nrow(dtmsred)^2 * 8 / 1024 / 1024 / 1024

#dtm = as.matrix(dtm)
class(dtm) = "textmatrix"

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# determine number of eigendimensions

tr = sum(dtm*dtm)
tr
# 110366

tr80 = tr * 0.8
# ?


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# lsa

a = proc.time()
space = lsa(dtm, dims=dimcalc_raw())
proc.time() - a

#save(space, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-ouforum/space-1000.RData")
#save(space, file="space-medicalforum-full.RData")

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
# 96 for 0.8


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


class(dtm) = "dgCMatrix"
dtm = as.matrix(dtm)

class(dtm) = "matrix"
dtm = Matrix(dtm)

source("~/Documents/werkstatt/lsa-package/lsa/pkg/lsa/R/svdlibc.R")

a = proc.time()
space = lsa_sparse(dtm, ndim=min(ncol(dtm), nrow(dtm)))
proc.time()-a


a = proc.time()
space = lsa_sparse(dtm, ndim=cutoff)
proc.time()-a




# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# cardsort

#source("~/examples-medical/lsaCosine.R")
source('~/Documents/werkstatt/lsa-package/lsa/pkg/lsa/R/lsaCosine.R')

cardsorts = read.csv("cardsort.csv")
maxima = unlist(lapply(apply(cardsorts[,4:(3+18)],2, unique), length))

voc = as.character(cardsorts$name)
dropped = which(!voc %in% rownames(space$tk))
dropped

voc = voc[ -dropped ] # drop those not in the space vocabulary

t2t = lsaCosine(space, side="left",filter=voc)

# average number of clusters
a =NULL
for (i in 3:21) a = c(a,length(table(cardsorts[,i])) )
mean(a)

#a = agnes( t2t, diss=FALSE )
#b = cutree(as.hclust(a), h=1.48)
a = hclust( dist((1+t2t)/2), method="complete")
b = cutree(as.hclust(a), h=0.85)
length(table(b))

membc = b
s = silhouette(as.integer(membc), dist(1+t2t/2))
plot(s, main="space", cex.names=0.7, cex.main=0.7)
avgwidths = sum(s[,"sil_width"]) / nrow(s)

for (i in 1:18) {
	
	memb = cardsorts[,3+i][-dropped]
	names(memb) = voc
	s = silhouette(memb, dist(1+t2t/2))
	plot(s, main=paste("PARTICIPANT",i), cex.names=0.7)
	avgwidths = c(avgwidths, sum(s[,"sil_width"]) / nrow(s))
    
}

i = 2 # best matching human

# Silhouette plots depict for each observation, how good the balance between its distances
# to its other cluster members compared to its distances within the next close cluster is.
# A value of 1 indicates that the observation clusters very well, whereas values around 0
# indicate that the observation lies in between two clusters. Observations
# tending towards -1 are probably in the wrong cluster.


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# cardsort: overlap between humans

hcards = cardsorts[,4:21]
rownames(hcards) = cardsorts[,2]
#hcards = hcards[-dropped,]

termpairs = matrix(0, ncol=nrow(hcards), nrow=nrow(hcards))
rownames(termpairs) = rownames(hcards)
colnames(termpairs) = rownames(hcards)
for (i in 1:ncol(hcards)) {
    clusters = unique(hcards[,i])
    for (n in clusters) {
        members = which(hcards[,i]==n)
        for (o in members) {
            for (l in members) {
                if (o!=l) termpairs[o,l] = termpairs[o,l] + 1
            }
        }
    }
}

# this image plot shows that there are not many termpairs that
# are put into the same clusters by many humans

image(termpairs, col=gray(1-0:18/18), xaxt="n", yaxt="n")

# this substantiates the visual analysis (outliers show .

boxplot(termpairs)

# calc maxima and mean/sd

library(matrixStats)
rmaxs = rowMaxs(termpairs)

mean(rmaxs)
# 10.15909
sd(rmaxs)
# 2.727493

layout(matrix(c(1,1,1,2,1,1,1,2), 2, 4, byrow = TRUE))
plot(sort(rmaxs), ylim=c(0,18), xlab="term (sorted by max paired)", ylab="max paired")
boxplot(rmaxs, ylim=c(0,18), yaxt="n", frame=FALSE)


# a bit of stats

length(which(termpairs>12))
length(termpairs)

nopairs = length(which(termpairs>0))

round( length(which(termpairs>12)) / nopairs, 2)
# 1%!

round(length(which(termpairs>6)) / nopairs,2)


# top 1%


top1a = NULL
top1b = NULL
top1f = NULL
top1c = NULL
counter = 0
for (i in 1:nrow(termpairs)) {
    for (n in 1:ncol(termpairs)) {
        if (termpairs[i,n] > 12) {
            counter = counter+1
            top1a = c(top1a,rownames(termpairs)[i])
            top1b = c(top1b,colnames(termpairs)[n])
            top1f = c(top1f, termpairs[i,n])
            top1c = c(top1c, t2t[top1a[counter],top1b[counter]])
        }
    }
}
round(sum(top1c)/length(top1c),1)


# now let's check for the most frequently matched term complements

t2tclean = t2t[-dropped,-dropped]

t2tclean = t2t # delete if there are terms that were dropped
diag(t2tclean) = 0
rmaxsmpia = rowMaxs(t2tclean)

# column index of highest matching term(s)

rcix = NULL
rix = NULL
ntimes = NULL
mpiaprox = NULL
rcixmpia = NULL
tnames = NULL
n = 1
for (i in 1:nrow(termpairs)) {

   print( paste( i, ": ", rownames(termpairs)[i], " matches most often (", rmaxs[i],") with ", colnames(termpairs)[ which(termpairs[i,]==rmaxs[i])], sep=""))
   dummy = which(termpairs[i,]==rmaxs[i])
   for (t in 1:length(dummy)) {
      rix[n] = i
      rcix[n] = dummy[t]
      tnames[n] = paste(rownames(termpairs)[i], "-", rownames(termpairs)[dummy[t]], sep=" ")
      ntimes[n] = rmaxs[i]
      rcixmpia[n] = which(t2tclean[i,]==rmaxsmpia[i])
      mpiaprox[n] = t2t[rix[n], rcix[n]]
      n = n+1
   }

}

thetable = cbind(rix,rcix,ntimes,rcixmpia,round(mpiaprox,1))
rownames(thetable) = tnames
six = sort(thetable[,"ntimes"], index.return=TRUE)$ix
thetable = thetable[six,]

thetable

# now get the mpia term pairs

membc = b

mpiapairs = matrix(0, ncol=nrow(hcards), nrow=nrow(hcards))
rownames(mpiapairs) = rownames(hcards)
colnames(mpiapairs) = rownames(hcards)
clusters = unique(membc)
for (n in clusters) {
    members = which(membc==n)
    for (o in members) {
        for (l in members) {
            if (o!=l) mpiapairs[o,l] = mpiapairs[o,l] + 1
        }
    }
}

# test

hits = NULL
alln = NULL
maxvals = table(rmaxs)
for (t in 1:length(maxvals)) {
    threshold = as.integer(names(table(rmaxs)))[t]-1
    alln[t] = length(which(ntimes>threshold))
    thehits = 0
    for (i in which(ntimes>threshold)) {
        theline = mpiapairs[rix[i],]
        if (theline[rcix[i]]>0) thehits = thehits + 1
    }
    hits[t] = thehits
}

plot(hits/alln, type="l", ylim=c(0,1), xaxt="n", xlab="threshold (minimum shared)")
axis(1, at=1:length(hits), labels=names(table(rmaxs)))


library(mpia)

dmgr = DomainManager()
dmgr$tempdir = "~/Documents/werkstatt/mpia-package/cache/"

d = Domain(name="safeprescribing")
d$corpus(dtm)
d$spacify()

dmgr$add(d)

# calculate visual data
#d = dmgr$get(name="bawe3")

d$proximityThreshold = 0.3
d$identityThreshold = 0.7
d$calculateTermProximities()
d$visualiser$calculateNetCoords()
d$visualiser$calculateReliefContour()

plot(d, method="topographic")
toponymy(d)

dmgr$materialise(d$signature)


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# edit distances

cards = cardsorts[,3:19]
rownames(cards) = cardsorts[,2]

newcards = matrix(ncol=(nrow(cards)+1), nrow=0)
colnames(newcards)[2:ncol(newcards)] = rownames(cards)
colnames(newcards)[1] = "group"

for (i in 1:ncol(cards)) {
    
    participant = colnames(cards)[i]
    groups = names(table(cards[,i]))
    pcards = cards[,i]
    for (g in groups) {
        members = which(pcards==g)
        gvec = rep(0,length(pcards))
        gvec[members] = 1
        gvec2 = as.matrix(t(c( paste("group", g), gvec)))
        rownames(gvec2) = participant
        newcards = rbind(newcards, gvec2)

    }
    
}

write.table(newcards, "~/mpia/examples-medical/newcards.csv")



# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# visual accuracy


dmgr = DomainManager()
dmgr$tempdir = "~/Documents/werkstatt/mpia-package/cache/"
d = dmgr$get(name="safeprescribing")

#d = Domain(name="safeprescribing")

#d$proximityThreshold = 0.3
#d$identityThreshold = 0.7
#d$calculateTermProximities()

#d$visualiser$calculateNetCoords()
#d$visualiser$calculateReliefContour()

plot(d, method="topographic")

lsavecs = d$space$tk %*% diag(d$space$sk)
t2t = cosine(t(lsavecs))

dim(t2t)

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

#sum(t2tnorm - distances)/(nrow(t2t)*ncol(t2t)-length(diag(t2t)))
#(sum((1-t2tnorm) - distances) - length(diag(t2t))) / (nrow(t2t)*ncol(t2t)-length(diag(t2t)))
(sum((1-t2tnorm) - distances)) / (nrow(t2t)*ncol(t2t)-length(diag(t2t)))
