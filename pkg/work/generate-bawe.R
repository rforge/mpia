

library(mpia)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# BAWE essay corpus

setwd("~/Documents/werkstatt/mpia-package/bawe/2539/")

bawe = read.csv("2539/documentation/BAWE.csv")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# filtering the BAWE corpus for the relevant submissions

# not all are essays
table(bawe$genre.family)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# basic stats

genres = c( unlist(strsplit(tolower(as.character(bawe$genre.family)), c(" + ","+ "), fixed=TRUE)), rep("", length(which(bawe$genre.family==""))))
genres = gsub(" $", "", genres)
table(genres)

gnames = names(table(genres))
dnames = names(table(unique(bawe$disciplinary.group)))

gf2dg = matrix(0, ncol=length(dnames), nrow=length(gnames))
rownames(gf2dg) = gnames
colnames(gf2dg) = dnames

for (gn in gnames) {
    
    for (disc in dnames) {
        
        if (gn!="") sel = grep(gn, tolower(as.character(bawe$genre.family)), fixed=TRUE) else sel = which(bawe$genre.family=="")
        sel2 = which(tolower(bawe$disciplinary.group[sel])==tolower(disc))
        gf2dg[which(rownames(gf2dg)==gn), which(colnames(gf2dg)==disc)] = length(sel2)
        
    }
    
}
gf2dg
sum(gf2dg) # be aware: some entries conflate multiple writings into one!!

#write.csv(gf2dg[-1,-1], file="genre2disc.csv")


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# restrict to essays, case studies, explanations, recounts

# some have double or triple categories, so let's restrict to all that contain "essays"
essays = grep("essay", tolower(bawe$genre.family))

# the corpus documentation says this should be 1238, but there are only
length(essays) # 1225

# since essays are more popular in arts and humanities + social sciences,
# let's add case studies, explanations, narrative recounts (for life and physical sciences)

casestudies = grep("case study", tolower(bawe$genre.family))
explanations = grep("explanation", tolower(bawe$genre.family))
recounts = grep("narrative recount", tolower(bawe$genre.family))

# some basic word stats

mean(bawe[essays,"words"])
mean(bawe[casestudies,"words"])
mean(bawe[explanations,"words"])
mean(bawe[recounts,"words"])

sd(bawe[essays,"words"])
sd(bawe[casestudies,"words"])
sd(bawe[explanations,"words"])
sd(bawe[recounts,"words"])

all = unique(c(essays, casestudies, explanations, recounts))

mean(bawe[all, "words"])
sd(bawe[all, "words"])

boxplot(list(essays=bawe[essays,"words"], cases=bawe[casestudies,"words"], explanations=bawe[explanations,"words"], recounts=bawe[recounts,"words"], all=bawe[all, "words"]))

# indicates: majority is somewhat in a similar length around

# let's remove the outliers

bs = boxplot.stats(bawe[all,"words"])
allclean = all[-which(bawe[all,"words"] %in% bs$out)]
wd = getwd()

#corpus = paste(wd,"/2539/CORPUS_TXT/",bawe[allclean,"id"],".txt", sep="")
#file.copy(corpus, "selection/")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# further selection: only those who have at least 1

bawe.sel = bawe[allclean,]
studsnrs = table(bawe.sel$student_id)
studs = names(studsnrs)[which(as.integer(studsnrs)>2)]
studs = studs[ which(as.integer( studsnrs[studs] )<7) ]
bawe.sel2 = bawe.sel[which(bawe.sel$student_id %in% studs),]

rm(bawe)
gc()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# move selected files to tempdir

corpus = paste(wd,"/2539/CORPUS_TXT/",bawe.sel2[,"id"],".txt", sep="")
file.copy(corpus, "selection2/")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# file in

a = proc.time()

tm = Corpus(
    DirSource("selection2/", recursive=FALSE),
    readerControl=list(
        reader=readPlain, language="english", # was: ger
        load=TRUE, removePunctuation=TRUE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE
    )
)

tm = tm_map(tm, function(e) return(gsub("[^[:alnum:]]", " ", e)))
tm = tm_map(tm, tolower)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# stemming

# save full dictionary for stem completion
dict = Terms(DocumentTermMatrix(
    tm,
    control=list(removePunctuation=TRUE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE)
))

length(dict)

# stemming
tm = tm_map(tm, stemDocument, language="english")
dtm = TermDocumentMatrix(tm, control = list(
    removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE,
    minWordLength = 3, bounds = list(global=c(3,Inf))
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

# sanity check: empty terms?

if (any(rownames(dtm) == "")) {
    cat("removing empty ones")
    dtm = dtm[-(which(rownames(dtm) == "")), ]
}

dim(dtm)

# spelling errors, roman numbers, dna sequences
#erroneous = grepl(".*([[:alpha:]])\\1{2,}.*", rownames(dtm))
#dtm = dtm[!erroneous,]
#dict = dict[!erroneous]
#dim(dtm)


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

#save(dtm, file="bawe-sel2-v2.RData")

#table(as.character(bawe.sel2$discipline))

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

freqs = rowSums(as.matrix(dtm))

#plot(sort(freqs, dec=T), type="l", log="xy", ylab="frequency (log-scaled)", xlab="terms (log-scaled)")

#greater10 = length(which(freqs > ncol(dtm)/10))
#greater25 = length(which(freqs > ncol(dtm)/4))
#greater50 = length(which(freqs > ncol(dtm)/2))

#just5 = length(which(freqs>5))
#just10 = length(which(freqs>5))

#rect(greater10,ncol(dtm)/10, just5, 5, lty="dotted", col=rgb(0.3,0.3,0.3, alpha=0.1))

#text(greater10+20, 10+ncol(dtm)/10, paste(greater10, "> 10%", sep=" "), col="darkgreen", cex=0.7)
#abline(h=ncol(dtm)/10, lty="dashed", col="darkgreen")

#text(greater25+15, 20+ncol(dtm)/4, paste(greater25, "> 25%", sep=" "), col="darkgreen", cex=0.7, font=2)
#abline(h=ncol(dtm)/4, lty="dashed", col="darkgreen")

#text(greater50+2, 20+ncol(dtm)/2, paste(greater50, "> 50%", sep=" "), col="darkgreen", cex=0.7)
#abline(h=ncol(dtm)/2, lty="dashed", col="darkgreen")

#abline(v=just5, lty="dotted", col="darkred")
#text(just5-50, 4, paste(just5,">5",sep=" "), col="darkred", cex=0.7)

#abline(v=just10, lty="dotted", col="darkred")
#text(just10-50, 9, paste(just10,">10",sep=" "), col="darkred", cex=0.7)


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# clip matrix of words to keywords within bounds

lower = which(freqs>10)
dtm = dtm[lower,]

freqs = rowSums(as.matrix(dtm))
upper = which(freqs < ncol(dtm)/10)
dtm = dtm[upper,]

empty = as.integer( which(colSums(as.matrix(dtm))==0) )
if (length(empty)>0) dtm = dtm[,-(empty)]

dim(dtm)

# t2t size:
nrow(dtm)^2 * 8 / 1024 / 1024
# 15.97449 MB

dtm = as.matrix(dtm)
class(dtm) = "textmatrix"
class(dtm) = c("TermDocumentMatrix", class(dtm))

proc.time() - a

save(dtm, file="dtm-10pcnt-10-v2.RData")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# stretch truncation

# superfast
tr = sum(dtm*dtm)
tr
# was: 503886

tr * 0.8
# was: 403108.8


a = proc.time()
space = lsa(dtm, dims=dimcalc_raw())
proc.time() - a


space = lsa(dtm, dims=300)

#save(space, file="space-10-10-300-v2.RData")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# find ideal truncation

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
# 285 for 0.8 # was: 259 for 0.8

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
# truncate by hand

space$tk = space$tk[, 1:cutoff]
space$dk = space$dk[, 1:cutoff]
space$sk = space$sk[ 1:cutoff ]

#save(space, file="space-10-10-trunc80-v2.RData")


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


# mpia

dmgr = DomainManager()
dmgr$tempdir = "~/Documents/werkstatt/mpia-package/cache/"
#dmgr$tempdir = "~/bawe/cache/"

d = Domain(name="bawe2")
d$setSpace(space)
dmgr$add(d)


#-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
#-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# clean way for the above truncation

dmgr = DomainManager()
dmgr$tempdir = "~/Documents/werkstatt/mpia-package/cache/"

d = Domain(name="bawe3")
dmgr$add(d)

d$corpus(dtm)
d$spacify()

# calculate visual data
#d = dmgr$get(name="bawe3")

d$proximityThreshold = 0.3
d$identityThreshold = 0.7
d$calculateTermProximities()
d$visualiser$calculateNetCoords()
d$visualiser$calculateReliefContour()

# persistance
dmgr$materialise(d$signature)

#-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# visualise

dmgr = DomainManager()
dmgr$tempdir = "~/Documents/werkstatt/mpia-package/cache/"
d = dmgr$get(name="bawe3")



pdf(width=(21-8.8)/2.54, height=(21-8.8)/2.54, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-bawe/bawe3-final-map.pdf")

plot(d, method="topographic")

dev.off()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# essays

pdf(width=(21-8.8)/2.54, height=(21-8.8)/2.54, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-bawe/bawe3-final-positions-sociology.pdf")

set.seed(22031977)
firstnames = c(
    readLines(dir("~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/names/", full.names=TRUE)[1], warn=F),
    readLines(dir("~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/work/names/", full.names=TRUE)[2], warn=F)
)

plot(d, method="topographic")

d$traces = matrix(ncol=0, nrow=0)
ppl = HumanResourceManager(domainmanager=dmgr, domain=d)

disciplines = unique(as.character(bawe.sel2$discipline))
studentids = unique(as.character(bawe.sel2$student_id))
students.names = firstnames[sample(length(firstnames),length(studentids))]

cs = c("firebrick", "chartreuse4", "darkorchid4", "turquoise2", "deeppink3", "orange4", "cadetblue4", "goldenrod2", "deepskyblue2", "darkgray", 11:30)

nr = 0
for (i in 1:length(studentids)) {

   studname = studentids[i]
   essaysel = which(bawe.sel2$student_id == studname)

   if (any(bawe.sel2[essaysel,"discipline"]=="Sociology")){

       nr = nr + 1

       s = students.names[i]
       p = ppl$add( name=students.names[i] )
       #assign(tolower(s), ppl$add(name=s))

       for (n in 1:length(essaysel)) {
           dsname = as.character(bawe.sel2[essaysel[n],"discipline"])
           essaytext = paste( readLines( paste(wd,"/2539/CORPUS_TXT/",bawe.sel2[essaysel[n],"id"],".txt", sep=""), warn=FALSE), collapse=" ")
           essaytext = gsub("[^[:alnum:]]", " ", essaytext)
           p$perform( essaytext, activity=paste("exam in ", dsname, sep=""), purpose=dsname, label=as.character(bawe.sel2[essaysel[n],"title"]))
           #get(tolower(s))$perform( essaytext, activity=paste("exam in ", dsname, sep=""), purpose=dsname, label=as.character(bawe.sel2[essaysel[n],"title"]))

       }
       
       plot(p, col=cs[nr], label=FALSE, component.labels=FALSE, component.arrows=F, dot.cex=1.5)
       #plot(path(p), col=cs[nr], label=FALSE, component.labels=FALSE, component.arrows=FALSE, dot.cex=1, alpha=0.5)

   } # if Sociology

}

dev.off()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# paths

pdf(width=(21-8.8)/2.54, height=(21-8.8)/2.54, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-bawe/bawe3-final-paths-sociology.pdf")

plot(d, method="topographic")

for (n in 1:length(ppl$all())) {
    p = ppl$people[[n]]
    plot(path(p), col=cs[n], label=FALSE, component.labels=FALSE, component.arrows=FALSE, dot.cex=1, alpha=0.5)
}

dev.off()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# further analysis

melonie = ppl$last()
near(ppl, melonie)

uriel = ppl$people[[1]]
constantino = ppl$people[[7]]

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# show visually

pdf(width=(21-8.8)/2.54, height=(21-8.8)/2.54, file="~/Documents/denkfabrik/dissertation/workodrome-II/examples-bawe/bawe3-final-3pathsandpos-sociology.pdf")

plot(d, method="topographic")

plot(path(uriel), col=cs[1], label=FALSE, component.labels=FALSE, component.arrows=FALSE, dot.cex=1, alpha=1)
plot(position(uriel), col=cs[1], label=TRUE, component.arrows=F, dot.cex=3, component.labels=FALSE)

plot(path(constantino), col=cs[7], label=FALSE, component.labels=FALSE, component.arrows=FALSE, dot.cex=1, alpha=1)
plot(position(constantino), col=cs[7], label=TRUE, component.arrows=F, dot.cex=3, component.labels=FALSE)

plot(path(melonie), col=cs[10], label=FALSE, component.labels=FALSE, component.arrows=FALSE, dot.cex=1, alpha=1)
plot(position(melonie), col=cs[10], label=TRUE, component.arrows=F, dot.cex=3, component.labels=FALSE)

dev.off()


# more details

pfs = as.list( c( performances(melonie), performances(uriel), performances(constantino)  ))
class(pfs) = "Performance"

proxs = proximity(pfs)
diag(proxs) = 0

nears = (proxs>d$proximityThreshold)*1
nears = nears[1:length(performances(melonie)),(length(performances(melonie))+1):ncol(nears)]

rownames(nears) = unlist(lapply(performances(melonie), names))
colnames(nears) = c( unlist(lapply(performances(uriel), names)), unlist(lapply(performances(constantino), names)) )


equals = (proxs>=d$identityThreshold)*1
equals = equals[1:length(performances(melonie)),(length(performances(melonie))+1):ncol(equals)]

rownames(nears) = unlist(lapply(performances(melonie), names))
colnames(nears) = c( unlist(lapply(performances(uriel), names)), unlist(lapply(performances(constantino), names)) )



overlap(melonie[3],uriel[1])


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# visual accuracy


dmgr = DomainManager()
dmgr$tempdir = "~/Documents/werkstatt/mpia-package/cache/"
d = dmgr$get(name="bawe3")

#d$proximityThreshold = 0.3
#d$identityThreshold = 1 # was 0.7
#d$calculateTermProximities()

#d$visualiser$calculateNetCoords()
#d$visualiser$calculateReliefContour()

lsavecs = d$space$tk %*% diag(d$space$sk)
t2t = cosine(t(lsavecs))

dim(t2t)

# t2t - termProximites
sum(t2t - d$termProximities)/(nrow(t2t)*ncol(t2t)-length(diag(t2t)))
# 0.01549215

plot(d, method="topographic")

map = d$visualiser$netcoords

distances = matrix(ncol=nrow(map), nrow=nrow(map))
for (i in 1:nrow(map)) {
    for (n in 1:nrow(map)) {
        distances[i,n] = sqrt( (map[i,1]-map[n,1])^2 + (map[i,2]-map[n,2])^2)
    }
}

t2tnorm = (t2t-min(t2t)) / (max(t2t) - min(t2t))

(sum((1-t2tnorm) - distances)) / (nrow(t2t)*ncol(t2t)-length(diag(t2t)))



