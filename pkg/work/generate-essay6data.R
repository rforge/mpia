
corpus = "~/Documents/werkstatt/lsa-package/lsa/pkg/lsa/work/corpus/corpus.6.base"

tm = Corpus(DirSource(corpus), readerControl=list( reader = readPlain, language = "german",
	load = TRUE, removePunctuation=TRUE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE) # language = "german",
)

corpus = "~/Documents/werkstatt/lsa-package/lsa/pkg/lsa/work/corpus/corpus.6.base"

tm = Corpus(DirSource(corpus), readerControl=list( reader = readPlain, language = "german",
load = TRUE, removePunctuation=TRUE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE) # language = "german",
)

tm = tm_map(tm, function(e) return(gsub("[\u0093\u0092\u0091\u0084]", "", e)))
tm = tm_map(tm, tolower)
tmorig = tm

tm = tm_map(tm, stemDocument, language="ger")

dict = Dictionary(TermDocumentMatrix(tmorig, control=list(removePunctuation=TRUE, stopwords=FALSE, minWordLength=1, removeNumbers=TRUE)))

#tm_map(tm, stemCompletion, dictionary=dict, type="first")
#for (i in 1:length(tm))
#tm[[i]] = stemCompletion(as.character(tm[[i]]), dictionary=dict)

dtm = TermDocumentMatrix(tm, control = list(
#weighting = weightTfIdf,
#language="german",
removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE,
minWordLength = 3, bounds = list(global=c(1,Inf))
))

dtms = dtm
#dtms = removeSparseTerms(dtm, 0.98)

dim(dtms)

sc = as.character( stemCompletion(rownames(dtms), dictionary=dict, type="shortest") )
sc[which(is.na(sc))] = rownames(dtms)[which(is.na(sc))]

dtmsold = dtms
#dtms = dtmsold

rownames(dtms) = sc
if (any(duplicated(rownames(dtms)))) {
   
   dupes = which(duplicated(rownames(dtms)))
   for (i in dupes) {
      
      cat(paste("removing dupe for ", sc[i], "\n", sep=""))
		hits = which(sc == sc[i])
      target = hits[ which(! hits %in% which(duplicated(sc))) ]
      dtms[ target, ] = colSums(as.matrix(dtms[ hits[which(hits != target)], ]))
      
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

space = lsa(dtms, dims=dimcalc_raw())

tr = Trace(t(as.matrix(dtms)) %*% as.matrix(dtms))
# 2671
sum(space$sk^2)
# 2671


plot(space$sk*space$sk, type="l", ylab="singular values", ylim=c(0,max(space$sk)+1), xlab="index")
grid(10,10)

tr * 0.8
sum((space$sk^2)[1:29])

qplot(1:length(space$sk),  space$sk*space$sk, geom="path", ylab="eigenvalues", xlab="index") + geom_path(aes(29, sum((space$sk^2)[1:29])

# with 80/20 area under curve
qplot(1:length(space$sk),  space$sk*space$sk, geom="path", ylab="eigenvalues", xlab="index") + geom_vline(xintercept=29, col="darkgray", linetype=2) + geom_area(aes(1:29,  (space$sk*space$sk)[1:29]), bg=hsv(0.3,0.8,0.8, alpha=0.5)) + geom_area(aes(29:length(space$sk),  (space$sk*space$sk)[29:length(space$sk)]), bg=hsv(0.6,0.8,0.8, alpha=0.5)) +
geom_text(aes(34,40,label = "20%"), cex=5, col=hsv(0.6,0.8,0.8, alpha=0.8))+geom_text(aes(24,50,label = "80%"), cex=5, col=hsv(0.3,0.8,0.8, alpha=0.8))

# with sk and sk^2
qplot(1:length(space$sk),  space$sk*space$sk, geom="path", ylab="eigenvalues = s^2", xlab="index") + geom_path(aes(1:length(space$sk), space$sk), col="red", linetype = 2)

#dims = dimcalc_share(share=0.5)(space$sk)
dims = dimcalc_var()(space$sk)

space$tk = space$tk[, 1:dims]
space$dk = space$dk[, 1:dims]
space$sk = space$sk[ 1:dims ]

dims

dmgr = DomainManager()
dmgr$tempdir = "~/Documents/werkstatt/mpia-package/cache/"
d = Domain(name="essayscoring")
d$setSpace(space)
dmgr$add(d)



# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# from demo(lsa_essayscoring)

data(corpus_training)
weighted_training = corpus_training * gw_entropy(corpus_training)

space = lsa( weighted_training, dims=dimcalc_raw() )

dtm = weighted_training
tr = sum(dtm*dtm)

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
# 14 for 0.8

dimcalc_share(s=0.5)(space$sk)
# 18

space$tk = space$tk[, 1:cutoff]
space$dk = space$dk[, 1:cutoff]
space$sk = space$sk[ 1:cutoff ]

data(corpus_essays)
weighted_essays = corpus_essays * gw_entropy(corpus_training)
lsaEssays = fold_in( weighted_essays, space )

essay2essay = cor(lsaEssays, method="spearman")
goldstandard = c( "data6_golden_01.txt", "data6_golden_02.txt", "data6_golden_03.txt" )
machinescores = colSums( essay2essay[goldstandard, ] ) / 3

data(corpus_scores)
humanscores = corpus_scores

cor.test(humanscores[names(machinescores),], machinescores, exact=FALSE, method="spearman", alternative="two.sided")



# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -



# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# extract best results from lsa-dimensions-3

setwd('mpia/dims')

results_all = read.table("sample.csv", header=T)
dim(results_all) # should be 100k out of the 1.3m

# find highest cors for each essay collection

results = results_all
results = results[which(!is.na(results$rho)),] # only successful results
results = results[which(results$pvalue<0.05),]  # only significant results

collections = names(table(results$essays))

maxr = NULL
for (n in 1:length(collections)) {
    
	collection = collections[n]
	res_e = results[which(results$essays == collection),]
    
	maxr[n] = which(results$essays == collection)[ which(res_e[,"rho"] == max(res_e[, 'rho'])) ]
    
	print(paste(round(results[maxr[n],'rho'],2),' at ', round(results[maxr[n],'pvalue'],4),' for collection ',collection, sep=''))
}

results.filtered = results[maxr,]

cbind(as.character(results.filtered$essays), round(results.filtered$rho,2))


# -  -  -  -  -  -  -  -  -  -  -  -  -  -
# recalc for each essay collection

essays.content = read.csv2(file="essays.content.csv", stringsAsFactors=FALSE)

collections = names(table(results$essays))

collection = collections[1]

#for (collection in collections) {
    
    nr = as.integer(strsplit(collection, ".", fixed=TRUE)[[1]][2])
    
    # -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
    # file base in
    
    basedir = paste("~/mpia/dims/essays.base/domain/specific/corpus.", nr,sep="")
    seeddir = paste("~/mpia/dims/essays.base/seed/corpus.", nr, ".golden", sep="")
    
    fcorpus = c( dir(basedir, recursive=TRUE, full.names=TRUE), dir(seeddir, recursive=TRUE, full.names=TRUE) )
    length(fcorpus)
    
    fnessays = basename( c( dir(basedir, recursive=TRUE), dir(seeddir, recursive=TRUE)) )
    length(fnessays)

    genericdir = paste("~/mpia/dims/essays.
    base/domain/wi-buch-495/", sep="")
    generic2dir = paste("~/mpia/dims/essays.base/base/", sep="")

    gennr = min( length(dir(basedir, recursive=TRUE))*2, length(dir(genericdir, recursive=TRUE)) )
    rndsample = sample(1:length(dir(genericdir, recursive=TRUE)), gennr)
    
    fcorpus = c(fcorpus, c( dir(genericdir, recursive=TRUE, full.names=TRUE), dir(generic2dir, recursive=TRUE, full.names=TRUE)) [rndsample])
    length(fcorpus)
    
    fnessays = c(fnessays, c( dir(genericdir, recursive=TRUE), dir(generic2dir, recursive=TRUE))[rndsample])
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
    base.content = cbind(fnessays, content)

    # -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
    # calc space from base
    
    tm = Corpus(
        VectorSource(base.content[,2]),
        readerControl=list(
        reader=readPlain, language="de",
        load=TRUE, removePunctuation=TRUE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE # was: stopwords=TRUE and minWordLength=3
        )
    )
    tm = tm_map(tm, function(e) return(gsub("[^a-zA-Z0-9äöüÄÖÜß]", " ", e)))
    tm = tm_map(tm, tolower)
    dict = Terms(DocumentTermMatrix(
        tm,
        control=list(removePunctuation=TRUE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE)
    ))
    length(dict)
    tm = tm_map(tm, stemDocument, language="ger")
    dtm = TermDocumentMatrix(tm, control = list(
        removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE, # was: stopwords=TRUE and minWordLength=3
        minWordLength = 3, bounds = list(global=c(1,Inf)) # was 2
    ))
    dim(dtm)
    
    sc = as.character( stemCompletion(rownames(dtm), dictionary=dict, type="shortest") )
    sc[which(is.na(sc))] = rownames(dtm)[which(is.na(sc))]
    rownames(dtm) = sc
    if (any(duplicated(rownames(dtm)))) {
        dupes = which(duplicated(rownames(dtm)))
        for (i in dupes) {
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
    
    freqs = rowSums(as.matrix(dtm))
    lower = which(freqs>1)
    dtm = dtm[lower,]
    freqs = rowSums(as.matrix(dtm))
    upper = which(freqs < ncol(dtm)/2)
    dtm = dtm[upper,]

    data(stopwords_de)
    dtm = dtm[-which(rownames(dtm) %in% stopwords_de),]

    empty = as.integer( which(colSums(as.matrix(dtm))==0) )
    if (length(empty)>0) dtm = dtm[,-(empty)]
    
    dtm = as.matrix(dtm)
    class(dtm) = "textmatrix"
    
    space = lsa(dtm, dims=dimcalc_raw())
    
    length(space$sk)
    
    # superfast
    tr = sum(dtm*dtm)
    tr
    
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
    
    space$tk = space$tk[, 1:cutoff]
    space$dk = space$dk[, 1:cutoff]
    space$sk = space$sk[ 1:cutoff ]

    #class(space) = "LSAspace"

    # file essays in

    thekey = paste( "data",nr, "_", sep="")
    theix = which( substr(essays.content$fnessays,1,nchar(thekey)) == thekey)
    theessays = essays.content[theix,]
    
    # re-add golden
    theessays = rbind(theessays, base.content[which(base.content[,1] %in% gold),])
    
    etm = matrix(ncol=0, nrow=(nrow(dtm)))
    rownames(etm) = rownames(dtm)
    for (i in 1:nrow(theessays)) {
        etm = cbind(etm, query(theessays[i,2], rownames(dtm)))
    }
    colnames(etm) = theessays[,1]
    

    # fold in
    lsaEssays = fold_in( etm, space )
    
    essay2essay = cor(lsaEssays, method="spearman")
    humanscores = read.csv(paste("~/mpia/dims/essays.scores/corpus.", nr, ".grades", sep=""), sep=" ", header=FALSE, row.names="V1")

    goldstandard = gold
    machinescores = colSums( essay2essay[goldstandard, 1:(ncol(essay2essay)-3)] ) / 3

    cor.test( humanscores[names(machinescores),1], machinescores, exact=FALSE, method="spearman", alternative="two.sided")

    proxs = cosine(etm)
    machinescorescos = colSums( proxs[goldstandard, 1:(ncol(proxs)-3)] ) / 3

    cor.test( humanscores[names(machinescorescos),1], machinescorescos, exact=FALSE, method="spearman", alternative="two.sided")

    # now try with mpia

    dmgr = DomainManager()
    d = Domain(name=collection)
    d$setSpace(space)
    dmgr$add(d)
    
    d$proximityThreshold = 0.1
    d$identityThreshold = 0.7 # was 0.7
    #d$calculateTermProximities()
    
    #d$visualiser$calculateNetCoords()
    #d$visualiser$calculateReliefContour()



    ppl = HumanResourceManager(domainmanager=dmgr, domain=d)
    d$traces = matrix(ncol=0, nrow=0)

    goldpers = ppl$add( name="tutor" )
    goldpers$perform( theessays[(nrow(theessays)),2], activity="gold1", purpose="exam")
    goldpers$perform( theessays[(nrow(theessays)-1),2], activity="gold2", purpose="exam")
    goldpers$perform( theessays[(nrow(theessays)-2),2], activity="gold3", purpose="exam")

    #overlap(performances(goldpers))
    terms(position(goldpers))

    theoverlaps = vector(mode="character", length=nrow(theessays)-3)

    msc = vector(mode="numeric", length=nrow(theessays)-3)
    msc2 = vector(mode="numeric", length=nrow(theessays)-3)
    for (i in 1:(nrow(theessays)-3)) {
        p = ppl$add( name=theessays[i,1] )
        theessays[i,2] = gsub("[^a-zA-Z0-9äöüÄÖÜß]", " ", theessays[i,2])
        p$perform( tolower(theessays[i,2]), activity="exam", purpose="exam")
        
        theoverlap = length( overlap(p[1], position(goldpers)) )
        if (length(theoverlap)==1) msc[i] = theoverlap else msc[i] = 0
        
        theoverlap2 = length( c(overlap(p[1], goldpers[1]), overlap(p[1], goldpers[1]), overlap(p[1], goldpers[1])) )
        if (length(theoverlap2)==1) msc2[i] = theoverlap2 else msc2[i] = 0
        
        #print(paste(overlap(p[1], position(goldpers)), collapse=","))
        theoverlaps[i] = paste(overlap(p[1], position(goldpers)), collapse=", ")
        print(theoverlap)
        print(theoverlap2)
        print(i)
        #print(theoverlap2)
        
    }
    names(msc) = theessays[1:length(msc),1]
    names(msc2) = theessays[1:length(msc),1]

    as.data.frame(cbind( as.integer(msc), theoverlaps ))

    #msc
    #msc2

    cor.test( humanscores[names(msc),1], msc, exact=FALSE, method="spearman", alternative="two.sided")
    cor.test( humanscores[names(msc2),1], msc2, exact=FALSE, method="spearman", alternative="two.sided")

    plot((sort(msc)-min(msc))/(max(msc)-min(msc)), type="l")
    msc2 = sort(msc, index.return=TRUE)
    lines((humanscores[names(msc),1][msc2$ix]-min(humanscores[,1]))/(max(humanscores[,1])-min(humanscores[,1])), col="red")

    #msc2 = sort(msc, index.return=TRUE)
    #plot(msc2$x/max(msc), humanscores[names(msc),1][msc2$ix]/max(humanscores))
    
    d$proximityThreshold = 0.3
    needed = terms(position(goldpers))
    # [1] "daten"         "implementiert" "deklariert"    "public"        "variable"      "private"       "angemeldet"
    # [8] "nachname"      "zugegriffen"   "information"   "hiding"        "auszulesen"    "vorteil"       "zugriff"
    # [15] "zugreifen"

    has = terms(p[1])
    missed = needed[! needed %in% has]

#}




