
# space database from LTfLL

spaces = read.csv2("~/Documents/denkfabrik/dissertation/workodrome-II/examples-medical/spaces.csv", sep="\t")
spaces[c(7,22),]

library(mpia)

# convert old doc term matrix (from previous tm version)

setwd("~/mpia/136")
load("tdm_full.RData")
dtm = attr(tdm, "Data")

dim(dtm)
# 112670 235830

# check for dna sequences, spelling errors: repetition with rep of char > 3

rownames(dtm)[grepl(".*([[:alpha:]])\\1{2,}.*", rownames(dtm))]

# might have been wise to prune the matrix for outliers first?

# pruning

freqs = rowSums(dtm)
length( which(freqs > 1) )
length( which(freqs > 2) )
length( which(freqs > 5) )
lower = which(freqs>5)
dtmred = dtm[lower,]

freqs2 = rowSums(dtmred)
upper = which(freqs2 < ncol(dtm)/5)
length( upper )
dtmred = dtmred[upper,]

# filter out spelling mistakes, obvious dna sequences
dtmred = dtmred[!grepl(".*([[:alpha:]])\\1{2,}.*", rownames(dtmred)),] # 34986

# filter out words with less than 4 characters
dtmred = dtmred[ which(nchar(rownames(dtmred)) >= 4), ]

# filter more dna sequences
dtmred = dtmred[ !grepl("^[acgt]{3,}$", rownames(dtmred)), ] # 34961

write.csv(rownames(dtmred), file="terms-clean.csv")
save(dtmred, file="dtmred-gf-6-20pcnt-repcharcleaned-min4chars-dnacleaned.RData")


# frequency plot

freqs = rowSums(dtm)
fsorted = sort(freqs, dec=T)
plot( fsorted, type="l", ylim=c(0,10000))

# determine stretch truncation threshold value (trace80)

trace = sum( diag(dtmred %*% t(dtmred)) ) # 70688712 for full # 53660353 for pruned # 48541623 further pruned
trace80 = 0.8 * trace # 56550969.6 for full # 42928282 for pruned # 38833298 further pruned


# calc svd with a generous upper boundary for dims
# using svdlibc

source("svdlibc.R")
space = lsa_sparse(dtmred, ndim=10000, getRHS=FALSE)

# determine strech truncation cut off for dims

sum(space$sk^2)
eigenv = space$sk^2
eigenvsum = NULL
for (i in 1:length(eigenv)) {
   eigenvsum[i] = sum(eigenv[1:i])
}
dims80 = which(eigenvsum > (trace80))[1]




# finally: truncate space

space$tk <<- space$tk[, 1:dims80]
space$dk <<- space$dk[, 1:dims80]
space$sk <<- space$sk[1:dims80]

