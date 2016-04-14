
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

data(stopwords_en)

docs = matrix(nrow=0, ncol=2)
colnames(docs) = c("id", "title")

docs = rbind( docs, c("c1", "a web interface for social media applications")) # web social interface
docs = rbind( docs, c("c2", "Review of access time restrictions to web system usage")) # web access review system time user
docs = rbind( docs, c("c3", "Content management system usage of the HTML 5 interface")) # interface system html management
docs = rbind( docs, c("c4", "Error spotting in HTML: social system versus software system")) # social 2x system html
docs = rbind( docs, c("c5", "Barriers to access and time spent in social mobile apps")) # access time social

docs = rbind( docs, c("m1", "The generation of random unordered trees")) # trees
docs = rbind( docs, c("m2", "A survey of divisive clustering along the intersection of partial trees")) # trees clustering intersection
docs = rbind( docs, c("m3", "Width and height of trees in using agglomerative clustering with Agnes")) # trees clustering agglomerative
docs = rbind( docs, c("m4", "Agglomerative clustering algorithms: a review")) # clustering agglomerative review

docs = rbind( docs, c("p1", "The intersection of learning and organisational knowledge sharing"))
docs = rbind( docs, c("p2", "A transactional perspective on teaching and learning"))
docs = rbind( docs, c("p3", "Innovations in online learning: moving beyond no significant difference"))
docs = rbind( docs, c("p4", "Tacit knowledge management in organisational learning"))
docs = rbind( docs, c("p5", "Knowledge building: theory, pedagogy, and technology"))

docs2 = docs[,2]
docs2 = tolower(docs2)
docs2 = gsub("[^[:alnum:]]", " ", docs2)
docs2 = gsub("[[:space:]]+", " ", docs2)
docs2 = lapply(docs2, function (e) { unlist(strsplit(e, " ")) })
data(stopwords_en)
docs2 = lapply(docs2, function (e) { e[! (e %in% stopwords_en) ] })
tabs = lapply(docs2, function(e){sort(table(e), dec=T)})
tabs2 = lapply(tabs, function(e) { data.frame(docs = "", terms = names(e), Freq = e, row.names = NULL) })
for (i in 1:nrow(docs)) { tabs2[[i]][,1]= docs[i,1] }

dtm = t(xtabs(Freq ~ ., data = do.call("rbind", tabs2)))
dtm = dtm[-which(rowSums(dtm)<=1),] # at least in more than a single document
dtm

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
# SVD

ata = dtm %*% t(dtm)
aat = t(dtm) %*% dtm

u = eigen(ata)$vectors
v = eigen(aat)$vectors

s = sqrt( eigen(ata)$values )
#s[which(is.na(s))] = 0

sred = s
sred[4:length(s)] = 0

round( u %*% diag(s)[,1:13] %*% t(v), 1)

dtm2 = u %*% diag(s)[,1:ncol(v)] %*% t(v)

dtmred = u %*% diag(sred)[,1:ncol(v)] %*% t(v)

rownames(dtm2) = rownames(dtm)
colnames(dtm2) = colnames(dtm)

round(dtm2,1)

rownames(dtmred) = rownames(dtm)
colnames(dtmred) = colnames(dtm)

round(dtmred,1)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# compare this with the standard package routines

round( as.textmatrix(lsa(dtm, dims=3)), 1)


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# eigenvector example with small matrix

b = cbind( c(1,2), c(2,1))
x = rbind( c(1,-1), c(1,1) )
s = c(-1,3)
b %*% x 
diag(s) %*% x
b %*% t(x)
b %*% x == diag(s) %*% x

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# do the math by hand

library(MASS)
library(pracma)

space = lsa(dtm, dims=dimcalc_raw())

round ( (space$tk) %*% ( diag(space$sk) %*% t(diag(space$sk)) ) %*% t(space$tk),1 ) # aat
dtm %*% t(dtm)

sst = t(space$tk) %*% dtm %*% t(dtm) %*% space$tk
sts = t(space$dk) %*% t(dtm) %*% (dtm) %*% (space$dk)

u = space$tk
v = space$dk
s = space$sk
a = dtm

round ( pinv(a%*%t(a)) %*% u %*% ( t(u) %*% a %*% t(a) %*% u )[,1:14] %*% diag(s[1:14]) %*% (( t(v) %*% (t(a)%*%a) %*% v ) %*% t(v) %*% pinv(t(a) %*% a))[1:14,], 1)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# by hand using eigen instead

# WARNING: eigen can produce complex solutions for algebraic real solutions(!)
# due to the algorithm used, this seems to break the example used!!

a=dtm

ata = a %*% t(a)
aat = t(a) %*% a

u = eigen(ata)$vectors
v = eigen(aat)$vectors

sts = eigen(ata)$values
sst = eigen(aat)$values

s = sqrt(eigen(ata)$values)
s[which(is.na(s))] = 0

# s = sqrt(sqrt(eigen(ata)$value^2)) # to avoid complex numbers due to problematic eigen implementation

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# how the eigenvalues and eigenvectors construct the original matrix

# the big roundabout through quadratification

round ( pinv(a%*%t(a)) %*% u %*% ( t(u) %*% a %*% t(a) %*% u )[,1:14] %*% diag(s[1:14]) %*% (( t(v) %*% (t(a)%*%a) %*% v ) %*% t(v) %*% pinv(t(a) %*% a))[1:14,], 1)

#ata x = l x
#
#round(ata %*% eigen(ata)$vectors, 1) == round( diag(eigen(ata)$values) %*% eigen(ata)$vectors,1) # not true!!
#
#ata x = x diag(l)
#round(ata %*% eigen(ata)$vectors, 1) == round( eigen(ata)$vectors %*% diag(eigen(ata)$values),1) # true!!
#
#t(diag(l) x^-1) = x diag(l)
#t(x^-1) diag(l) = x diag(l)
#round(eigen(ata)$vectors,1) == round( t(pinv(eigen(ata)$vectors)),1) # true!!
#
#round ( t( diag(eigen(ata)$values) %*% pinv(eigen(ata)$vectors)), 1) == round(eigen(ata)$vectors %*% diag(eigen(ata)$values),1)
#
#ata x = t(diag(l) x^-1)
#round(ata %*% eigen(ata)$vectors, 1) == round(  t( diag(eigen(ata)$values) %*% pinv(eigen(ata)$vectors)) ,1) # true!!
#
#ata x = t(x^-1) t(diag(l)) = t(x^-1) diag(l)
#ata x = t(x^-1) diag(l)
#round(ata %*% eigen(ata)$vectors, 1) == round( t(pinv(eigen(ata)$vectors)) %*% diag(eigen(ata)$values) ,1) # true!!
#
#ata x x^-1 = t(x^-1) diag(l) x^-1
#round(ata %*% eigen(ata)$vectors %*% pinv(eigen(ata)$vectors), 1) == round( t(pinv(eigen(ata)$vectors)) %*% diag(eigen(ata)$values) %*% pinv(eigen(ata)$vectors),1) # true!!
#
#ata = t(x^-1) diag(l) x^-1
#round(ata, 1) == round( t(pinv(eigen(ata)$vectors)) %*% diag(eigen(ata)$values) %*% pinv(eigen(ata)$vectors),1) # true!!
#
#t(ata) = t( t(x^-1) diag(l) x^-1 )
#t(ata) = t(x^-1) diag(l) x^-1
#

#ata x x^-1 = l x x^-1 # proof fall out: ata == round(ata %*% eigen(ata)$vectors %*% pinv(eigen(ata)$vectors),1)
#ata = l x x^-1
#

#ata = x diag(l) x^-1
#ata == round( eigen(ata)$vectors %*% diag(eigen(ata)$values) %*% (pinv(eigen(ata)$vectors))  ,1)

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  

# the s matrices are not the same! their length differs (n and m!)!
#pinv(a%*%t(a)) %*% u %*% ( t(u) %*% a %*% t(a) %*% u ) %*% s %*% ( t(v) %*% t(a) %*% a %*% v ) %*% u %*% pinv(a%*%t(a))
