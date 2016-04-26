
m = diag(rep(1,6))
rownames(m) = letters[1:6]
colnames(m) = letters[1:6]

m["a","b"] = 1
m["b","a"] = 1

m["a","e"] = 1
m["e","a"] = 1

m["e","b"] = 1
m["b","e"] = 1

m["b","d"] = 1
m["d","b"] = 1

m["d","c"] = 1
m["c","d"] = 1

m["f","d"] = 1
m["d","f"] = 1

m["f","c"] = 1
m["c","f"] = 1

par(mfrow=c(3,3))
for (n in c(0:5, (1:3)*50)) {
    p = plot(network::network(m), mode="kamadakawai", layout.par=list(niter=n*50), main=paste(n, "iteration(s)"), vertex.col="white", vertex.cex=3, usearrows=FALSE)
}

# - - - - - - - - - - - - - - - - - - - - - - - - 

xmin = min(p[,"cx"])
xmax = max(p[,"cx"])
ymin = min(p[,"cy"])
ymax = max(p[,"cy"])
p[,"cx"] = p[,"cx"] - xmin
p[,"cx"] = p[,"cx"]/(xmax-xmin)
p[,"cy"] = p[,"cy"] - ymin
p[,"cy"] = p[,"cy"]/(ymax-ymin)

calculateReliefContour = function( netc, nrow=100, ncol=100 ) {
   nr = nrow
   nc = ncol
   hills = matrix(0, ncol=nc,nrow=nr)
   for (i in 1:nrow(netc)) {
      node = netc[i,]
      x = round(node["cx"] / (1/nc))
      if (x == 0) x = 1
      y = round(node["cy"] / (1/nr))
      if (y == 0) y = 1
      hills[ y, x ] = hills[ y, x ] + 1
   } # for each node
   wireframe <- fields::image.smooth(fields::image.smooth(hills)$z, theta=1)$z
   return(list(hills=hills, wireframe=wireframe))
}

res = calculateReliefContour(netc=p)

graphics::persp(res$hills, scale=FALSE,
	expand=0.5,
	border="white",

	xlab="x", ylab="y",
	zlab="density",
	main="", sub="",

	box=FALSE,
	axes=FALSE, nticks=10, ticktype="simple", # use "detailed" for tick marks


	theta=10, phi=30,
	shade=TRUE, ltheta=70, lphi=45 # light source position: ltheta=angle in xy plane; lphi: angle to z axis

)

# - - - - - - - - - - - - - - - - - - - - - - - -

graphics::persp(res$wireframe, scale=FALSE,
	expand=0.5,
	border="white",

	xlab="x", ylab="y",
	zlab="density",
	main="", sub="",

	box=FALSE,
	axes=FALSE, nticks=10, ticktype="simple", # use "detailed" for tick marks

	theta=10, phi=30,
	shade=TRUE, ltheta=70, lphi=45 # light source position: ltheta=angle in xy plane; lphi: angle to z axis

)

# - - - - - - - - - - - - - - - - - - - - - - - -

topo.colors.pastel = function ( n = 21) {
   j = n %/% 3 # terrain
   k = n %/% 3 # mountain
   i = n - j - k # water
   alpha = 1
   cs = c(
     grDevices::hsv(h = seq.int(from = 38/60, to = 31/60, length.out = i), s=0.5, v=0.8, alpha = alpha),
     grDevices::hsv(h = seq.int(from = 18/60, to = 8/60, length.out = j), s=0.3, v=seq.int(from=0.6,to=1, length.out=j), alpha = alpha),
     grDevices::hsv(h = seq.int(from = 7.8/60, to = 6/60, length.out = k), s=seq.int(from = 0.3, to = 0.1, length.out = k), alpha = alpha, v = seq.int(from = 0.85, to = 1, length.out = k))
   )
   return(cs)
}

graphics::barplot(rep(0.05,21), ylim=c(0,1), col=topo.colors.pastel(), xaxt="n", yaxt="n", space=0, border="black")

# - - - - - - - - - - - - - - - - - - - - - - - -

tileColours = function( x, col=.self$topo.colors.pastel() ) {
   x.avg = (x[-1, -1] + x[-1, -(ncol(x) - 1)] + x[-(nrow(x) -1), -1] + x[-(nrow(x) -1), -(ncol(x) - 1)]) / 4
   colors = col[cut(x.avg, breaks = length(col), include.lowest = T)]
   return(colors)
}

par( list(mar=c(0,0,0,0), bg="white", xaxt="n", yaxt="n", xpd=TRUE))

b = FALSE
border = "black"
theta = 10
phi = 30

mapData = graphics::persp(

	res$wireframe,
	col=tileColours(res$wireframe, col=topo.colors.pastel()), 
	border=border, # tile border frame colour

	scale=FALSE,
	expand=0.5,

	xlab="x", ylab="y",
	zlab="density",
	main="", sub="",

	box=b,
	axes=b, nticks=10, ticktype="simple", # use "detailed" for tick marks

	# viewing angles: theta: angle in xy plane; phi: angle to z axis
	theta=theta, phi=phi,
	shade=TRUE, ltheta=70, lphi=45 # light source position: ltheta=angle in xy plane; lphi: angle to z axis

)
