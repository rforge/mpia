# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
#       Class: Visualiser
#
#       fridolin.wild@open.ac.uk
#       last update: August 17, 2013
#
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

Visualiser <- setRefClass( "Visualiser",

   fields = list(

		domain="ANY",
		type="character",
		mode = "character",

      netcoords = "ANY",
      prestigeTerms = "ANY",
      prestigeDocs = "ANY",
      wireframe = "ANY",
      mapData = "ANY",

      perspective="logical",

		version = "numeric"

   ),

   methods = list(

      initialize = function( domain, ... ) {
         
         callSuper(...)

         if (missing(domain)) stop("Domain missing!")
         domain <<- domain
         
         netcoords <<- NULL
         prestigeTerms <<- NULL
         prestigeDocs <<- NULL
         wireframe <<- NULL
         mapData <<- NULL
         
         type <<- "persp"
         mode <<- "terminology"
         perspective <<- TRUE
         
         version <<- 0.60
         
      },

      print = function() {
         cat(paste("An object of class 'Visualiser'.\n", sep=""))
      },

      show = function () {
         cat(paste("An object of class 'Visualiser'.\n", sep=""))
      }

   )
) # Class Visualiser


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# copy field data, recreate objects and copy their field data

Visualiser$methods(
   copy = function( shallow=FALSE, domain ) {
      
      def <- .refClassDef
      value <- new(def, domain) # added domain parameter
      vEnv <- as.environment(value)
      selfEnv <- as.environment(.self)
      for (field in names(def@fieldClasses)) {
         if (field!="domain"){
            if (shallow)
               assign(field, get(field, envir = selfEnv), envir = vEnv)
            else {
               current <- get(field, envir = selfEnv)
               if (is(current, "envRefClass"))
                  current <- current$copy(FALSE)
               assign(field, current, envir = vEnv)
            }
         }
      }
      value
      
   }
) # method: copy()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# calculateNetCoords(): calculate 2D map coordinates

Visualiser$methods(
calculateNetCoords = function( method="network" ) { # , cluster=FALSE
   
   if (is.null(domain$termProximities)) {
      cat("calculateNetCoords(): termProximities not yet calculated - calling calculateTermProximities()\n")
      if (domain$calculateTermProximities( mode=.self$mode )) cat("succeeded: TermProximities calculated.\n")
   }
   
   fn = tempfile()
   grDevices::pdf( width=10, height=10, file=fn)
   
   xy = grDevices::dev.size("px")
   width = round(xy[1])
   height = round(xy[2])
   
   if (method=="network") {
      
      prestigeTerms <<- sna::prestige(as.matrix(domain$termProximities))
      
      v2 = round(log(round(prestigeTerms)),1)*10
      v2[which(is.infinite(v2))] = 0
      v2 = (v2+1)/max(v2+1)
      
      # REPLACE WITH gplot.layout.kamadakawai !!
      #n = network(as.matrix(domain$termProximities), directed=FALSE)
      #p = plot(n, mode = "kamadakawai",
      #   # mode = "fruchtermanreingold",
      #   usearrows=F, vertex.cex=v2, # edge.lwd=lwds,
      #   displaylabels=FALSE, #boxed.labels=FALSE, label.pad=0.2, label.cex=1,
      #   displayisolates=TRUE, usecurve=FALSE
      #)
      
      p = sna::gplot.layout.kamadakawai(as.matrix(domain$termProximities), layout.par=list())
      colnames(p) = c("cx","cy")
      
   } else {
      
      # would be nice to have e.g. a sammon mapping as well!
      stop("calculateNetCoords(): This method is not supported.")
      
   }
   
   dev.off()
   
   # clipping: remove empty margins
   
   xmin = min(p[,"cx"])
   xmax = max(p[,"cx"])
   ymin = min(p[,"cy"])
   ymax = max(p[,"cy"])
   p[,"cx"] = p[,"cx"] - xmin
   p[,"cx"] = p[,"cx"]/(xmax-xmin)
   p[,"cy"] = p[,"cy"] - ymin
   p[,"cy"] = p[,"cy"]/(ymax-ymin)
   
   # store
   
   #rownames(p) = network.vertex.names(n)
   rownames(p) = rownames(domain$termProximities)
   netcoords <<- p
   
   return(invisible(TRUE))
   
}
) # method: calculateNetCoords()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# calculateReliefContour(): calculate wireframe with
# elevation representing node density

Visualiser$methods(
   calculateReliefContour = function( nrow=100, ncol=100 ) {
      
      if ( is.null(.self$netcoords) ) {
         cat("calculateReliefContour(): netcoords not yet calculated. calling calculateNetCoords() now - this may take a while.\n")
         if ( .self$calculateNetCoords() ) cat("success: calculateNetCoords is finished.\n")
      }
      
      # count nodes in each quadrant of the nr*nc grid
      
      nr = nrow
      nc = ncol
      
      hills = matrix(0, ncol=nc,nrow=nr)
      for (i in 1:nrow(.self$netcoords)) {
         
         node = .self$netcoords[i,]
         
         #x = node["cx"] %/% (1/nc)
         x = round(node["cx"] / (1/nc))
         if (x == 0) x = 1
         
         #y = node["cy"] %/% (1/nr)
         y = round(node["cy"] / (1/nr))
         if (y == 0) y = 1
         
         hills[ y, x ] = hills[ y, x ] + 1
         
      } # for each node
      
      # smoothen
      wireframe <<- fields::image.smooth(fields::image.smooth(hills)$z, theta=1)$z
      
      # alternative smootheners
      # wireframe = matrixSmooth(hills)
      # wireframe = setup.image.smooth( nrow=nrow(hills), ncol=ncol(hills),  dx=ncol(hills), dy=nrow(hills), theta=.25, xwidth=2.5, ywidth=2.5)
      
      return(invisible(TRUE))
      
   }
) # method: calculateReliefContour()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# plotMap(): 2D or 3D visualisation of the term2term similarity map data

Visualiser$methods(
   plotMap = function( method=.self$type, rotated=TRUE, name="map", contour=TRUE, focus=.self$mode ) {
      
      # focus mode %in% c("terminology", "corpus", "both", "traces")
      
      #if (!missing(rotated)) .self$perspective = rotated
      if (!missing(method)) .self$type = method
      
      if (is.null(.self$wireframe)) {
         cat("plotMap(): wireframe not yet calculated, calling calculateReliefContour() - this may take a while.\n")
         if (.self$calculateReliefContour()) cat("success: calculateReliefContour is finished.\n")
      }
      
      if (method=="topographic") {
         
         .self$perspective = FALSE
         
         # cs = gray( 0:20/20 )
         # cs =  terrain.colors(20)
         # cs = topo.colors(40)
         
         cs = .self$topo.colors.pastel()
         
         par(list(mar=c(0,0,0,0)))
         
         # interpolate: resolution x 10
         wf = .self$wireframe
         nw = ncol(wf)*5
         nh = nrow(wf)*5
         obj = list( x=(1:ncol(wf)), y=(1:nrow(wf)), z=wf )
         loc = fields::make.surface.grid( list( x=seq(1,ncol(wf),,nw), y=seq(1,nrow(wf),,nh) ) )
         wf2 = fields::interp.surface( obj, loc)
         dim(wf2) = c(nw,nh)
         #wf2 = image.smooth(image.smooth(wf2)$z, theta=4)$z # was 2 = nice hills!
         wf2 = fields::image.smooth(wf2, theta=1)$z # was 2 = nice hills!
         
         #image( wf2, col = cs, xaxt="n", yaxt="n")
         .self$mapData = graphics::image( wf2, col = cs, xaxt="n", yaxt="n")
         
         if (contour) graphics::contour( wireframe,nlevels=21, add=TRUE, col=gray(0,alpha=0.3), lty=1, lwd=2, method="simple", drawlabels=FALSE)
         
      } else if (method=="wireframe") {
         
         .self$perspective = TRUE
         
         # should become obsolete! persp is better!
         
         cs = .self$topo.colors.pastel()
         xy = dev.size("in")
         
         .self$mapData = lattice::wireframe(
            .self$wireframe, shade = TRUE, aspect = c(61/87, 0.2), light.source = c(10,0,10),
            scales = list(
            arrows = FALSE,
            distance=c(.75,.75,0.25), cex=1, col="black",
               z = list(draw = FALSE),
               #x = list(at=0:10*10, labels=seq(0,1, by=(1/100)*10)),
               #y = list(at=0:10*10, labels=seq(0,1, by=(1/100)*10))
               x=list(draw=FALSE), y=list(draw=FALSE)
            ),
            par.settings=list(
               layout.widths = list( x=xy[1], units="in", left.padding=0, right.padding=0 ),
               layout.heights = list( main.key.padding=0, x=xy[2], units="in", top.padding=-25, bottom.padding=-25 ),
               axis.line = list(col=NA,lty=1,lwd=1)
            ),
            drape = FALSE,
            colorkey = FALSE,
            screen=c(10,0,10),
            ylab="", xlab="", zlab="", main="",
            perspective=FALSE
         )
         plot(mapData)
         
      } else if (method=="persp") {
         
         .self$perspective = TRUE
         
         tileColours = function( x, col=.self$topo.colors.pastel() ) {
            # drop the edges and average facet corners -> gives (nx - 1)(ny - 1) facet colours
            x.avg = (x[-1, -1] + x[-1, -(ncol(x) - 1)] + x[-(nrow(x) -1), -1] + x[-(nrow(x) -1), -(ncol(x) - 1)]) / 4
            # actual colours matrix
            colors = col[cut(x.avg, breaks = length(col), include.lowest = TRUE)]
            return(colors)
         }
         
         par( list(mar=c(0,0,0,0), bg="white", xaxt="n", yaxt="n", xpd=TRUE))
         
         if (rotated) {
            b = FALSE
            border = "black"
            theta = 10
            phi = 30
         } else {
            b = FALSE
            border = "black"
            theta = 0
            phi = 90
         }
         
         .self$mapData = graphics::persp(
         
            .self$wireframe,
            col=tileColours(.self$wireframe, col=.self$topo.colors.pastel()), #topo.colors(40)
            border=border, # tile border frame colour
            
            scale=FALSE,
            expand=0.5,
            
            xlab="x", ylab="y",
            zlab="density",
            main="", sub="",
            
            box=b,
            axes=b, nticks=10, ticktype="simple", # use "detailed" for tick marks
            
            #theta=10, phi=30, # viewing angles: theta: angle in xy plane; phi: angle to z axis
            theta=theta, phi=phi,
            shade=TRUE, ltheta=70, lphi=45 # light source position: ltheta=angle in xy plane; lphi: angle to z axis
         
         )
         
      } else if (method=="contour") {
         
         par(mar=c(0,0,0,0)) # needed to inform filled.contour
         plot.new()
         
         .self$perspective = FALSE
         
         .self$mapData = graphics::filled.contour( .self$wireframe,
            color.palette=.self$topo.colors.pastel, axes=F, frame.plot=F,
            key.axes=list(draw=F)
         )
         
      } else {
         
         stop("Method not supported.")
         
      } # if method
      
   }
) # method: plotMap()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# topo.colors.pastel(): helper function to generate nice colors for topographic levels in pastel shades

Visualiser$methods(
   topo.colors.pastel = function ( n = 21) {
      
      j = n %/% 3 # greens
      k = n %/% 3 # brown/gray/whites
      i = n - j - k # blues
      alpha = 1
      cs = c(
         grDevices::hsv(h = seq.int(from = 38/60, to = 31/60, length.out = i), s=0.5, v=0.8, alpha = alpha),
         grDevices::hsv(h = seq.int(from = 18/60, to = 8/60, length.out = j), s=0.3, v=seq.int(from=0.6,to=1, length.out=j), alpha = alpha),
         grDevices::hsv(h = seq.int(from = 7.8/60, to = 6/60, length.out = k), s=seq.int(from = 0.3, to = 0.1, length.out = k), alpha = alpha, v = seq.int(from = 0.85, to = 1, length.out = k))
      )
      
      return(cs)
   }
) # method: topo.colors.pastel()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# toponymy(): add term labels (to map or standalone)

Visualiser$methods(
   toponymy = function( gridsize=c(10,10), method = "mountains", add=TRUE, col=NULL, grid.col="white" ) {
      
      if (missing(grid.col) && (.self$type=="topographic" || .self$type=="contour") ) grid.col = "white" else grid.col="black"
      
      if (add==FALSE && missing(method)) {
         .self$type = "topographic"
         .self$perspective = FALSE
      }

      if (add==FALSE && method=="all") {
         .self$type = "topographic"
         .self$perspective = FALSE
      }
      
      if (.self$perspective && is.null(.self$mapData)){
         stop("toponymy(): no mapData found, but needed for perspective projection - you have to run plotMap(rotated=TRUE) first!\n")
      }
      
      if (.self$type=="wireframe") {
         stop("toponymy(): labels currently not supported for type 'wireframe'!\n")
      }
      
      if (is.null(.self$wireframe) || is.null(.self$netcoords)) {
         cat("toponymy(): no wireframe and/or netcoords found, but needed for placement - calling calculateReliefContour() and calculateNetCoords!\n")
         success = .self$calculateNetCoords()
         if (success) success = .self$calculateReliefContour() else stop("toponymy(): error while calculating netcoords")
         if (!success) stop("toponymy(): error while calculating relief contour.")
      }
      
      if ( add==FALSE && ( is.null(dev.list()) || (dev.cur()==1) || is.null(.self$mapData) ) ) {
         par(mar=c(0,0,0,0))
         plot.new()
      }
      
      if (method=="mountains") {
         
         nc = .self$netcoords
         wf = .self$wireframe
         
         spanX = ncol(wf) %/% gridsize[1]
         spanY = nrow(wf) %/% gridsize[2]
         
         for (x in (gridsize[1]-1):0) { # reversed now to limit breaking the perspective
            for (y in (gridsize[2]-1):0) { # reversed now to limit breaking the perspective
               
               mountain=list()
               
               mountain$rangeX = NULL
               mountain$rangeY = NULL
               mountain$index = NULL
               mountain$max = NULL
               mountain$x = NULL
               mountain$y = NULL
               mountain$offsetX = NULL
               mountain$offsetY = NULL
               mountain$wfsubset = NULL
               
               if (x+1<=gridsize[1]) mountain$rangeX = ((x*spanX)+1):((x+1)*spanX) else rangeX = (x*spanX+1):(nrow(wf))
               if (y+1<=gridsize[2]) mountain$rangeY = ((y*spanY)+1):((y+1)*spanY) else rangeY = (y*spanY+1):(ncol(wf))
               
               mountain$wfsubset = wf[mountain$rangeY, mountain$rangeX]
               mountain$index = which(mountain$wfsubset == max(mountain$wfsubset))[1]
               mountain$max = max(mountain$wfsubset)
               
               mountain$x = (mountain$index %/% spanY)
               
               mountain$y = (mountain$index %% spanY)
               if (mountain$y==0) mountain$y = spanY else mountain$x = mountain$x + 1
               
               if ( mountain$max != mountain$wfsubset[mountain$y, mountain$x] ) stop("max not found")
               
               mountain$offsetX = ( mountain$rangeX[mountain$x]  ) / ncol(wf)
               mountain$offsetY = ( mountain$rangeY[mountain$y] ) / nrow(wf)
               
               xIx = NULL
               ax = which(nc[,1]>mountain$offsetX)
               bx = which(nc[,1] < (mountain$offsetX+ (1/ncol(wf))))
               xIx = ax[ which( ax %in% bx )]
               
               yIx = NULL
               ay = which(nc[,2]>mountain$offsetY)
               by = which(nc[,2] < (mountain$offsetY+ (1/nrow(wf))))
               yIx = ay[ which( ay %in% by )]
               
               Ix = names( xIx[which(xIx %in% yIx )] )
               if (length(Ix) == 0) {
                  Ix = "" #paste(mountain$rangeX[mountain$x], mountain$rangeY[mountain$y], collapse=", ")
               } else if (length(Ix > 1)) {
                  Ix = Ix [1]
               }
               
               if (Ix!="") .self$labelFlag(mountain$offsetX, mountain$offsetY, Ix, col=col)
               
            } # for grid y
         } # for grid x
         
      } else if (method == "gridprestige") {
         
         nc = .self$netcoords
         wf = .self$wireframe
         
         gridsize=c(5,5)
         
         spanX = 1 / gridsize[1]
         spanY = 1 / gridsize[2]
         
         for (x in (gridsize[1]-1):0) { # reversed now to limit breaking the perspective
            for (y in (gridsize[2]-1):0) { # reversed now to limit breaking the perspective
               
               cell = list()
               cell$xa = which(nc[,1]< ((x+1)*spanX))
               cell$xb = which(nc[,1]>= (x*spanX))
               cell$ya = which(nc[,2]< ((y+1)*spanY))
               cell$yb = which(nc[,2]>= (y*spanY))
               cell$xb = as.integer(cell$xb)
               cell$xa = as.integer(cell$xa)
               cell$ya = as.integer(cell$ya)
               cell$yb = as.integer(cell$yb)
               
               cell$a = cell$xa[ which( cell$xa %in% cell$xb ) ]
               cell$b = cell$ya[ which( cell$ya %in% cell$yb ) ]
               cell$ab = cell$a[ which( cell$a %in% cell$b)]
               
               # any termvectors in this cell?
               if (length(cell$ab) != 0) {
                  
                  cell$prestige = .self$prestigeTerms[cell$ab]
                  cell$names = rownames(nc)[cell$ab]
                  
                  cell$prestIx = sort(cell$prestige, index.return=TRUE, dec=TRUE)$ix
                  cell$x = nc[cell$ab[cell$prestIx[1]],1]
                  cell$y = nc[cell$ab[cell$prestIx[1]],2]
                  cell$name = cell$names[cell$prestIx[1]]
                  
                  if (cell$name!="") {
                     
                     v2 = round(log(round( .self$prestigeTerms )) ,1)*10
                     v2[which(is.infinite(v2))] = 0
                     v2 = (v2+1)/(2*max(v2+1))
                     v2 = v2 [cell$ab[ cell$prestIx[1] ]]
                     
                     .self$labelFlag(cell$x, cell$y, cell$name, bg=rgb(1,0.7,0.7, alpha=0.9), cex=v2*2, col=col)
                     
                  }
                  
               } # any termvectors in this cell?
               
            } # for grid y
         } # for grid x
         
         a = gc()
         
      } else if (method == "gridcenter") {
         
         if (.self$perspective) warning("toponymy(): This method works best in 2D, you should plotMap(rotated=FALSE)!")
         
         nc = .self$netcoords
         wf = .self$wireframe
                  
         gridsize=c(5,5)
         
         spanX = 1 / gridsize[1]
         spanY = 1 / gridsize[2]
         
         for (x in (gridsize[1]-1):0) { # reversed now to limit breaking the perspective
            for (y in (gridsize[2]-1):0) { # reversed now to limit breaking the perspective
               
               cell = list()
               cell$xa = which(nc[,1]< ((x+1)*spanX))
               cell$xb = which(nc[,1]>= (x*spanX))
               cell$ya = which(nc[,2]< ((y+1)*spanY))
               cell$yb = which(nc[,2]>= (y*spanY))
               cell$xb = as.integer(cell$xb)
               cell$xa = as.integer(cell$xa)
               cell$ya = as.integer(cell$ya)
               cell$yb = as.integer(cell$yb)
               
               cell$a = cell$xa[ which( cell$xa %in% cell$xb ) ]
               cell$b = cell$ya[ which( cell$ya %in% cell$yb ) ]
               cell$ab = cell$a[ which( cell$a %in% cell$b)]
               
               # any termvectors in this cell?
               if (length(cell$ab) >= 3) {
                  
                  cell$prestige = .self$prestigeTerms[cell$ab]
                  cell$names = rownames(nc)[cell$ab]
                  
                  cell$prestIx = sort(cell$prestige, index.return=TRUE, dec=TRUE)$ix
                  cell$x = ((x+0.5)*spanX)
                  cell$y = ((y+0.5)*spanY)
                  
                  cell$name = paste(cell$names[cell$prestIx[1:3]], collapse=",\n")
                  
                  if (cell$name!="") {
                     labelFlag(cell$x, cell$y, cell$name, bg=rgb(1,1,1, alpha=0.7), col=col)
                  }
                  
               } # any termvectors in this cell?
               
            } # for grid y
         } # for grid x
         
         a = gc()
         
      } else if (method=="all"){
         
         if (is.null(.self$prestigeTerms)) {
            cat("toponymy(): the graph 'prestige' values have not yet been calculated for the term vectors - calling calculateNetCoords(). This may take a while.\n")
            if (.self$calculateNetCoords()) cat("success: calculateNetCoords is finished.\n")
         }
                  
         v2 = round(log(round(.self$prestigeTerms)),1)*10
         v2[which(is.infinite(v2))] = 0
         v2 = (v2+1)/max(v2+1)
         
         colours = round( (v2-min(v2))/(max(v2)-min(v2)), 1) * 10 + 1
         hypsometricTints = grDevices::heat.colors(11, alpha=0.8)[11:1]
         shades = grDevices::gray(seq(0.9,0,by=-1/12), alpha=0.3)
         if (is.null(col)) cs = hypsometricTints[colours] else cs = rep(col, nrow(.self$netcoords))
         
         labels = rownames(.self$netcoords)
         
         if (!is.null(gridsize) && gridsize!=FALSE && .self$perspective==FALSE && .self$type != "persp") grid(gridsize[1], gridsize[2], col=grid.col)
         
         #text(x=netcoords[,"cx"], y=netcoords[,"cy"], labels=labels, cex=v2, pos=4, col=cs)
         #points(netcoords, cex=v2, pch=4, col=shades[colours])
         
         for (l in 1:nrow(.self$netcoords)) {
            .self$labelFlag( netcoords[l,"cx"], netcoords[l,"cy"], labels[l], cex=v2[l], col=cs[l], marker.col=shades[colours][l])
         }
         
      } else {
         stop ("toponymy(): the requested method is not supported.")
      }
      
      #return(invisible(.self))
      
   }
) # method: toponymy


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# labelFlag(): add labels on top of the hills

Visualiser$methods(

   labelFlag = function( x, y=NULL, label, border="black", bg=grDevices::gray(1,alpha=0.9), cex=0.5, box=TRUE, col=NULL, marker.col=c(grDevices::gray(0,alpha=1), grDevices::gray(1, alpha=1)) ) {
      
      if (missing(y)) {
         if (length(x)==2) {
            y = x[2]
            x = x[1]
         } else {
            stop("labelFlag(): y coordinate is missing!")
         }
      }
      
      if (.self$perspective) { #  || .self$type=="persp"

         # if 3D
         
         if (missing(col) || is.null(col)) col = "black"
         
         nr = nrow(.self$wireframe)
         nc = ncol(.self$wireframe)
         
         #xr = (x %/% (1/nc))
         xr = round(x / (1/nc))
         #yr = (y %/% (1/nr))
         yr = round(y / (1/nr))
         
         if (xr==0) xr = 1
         if (xr>nr) xr = nr
         if (yr==0) yr = 1
         if (yr>nc) yr = nc
         
         z = .self$wireframe[ yr, xr  ]

         pixelx = 2 / dev.size(units="px")[1]
         coords3d = grDevices::trans3d( y, x, z+0.1, pmat = .self$mapData)
         coords3d2 = grDevices::trans3d( y, x, z, pmat = .self$mapData)

         # +pixelx
         graphics::segments( coords3d$x, coords3d$y, coords3d2$x, coords3d2$y, col=marker.col[1], lwd=4)
         graphics::segments( coords3d$x, coords3d$y, coords3d2$x, coords3d2$y, col=marker.col[2], lwd=2)
         
         if (box) {
            plotrix::boxed.labels(coords3d$x, coords3d$y, label, bg=bg, border=border, xpad=1.5, ypad=2, cex=cex, col=col)
         } else {
            plotrix::boxed.labels(coords3d$x, coords3d$y, label, bg="transparent", border="transparent", xpad=1.5, ypad=2, cex=cex, col=col)
         }
         
      } else {
         
         # if 2D
         
         if (missing(box)) box = FALSE
         if (missing(cex)) cex = 0.8
         if (missing(col) || is.null(col)) col = gray(0,alpha=0.4)
         
         # correction required for contourplot since color scale key cannot be suppressed
         if (.self$type=="contour") x = x/(1+0.8/par()$din[1])
         
         pixelx = 1 / dev.size(units="px")[1]
         pixely = 1 / dev.size(units="px")[2]
         
         x2 = x
         x = y
         y = x2
         
         graphics::points(x,y, cex=cex, pch=4, col=marker.col[1], lwd=2)
         
         if (box) {
            plotrix::boxed.labels(x+pixelx, y+pixely, label, bg=bg, border=border, xpad=1.5, ypad=2, cex=cex, col=col)
         } else {
            plotrix::boxed.labels(x, y+6*pixely, label, bg="transparent", border="transparent", xpad=1.5, ypad=2, cex=cex, col=col)
         }
         
         # fix for trellis graphics (wireframe):
         # library(grid)
         # grid.text("some text", x=unit(0.2, "npc"), y=unit(0.2, "npc"), gp=gpar(fontsize=20, col="red"))
         
      } # if 2D
      
   }
) # method: labelFlag()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# plotPerformance(): plot performance point, labelflags for key term descriptors, arrows pointing towards them

Visualiser$methods(
   plotPerformance = function ( performance, polyMax=3, col="black", label=TRUE, component.labels=TRUE, component.arrows=TRUE, dot.cex=1 ) {

      if (class(performance)!="Performance") stop("Visualiser$plotPerformance(): expects a performance as parameter.")
      
      if (.self$mode=="terminology") {
         
         termDescriptors = performance$getActivatedTerms()
         
         if ( (!is.list(termDescriptors) || length(termDescriptors$labels)==0) ) {
            warning("No terms were activated by this performance.")
            return(invisible(FALSE))
         }
         
         # labelFlags for the constituent term vectors
         tnc = .self$netcoords[termDescriptors$tkix,]
         
         pixelx = 1 / dev.size(units="px")[1]
         pixely = 1 / dev.size(units="px")[2]
         
         weightedFocalPoint = NULL

         # if multiple term descriptors
         if (length(termDescriptors$labels)>1) {
            
            # weighted focal point
            wfp = (1/(sum(termDescriptors$values))) * (colSums(tnc*termDescriptors$values))
            
            # arrows that point towards constituent term positions
            if (component.arrows) {
               
               for (i in 1:nrow(tnc)) {
                  
                  cx = termDescriptors$values[ i ]
                  cxX = (tnc[i,1]-wfp["cx"])*0.1 #cxX
                  cxY = (tnc[i,2]-wfp["cy"])*0.1 #cxY
                  
                  if (.self$perspective) {
                     
                     xr = round(wfp["cx"]/(1/ncol(.self$wireframe)))
                     yr = round(wfp["cy"]/(1/nrow(.self$wireframe)))
                     if (xr==0) xr = 1
                     if (yr==0) yr = 1
                     z = .self$wireframe[yr, xr]
                     c3d1 = grDevices::trans3d(wfp["cy"], wfp["cx"],z+0.1, pmat=.self$mapData)
                     c3d2 = grDevices::trans3d(wfp["cy"]+cxY, wfp["cx"]+cxX, z+0.1, pmat=.self$mapData)
                     graphics::arrows( c3d1$x, c3d1$y, c3d2$x, c3d2$y, col=col, length=0.05, lwd=ceiling(log(cx+1)))
                     
                  } else {
                     
                     graphics::arrows( wfp["cy"], wfp["cx"], wfp["cy"] + cxY, wfp["cx"]+cxX, col=col, length=0.05, lwd=ceiling(log(cx+1)))
                     
                  }
                  
               } # for all terms
               
            # end of: if component arrows
            } else {
                
                if (.self$perspective) {
                    xr = round(wfp["cx"]/(1/ncol(.self$wireframe)))
                    yr = round(wfp["cy"]/(1/nrow(.self$wireframe)))
                    if (xr==0) xr = 1
                    if (yr==0) yr = 1
                    z = .self$wireframe[yr, xr]
                    c3d1 = grDevices::trans3d(wfp["cy"], wfp["cx"],z+0.1, pmat=.self$mapData)
                    c3d2 = grDevices::trans3d(wfp["cy"]+cxY, wfp["cx"]+cxX, z+0.1, pmat=.self$mapData)
                    graphics::points( c3d1$x, c3d1$y, pch=21, col="darkgray", cex=dot.cex, bg=col, lwd=0.5)
                } else {
                    graphics::points( wfp["cy"], wfp["cx"],  pch=21, col="darkgray", cex=dot.cex, bg=col, lwd=0.5)
                }

            } # if no component arrows
            
            # labels for these constituent vectors
            if (component.labels) {
               
               labelix = 1:(min(length(termDescriptors$labels),polyMax))
               for (i in labelix) { # 1:nrow(tnc)
                  cx = termDescriptors$values[i]
                  cx = log(cx+1)/2
                   .self$labelFlag(tnc[i,"cx"], tnc[i,"cy"], termDescriptors$labels[i], col=col, box=FALSE, cex=cx)
               } # for all terms
               
            }
            
            weightedFocalPoint = wfp
            
         } else { # if just a single term descriptor
            
            # cross instead of arrows
            if (component.arrows) {
               if (.self$perspective) {
                  
                  xr = round(tnc["cx"]/(1/ncol(.self$wireframe)))
                  yr = round(tnc["cy"]/(1/nrow(.self$wireframe)))
                  if (xr==0) xr = 1
                  if (yr==0) yr = 1
                  z = .self$wireframe[yr, xr]
                  c3d1 = grDevices::trans3d(tnc["cy"], tnc["cx"],z, pmat=.self$mapData)
                  
	               #segments( c3d1$y+5*pixely, c3d1$x+5*pixelx, c3d1$y-5*pixely, c3d1$x-5*pixelx, col=col, lwd=3)
	               #segments( c3d1$y-5*pixely, c3d1$x+5*pixelx, c3d1$y+5*pixely, c3d1$x-5*pixelx, col=col, lwd=3)
                  graphics::points(c3d1, pch=4, col=col, cex=1 )

               } else {
                  
	               #segments( tnc["cy"]+2*pixely, tnc["cx"]+2*pixelx, tnc["cy"]-2*pixely, tnc["cx"]-2*pixelx, col=col, lwd=3)
	               #segments( tnc["cy"]-2*pixely, tnc["cx"]+2*pixelx, tnc["cy"]+2*pixely, tnc["cx"]-2*pixelx, col=col, lwd=3)
                  graphics::points(tnc["cy"], tnc["cx"], pch=4, col=col, cex=1 )
                  
               }
               
            } # if component.arrows
            
            weightedFocalPoint = tnc
            
            cx = termDescriptors$values[1]
            cx = log(cx+1)/2
            
            if (component.labels) {
             	.self$labelFlag(tnc["cx"], tnc["cy"], termDescriptors$labels[1], col=col, border="transparent", box=FALSE, cex=cx)
            }
            
         }
         
         # labelFlag for weightedFocalPoint of the Performance
         if (label) .self$labelFlag(weightedFocalPoint["cx"], weightedFocalPoint["cy"], names(performance), col=col, cex=1, box=FALSE) # cy: +5*pixely
         # cex=log(mean(termDescriptors)+1)
         
         invisible( c( weightedFocalPoint["cx"], weightedFocalPoint["cy"]))
         
      } else if (.self$mode=="incidences" || .self$mode =="both" ) {
         
         # if documents or both
         
         # almost same calculation: focal point for trace vecs is calculated from both terms and doc vectors.
         # but maybe choose different labels (i.e. show only labels for component.vectors that are term vectors?)
         
         
      } else if (.self$mode=="evidence") {
         # if traces
         
      } else {
         
         # combinations
         
      }
      
   }
) # method: plotPerformance()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# plotPath(): plot a sequence of points (one per performance handed over), connect them with a xspline

Visualiser$methods(
   plotPath = function( performanceList, col="red",
		alpha = if (length(performanceList)>1) seq(0.3, 1, length.out=length(performanceList)) else 1,
		label = TRUE, component.labels = TRUE, component.arrows=TRUE, dot.cex=1, box = TRUE, connect = TRUE ) {
      
      if (length(performanceList)>1 && length(alpha)==1) shades = rep(scales::alpha(col, alpha), length(performanceList)) else shades = scales::alpha(col, alpha)
         
      path = NULL

      # checking whether performanceList is one or more performances
      if (is.list(performanceList)) pclasses = unlist(lapply(performanceList, function(e) class(e))) else pclasses=class(performanceList)[1]
      if (any(pclasses!="Performance") ) stop ("Visualiser$plotPath(): requires one or more performances as parameter.")

      path = NULL
      if (is.list(performanceList)) {
         for (i in 1:length(performanceList)) {
            # if it is a list of performances
            path = rbind(path, plotPerformance(performanceList[[i]], col=shades[i], label=label, component.labels=component.labels, component.arrows=component.arrows, dot.cex=dot.cex))
         }
      } else {
         # if it is just a single performance
       	path = t(plotPerformance(performanceList[[i]], col=col, label=label, component.labels=component.labels, component.arrows=component.arrows, dot.cex=dot.cex))
      }
      
      if (connect) {
         # add xspline or line connecting the performances
         if (.self$perspective || .self$type=="persp") {
            
            # 3D
            
            x = path[,2]
            y = path[,1]

            z = NULL
            for (i in 1:length(x)) {
               xr = round(x[i] / (1/ncol(.self$wireframe)))
               if (xr==0) xr = 1
               yr = round(y[i] / (1/nrow(.self$wireframe)))
               if (yr==0) yr = 1
               z = c(z, .self$wireframe[ xr, yr ])
            }
            if (nrow(path)>2) {
               graphics::xspline(grDevices::trans3d(x=x, y=y, z, pmat=.self$mapData), s=-1, border=col, lwd=3, open=TRUE) # c(0,rep(-1,nrow(path)-2),0)
            } else if (nrow(path)==2) {
               graphics::lines(grDevices::trans3d(x=path[,2],y=path[,1], z, pmat=.self$mapData), col=col, lwd=3)
            }

         } else {
            
            # 2D
            
            if (nrow(path)>2) {
               graphics::xspline(path[,c(2,1)], s=c(0,rep(-1,nrow(path)-2),0), border=col, lwd=3, open=TRUE)
            } else if (nrow(path)==2) {
               graphics::lines(path[,c(2,1)], col=col, lwd=3)
            }

         } # 3D/2D xspline/line connection
         
      } # connect with a line?
      
   }
) # method: plotPath()


# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# newDevice(): helper method: set up a new device with higher resolution (cross platform)

Visualiser$methods(
   newDevice = function(name="image", pdf=FALSE, filename=paste(name,".pdf",sep="")) {
      
      width = 11
      height = 9
      
      if (pdf) {
         grDevices::pdf(width=width, height=height, file=filename)
      } else {
         
         platform = sessionInfo()$platform
         if (grepl("linux",platform)) {
            grDevices::x11(width=width, height=height)
         } else if (grepl("pc",platform)) {
            grDevices::windows(width=width, height=height, xpinch=1024/width, ypinch=768/height)
         } else if (grepl("apple", platform)) {
            grDevices::quartz(title=name, width=width, height=height, dpi=100)
         } else { grDevices::dev.new() }
         
      }
      
   }
) # method: newDevice()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# closeDevice(): helper method: set up a new device with higher resolution (cross platform)

Visualiser$methods(
   closeDevice = function() {
      grDevices::dev.off()
   }
) # method: closeDevice()

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# override generics

#if (!isGeneric("summary")) setGeneric("summary", function(object, ...) standardGeneric("summary") )
setMethod("summary", signature=list(object="Visualiser"), function ( object, ... ) {

   object$show()

   cat("\n  visualisation data\n")
   cat(paste("    netcoords: ",!is.null(object$netcoords),"\n", sep=""))
   cat(paste("    wireframe: ",!is.null(object$wireframe),"\n", sep=""))
   cat(paste("    mapData: ",!is.null(object$mapData),"\n", sep=""))
   cat("\n")

})

