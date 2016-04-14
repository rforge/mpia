function (x, attrname = NULL, label = network.vertex.names(x),
coord = NULL, jitter = TRUE, thresh = 0, usearrows = TRUE,
mode = "fruchtermanreingold", displayisolates = TRUE, interactive = FALSE,
xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, pad = 0.2,
label.pad = 0.5, displaylabels = !missing(label), boxed.labels = FALSE,
label.pos = 0, label.bg = "white", vertex.sides = 50, vertex.rot = 0,
arrowhead.cex = 1, label.cex = 1, loop.cex = 1, vertex.cex = 1,
edge.col = 1, label.col = 1, vertex.col = 2, label.border = 1,
vertex.border = 1, edge.lty = 1, label.lty = NULL, vertex.lty = 1,
edge.lwd = 0, label.lwd = par("lwd"), edge.len = 0.5, edge.curve = 0.1,
edge.steps = 50, loop.steps = 20, object.scale = 0.01, uselen = FALSE,
usecurve = FALSE, suppress.axes = TRUE, vertices.last = TRUE,
new = TRUE, layout.par = NULL, ...)
{
   if (!is.network(x))
   stop("plot.network requires a network object.")
   if (network.size(x) == 0)
   stop("plot.network called on a network of order zero - nothing to plot.")
   bellstate <- options()$locatorBell
   expstate <- options()$expression
   on.exit(options(locatorBell = bellstate, expression = expstate))
   options(locatorBell = FALSE, expression = Inf)
   "%iin%" <- function(x, int) (x >= int[1]) & (x <= int[2])
   if (is.hyper(x)) {
      xh <- network.initialize(network.size(x) + sum(!sapply(x$mel,
      is.null)), directed = is.directed(x))
      for (i in list.vertex.attributes(x)) {
         set.vertex.attribute(xh, attrname = i, value = get.vertex.attribute(x,
         attrname = i, null.na = FALSE, unlist = FALSE),
         v = 1:network.size(x))
      }
      for (i in list.network.attributes(x)) {
         if (!(i %in% c("bipartite", "directed", "hyper",
         "loops", "mnext", "multiple", "n")))
         set.network.attribute(xh, attrname = i, value = get.network.attribute(x,
         attrname = i, unlist = FALSE))
      }
      cnt <- 1
      for (i in 1:length(x$mel)) {
         if (!is.null(x$mel[[i]])) {
            for (j in x$mel[[i]]$outl) {
               if (!is.adjacent(xh, j, network.size(x) + cnt))
               add.edge(xh, j, network.size(x) + cnt, names.eval = names(x$mel[[i]]$atl),
               vals.eval = x$mel[[i]]$atl)
            }
            for (j in x$mel[[i]]$inl) {
               if (!is.adjacent(xh, network.size(x) + cnt,
               j)) {
                  add.edge(xh, network.size(x) + cnt, j, names.eval = names(x$mel[[i]]$atl),
                  vals.eval = x$mel[[i]]$atl)
               }
            }
            cnt <- cnt + 1
         }
      }
      cnt <- cnt - 1
      if (length(label) == network.size(x))
      label <- c(label, paste("e", 1:cnt, sep = ""))
      xh %v% "vertex.names" <- c(x %v% "vertex.names", paste("e",
      1:cnt, sep = ""))
      x <- xh
      n <- network.size(x)
      d <- as.matrix.network(x, matrix.type = "edgelist", attrname = attrname)
      if (!is.directed(x))
      usearrows <- FALSE
   }
   else if (is.bipartite(x)) {
      n <- network.size(x)
      d <- as.matrix.network(x, matrix.type = "edgelist", attrname = attrname)
      usearrows <- FALSE
   }
   else {
      n <- network.size(x)
      d <- as.matrix.network(x, matrix.type = "edgelist", attrname = attrname)
      if (!is.directed(x))
      usearrows <- FALSE
   }
   if (NCOL(d) == 2) {
      if (NROW(d) == 0)
      d <- matrix(nrow = 0, ncol = 3)
      else d <- cbind(d, rep(1, NROW(d)))
   }
   diag <- has.loops(x)
   d[is.na(d)] <- 0
   edgetouse <- d[, 3] > thresh
   d <- d[edgetouse, , drop = FALSE]
   d.raw <- d
   if (!is.null(coord)) {
      cx <- coord[, 1]
      cy <- coord[, 2]
   }
   else {
      layout.fun <- try(match.fun(paste("network.layout.",
      mode, sep = "")), silent = TRUE)
      if (class(layout.fun) == "try-error")
      stop("Error in plot.network.default: no layout function for mode ",
      mode)
      temp <- layout.fun(x, layout.par)
      cx <- temp[, 1]
      cy <- temp[, 2]
   }
   if (jitter) {
      cx <- jitter(cx)
      cy <- jitter(cy)
   }
   use <- displayisolates | (((sapply(x$iel, length) + sapply(x$oel,
   length)) > 0))
   if (is.null(xlab))
   xlab = ""
   if (is.null(ylab))
   ylab = ""
   if (is.null(xlim))
   xlim <- c(min(cx[use]) - pad, max(cx[use]) + pad)
   if (is.null(ylim))
   ylim <- c(min(cy[use]) - pad, max(cy[use]) + pad)
   xrng <- diff(xlim)
   yrng <- diff(ylim)
   xctr <- (xlim[2] + xlim[1])/2
   yctr <- (ylim[2] + ylim[1])/2
   if (xrng < yrng)
   xlim <- c(xctr - yrng/2, xctr + yrng/2)
   else ylim <- c(yctr - xrng/2, yctr + xrng/2)
   baserad <- min(diff(xlim), diff(ylim)) * object.scale
   if (new) {
      plot(0, 0, xlim = xlim, ylim = ylim, type = "n", xlab = xlab,
      ylab = ylab, asp = 1, axes = !suppress.axes, ...)
   }
   if (is.character(vertex.cex) && (length(vertex.cex == 1))) {
      temp <- vertex.cex
      vertex.cex <- rep(get.vertex.attribute(x, vertex.cex),
      length = n)
      if (all(is.na(vertex.cex)))
      stop("Attribute", temp, "had illegal missing values or was not present in plot.graph.default.")
   }
   else vertex.cex <- rep(vertex.cex, length = n)
   vertex.radius <- rep(baserad * vertex.cex, length = n)
   if (is.character(vertex.sides) && (length(vertex.sides ==
   1))) {
      temp <- vertex.sides
      vertex.sides <- rep(get.vertex.attribute(x, vertex.sides),
      length = n)
      if (all(is.na(vertex.sides)))
      stop("Attribute", temp, "had illegal missing values or was not present in plot.graph.default.")
   }
   else vertex.sides <- rep(vertex.sides, length = n)
   if (is.character(vertex.border) && (length(vertex.border) ==
   1)) {
      temp <- vertex.border
      vertex.border <- rep(get.vertex.attribute(x, vertex.border),
      length = n)
      if (all(is.na(vertex.border)))
      vertex.border <- rep(temp, length = n)
      else {
         if (!all(is.color(vertex.border), na.rm = TRUE))
         vertex.border <- as.color(vertex.border)
      }
   }
   else vertex.border <- rep(vertex.border, length = n)
   if (is.character(vertex.col) && (length(vertex.col) == 1)) {
      temp <- vertex.col
      vertex.col <- rep(get.vertex.attribute(x, vertex.col),
      length = n)
      if (all(is.na(vertex.col)))
      vertex.col <- rep(temp, length = n)
      else {
         if (!all(is.color(vertex.col), na.rm = TRUE))
         vertex.col <- as.color(vertex.col)
      }
   }
   else vertex.col <- rep(vertex.col, length = n)
   if (is.character(vertex.lty) && (length(vertex.lty) == 1)) {
      temp <- vertex.lty
      vertex.lty <- rep(get.vertex.attribute(x, vertex.lty),
      length = n)
      if (all(is.na(vertex.lty)))
      stop("Attribute", temp, "had illegal missing values or was not present in plot.graph.default.")
   }
   else vertex.lty <- rep(vertex.lty, length = n)
   if (is.character(vertex.rot) && (length(vertex.rot) == 1)) {
      temp <- vertex.rot
      vertex.rot <- rep(get.vertex.attribute(x, vertex.rot),
      length = n)
      if (all(is.na(vertex.rot)))
      stop("Attribute", temp, "had illegal missing values or was not present in plot.graph.default.")
   }
   else vertex.rot <- rep(vertex.rot, length = n)
   if (is.character(loop.cex) && (length(loop.cex) == 1)) {
      temp <- loop.cex
      loop.cex <- rep(get.vertex.attribute(x, loop.cex), length = n)
      if (all(is.na(loop.cex)))
      stop("Attribute", temp, "had illegal missing values or was not present in plot.graph.default.")
   }
   else loop.cex <- rep(loop.cex, length = n)
   if (is.character(label.col) && (length(label.col) == 1)) {
      temp <- label.col
      label.col <- rep(get.vertex.attribute(x, label.col),
      length = n)
      if (all(is.na(label.col)))
      label.col <- rep(temp, length = n)
      else {
         if (!all(is.color(label.col), na.rm = TRUE))
         label.col <- as.color(label.col)
      }
   }
   else label.col <- rep(label.col, length = n)
   if (is.character(label.border) && (length(label.border) ==
   1)) {
      temp <- label.border
      label.border <- rep(get.vertex.attribute(x, label.border),
      length = n)
      if (all(is.na(label.border)))
      label.border <- rep(temp, length = n)
      else {
         if (!all(is.color(label.border), na.rm = TRUE))
         label.border <- as.color(label.border)
      }
   }
   else label.border <- rep(label.border, length = n)
   if (is.character(label.bg) && (length(label.bg) == 1)) {
      temp <- label.bg
      label.bg <- rep(get.vertex.attribute(x, label.bg), length = n)
      if (all(is.na(label.bg)))
      label.bg <- rep(temp, length = n)
      else {
         if (!all(is.color(label.bg), na.rm = TRUE))
         label.bg <- as.color(label.bg)
      }
   }
   else label.bg <- rep(label.bg, length = n)
   if (!vertices.last)
   network.vertex(cx[use], cy[use], radius = vertex.radius[use],
   sides = vertex.sides[use], col = vertex.col[use],
   border = vertex.border[use], lty = vertex.lty[use],
   rot = vertex.rot[use])
   px0 <- vector()
   py0 <- vector()
   px1 <- vector()
   py1 <- vector()
   e.lwd <- vector()
   e.curv <- vector()
   e.type <- vector()
   e.col <- vector()
   e.hoff <- vector()
   e.toff <- vector()
   e.diag <- vector()
   e.rad <- vector()
   if (NROW(d) > 0) {
      if (length(dim(edge.col)) == 2)
      edge.col <- edge.col[d[, 1:2]]
      else if (is.character(edge.col) && (length(edge.col) ==
      1)) {
         temp <- edge.col
         edge.col <- x %e% edge.col
         if (!is.null(edge.col)) {
            edge.col <- edge.col[edgetouse]
            if (!all(is.color(edge.col), na.rm = TRUE))
            edge.col <- as.color(edge.col)
         }
         else edge.col <- rep(temp, length = NROW(d))
      }
      else edge.col <- rep(edge.col, length = NROW(d))
      if (length(dim(edge.lty)) == 2)
      edge.lty <- edge.lty[d[, 1:2]]
      else if (is.character(edge.lty) && (length(edge.lty) ==
      1)) {
         edge.lty <- (x %e% edge.lty)[edgetouse]
         if (all(is.na(edge.lty)))
         stop("Attribute", temp, "had illegal missing values or was not present in plot.graph.default.")
      }
      else edge.lty <- rep(edge.lty, length = NROW(d))
      if (length(dim(edge.lwd)) == 2) {
         edge.lwd <- edge.lwd[d[, 1:2]]
         e.lwd.as.mult <- FALSE
      }
      else if (is.character(edge.lwd) && (length(edge.lwd) ==
      1)) {
         edge.lwd <- (x %e% edge.lwd)[edgetouse]
         if (all(is.na(edge.lwd)))
         stop("Attribute", temp, "had illegal missing values or was not present in plot.graph.default.")
         e.lwd.as.mult <- FALSE
      }
      else {
         if (length(edge.lwd) == 1)
         e.lwd.as.mult <- TRUE
         else e.lwd.as.mult <- FALSE
         edge.lwd <- rep(edge.lwd, length = NROW(d))
      }
      if (!is.null(edge.curve)) {
         if (length(dim(edge.curve)) == 2) {
            edge.curve <- edge.curve[d[, 1:2]]
            e.curv.as.mult <- FALSE
         }
         else {
            if (length(edge.curve) == 1)
            e.curv.as.mult <- TRUE
            else e.curv.as.mult <- FALSE
            edge.curve <- rep(edge.curve, length = NROW(d))
         }
      }
      else if (is.character(edge.curve) && (length(edge.curve) ==
      1)) {
         edge.curve <- (x %e% edge.curve)[edgetouse]
         if (all(is.na(edge.curve)))
         stop("Attribute", temp, "had illegal missing values or was not present in plot.graph.default.")
         e.curv.as.mult <- FALSE
      }
      else {
         edge.curve <- rep(0, length = NROW(d))
         e.curv.as.mult <- FALSE
      }
      dist <- ((cx[d[, 1]] - cx[d[, 2]])^2 + (cy[d[, 1]] -
      cy[d[, 2]])^2)^0.5
      tl <- d.raw * dist
      tl.max <- max(tl)
      for (i in 1:NROW(d)) if (use[d[i, 1]] && use[d[i, 2]]) {
         px0 <- c(px0, as.double(cx[d[i, 1]]))
         py0 <- c(py0, as.double(cy[d[i, 1]]))
         px1 <- c(px1, as.double(cx[d[i, 2]]))
         py1 <- c(py1, as.double(cy[d[i, 2]]))
         e.toff <- c(e.toff, vertex.radius[d[i, 1]])
         e.hoff <- c(e.hoff, vertex.radius[d[i, 2]])
         e.col <- c(e.col, edge.col[i])
         e.type <- c(e.type, edge.lty[i])
         if (edge.lwd[i] > 0) {
            if (e.lwd.as.mult)
            e.lwd <- c(e.lwd, edge.lwd[i] * d.raw[i, 3])
            else e.lwd <- c(e.lwd, edge.lwd[i])
         }
         else e.lwd <- c(e.lwd, 1)
         e.diag <- c(e.diag, d[i, 1] == d[i, 2])
         e.rad <- c(e.rad, vertex.radius[d[i, 1]] * loop.cex[d[i,
         1]])
         if (uselen) {
            if (tl[i] > 0) {
               e.len <- dist[i] * tl.max/tl[i]
               e.curv <- c(e.curv, edge.len * sqrt((e.len/2)^2 -
               (dist[i]/2)^2))
            }
            else {
               e.curv <- c(e.curv, 0)
            }
         }
         else {
            if (e.curv.as.mult)
            e.curv <- c(e.curv, edge.curve[i] * d.raw[i])
            else e.curv <- c(e.curv, edge.curve[i])
         }
      }
   }
   if (diag && (length(px0) > 0) && sum(e.diag > 0)) {
      network.loop(as.vector(px0)[e.diag], as.vector(py0)[e.diag],
      length = 1.5 * baserad * arrowhead.cex, angle = 25,
      width = e.lwd[e.diag] * baserad/10, col = e.col[e.diag],
      border = e.col[e.diag], lty = e.type[e.diag], offset = e.hoff[e.diag],
      edge.steps = loop.steps, radius = e.rad[e.diag],
      arrowhead = usearrows, xctr = mean(cx[use]), yctr = mean(cy[use]))
   }
   if (length(px0) > 0) {
      px0 <- px0[!e.diag]
      py0 <- py0[!e.diag]
      px1 <- px1[!e.diag]
      py1 <- py1[!e.diag]
      e.curv <- e.curv[!e.diag]
      e.lwd <- e.lwd[!e.diag]
      e.type <- e.type[!e.diag]
      e.col <- e.col[!e.diag]
      e.hoff <- e.hoff[!e.diag]
      e.toff <- e.toff[!e.diag]
      e.rad <- e.rad[!e.diag]
   }
   if (!usecurve & !uselen) {
      if (length(px0) > 0)
      network.arrow(as.vector(px0), as.vector(py0), as.vector(px1),
      as.vector(py1), length = 2 * baserad * arrowhead.cex,
      angle = 20, col = e.col, border = e.col, lty = e.type,
      width = e.lwd * baserad/10, offset.head = e.hoff,
      offset.tail = e.toff, arrowhead = usearrows)
   }
   else {
      if (length(px0) > 0) {
         network.arrow(as.vector(px0), as.vector(py0), as.vector(px1),
         as.vector(py1), length = 2 * baserad * arrowhead.cex,
         angle = 20, col = e.col, border = e.col, lty = e.type,
         width = e.lwd * baserad/10, offset.head = e.hoff,
         offset.tail = e.toff, arrowhead = usearrows,
         curve = e.curv, edge.steps = edge.steps)
      }
   }
   if (vertices.last)
   network.vertex(cx[use], cy[use], radius = vertex.radius[use],
   sides = vertex.sides[use], col = vertex.col[use],
   border = vertex.border[use], lty = vertex.lty[use],
   rot = vertex.rot[use])
   if (displaylabels & (!all(label == "")) & (!all(use == FALSE))) {
      if (label.pos == 0) {
         xhat <- yhat <- rhat <- rep(0, n)
         xoff <- cx[use] - mean(cx[use])
         yoff <- cy[use] - mean(cy[use])
         roff <- sqrt(xoff^2 + yoff^2)
         for (i in (1:n)[use]) {
            ij <- unique(c(d[d[, 2] == i & d[, 1] != i, 1],
            d[d[, 1] == i & d[, 2] != i, 2]))
            ij.n <- length(ij)
            if (ij.n > 0) {
               for (j in ij) {
                  dx <- cx[i] - cx[j]
                  dy <- cy[i] - cy[j]
                  dr <- sqrt(dx^2 + dy^2)
                  xhat[i] <- xhat[i] + dx/dr
                  yhat[i] <- yhat[i] + dy/dr
               }
               xhat[i] <- xhat[i]/ij.n
               yhat[i] <- yhat[i]/ij.n
               rhat[i] <- sqrt(xhat[i]^2 + yhat[i]^2)
               if (rhat[i] != 0) {
                  xhat[i] <- xhat[i]/rhat[i]
                  yhat[i] <- yhat[i]/rhat[i]
               }
               else {
                  xhat[i] <- xoff[i]/roff[i]
                  yhat[i] <- yoff[i]/roff[i]
               }
            }
            else {
               xhat[i] <- xoff[i]/roff[i]
               yhat[i] <- yoff[i]/roff[i]
            }
            if (xhat[i] == 0)
            xhat[i] <- 0.01
            if (yhat[i] == 0)
            yhat[i] <- 0.01
         }
         xhat <- xhat[use]
         yhat <- yhat[use]
      }
      else if (label.pos < 5) {
         xhat <- switch(label.pos, 0, -1, 0, 1)
         yhat <- switch(label.pos, -1, 0, 1, 0)
      }
      else if (label.pos == 6) {
         xoff <- cx[use] - mean(cx[use])
         yoff <- cy[use] - mean(cy[use])
         roff <- sqrt(xoff^2 + yoff^2)
         xhat <- xoff/roff
         yhat <- yoff/roff
      }
      else {
         xhat <- 0
         yhat <- 0
      }
      os <- par()$cxy * label.cex
      lw <- strwidth(label[use], cex = label.cex)/2
      lh <- strheight(label[use], cex = label.cex)/2
      if (boxed.labels) {
         rect(cx[use] + xhat * vertex.radius[use] - (lh *
         label.pad + lw) * ((xhat < 0) * 2 + (xhat ==
         0) * 1), cy[use] + yhat * vertex.radius[use] -
         (lh * label.pad + lh) * ((yhat < 0) * 2 + (yhat ==
         0) * 1), cx[use] + xhat * vertex.radius[use] +
         (lh * label.pad + lw) * ((xhat > 0) * 2 + (xhat ==
         0) * 1), cy[use] + yhat * vertex.radius[use] +
         (lh * label.pad + lh) * ((yhat > 0) * 2 + (yhat ==
         0) * 1), col = label.bg, border = label.border,
         lty = label.lty, lwd = label.lwd)
      }
      text(cx[use] + xhat * vertex.radius[use] + (lh * label.pad +
      lw) * ((xhat > 0) - (xhat < 0)), cy[use] + yhat *
      vertex.radius[use] + (lh * label.pad + lh) * ((yhat >
      0) - (yhat < 0)), label[use], cex = label.cex, col = label.col,
      offset = 0)
   }
   if (interactive && ((length(cx) > 0) && (!all(use == FALSE)))) {
      os <- c(0.2, 0.4) * par()$cxy
      textloc <- c(min(cx[use]) - pad, max(cy[use]) + pad)
      tm <- "Select a vertex to move, or click \"Finished\" to end."
      tmh <- strheight(tm)
      tmw <- strwidth(tm)
      text(textloc[1], textloc[2], tm, adj = c(0, 0.5))
      fm <- "Finished"
      finx <- c(textloc[1], textloc[1] + strwidth(fm))
      finy <- c(textloc[2] - 3 * tmh - strheight(fm)/2, textloc[2] -
      3 * tmh + strheight(fm)/2)
      finbx <- finx + c(-os[1], os[1])
      finby <- finy + c(-os[2], os[2])
      rect(finbx[1], finby[1], finbx[2], finby[2], col = "white")
      text(finx[1], mean(finy), fm, adj = c(0, 0.5))
      clickpos <- unlist(locator(1))
      if ((clickpos[1] %iin% finbx) && (clickpos[2] %iin% finby)) {
         cl <- match.call()
         cl$interactive <- FALSE
         cl$coord <- cbind(cx, cy)
         cl$x <- x
         return(eval.parent(cl))
      }
      else {
         clickdis <- sqrt((clickpos[1] - cx[use])^2 + (clickpos[2] -
         cy[use])^2)
         selvert <- match(min(clickdis), clickdis)
         if (all(label == ""))
         label <- 1:n
         rect(textloc[1], textloc[2] - tmh/2, textloc[1] +
         tmw, textloc[2] + tmh/2, border = "white", col = "white")
         tm <- "Where should I move this vertex?"
         tmh <- strheight(tm)
         tmw <- strwidth(tm)
         text(textloc[1], textloc[2], tm, adj = c(0, 0.5))
         fm <- paste("Vertex", label[use][selvert], "selected")
         finx <- c(textloc[1], textloc[1] + strwidth(fm))
         finy <- c(textloc[2] - 3 * tmh - strheight(fm)/2,
         textloc[2] - 3 * tmh + strheight(fm)/2)
         finbx <- finx + c(-os[1], os[1])
         finby <- finy + c(-os[2], os[2])
         rect(finbx[1], finby[1], finbx[2], finby[2], col = "white")
         text(finx[1], mean(finy), fm, adj = c(0, 0.5))
         clickpos <- unlist(locator(1))
         cx[use][selvert] <- clickpos[1]
         cy[use][selvert] <- clickpos[2]
         cl <- match.call()
         cl$coord <- cbind(cx, cy)
         cl$x <- x
         return(eval.parent(cl))
      }
   }
   invisible(cbind(cx, cy))
}
<environment: namespace:network>