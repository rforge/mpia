write.S.ST <- function(tdm, fpath = "SVD.txt", sep = "\n") {
	#print(paste(Sys.time(),"Preparing data"))
	header <- paste(nrow(tdm),ncol(tdm),length(tdm@x))
	#print(paste(Sys.time(),"Header:",header))
	groups <- diff(tdm@p)
	groups <- rep(seq(groups),groups)
	i <- split(tdm@i,groups)
	names(i) <- NULL
	gc()
	x <- split(tdm@x,groups)
	names(x) <- NULL
	rm(tdm)
	rm(groups)
	gc()
	
	con <- file(fpath,open="wt")
	options(scipen=99)
	options(digits=4)
	cat(header,file=con,sep=sep)
	#print(paste(Sys.time(),"Printing content to file"))
	for (idx in seq(i)) {
		cat(c(length(i[[idx]]),paste(i[[idx]],x[[idx]])),file=con,sep=sep)
	}
	close(con)
}

write.S.SB <- function(tdm, fpath = "SVD.bin") {
	#print(paste(Sys.time(),"Preparing data"))
	header <- c(tdm@Dim[1],tdm@Dim[2],length(tdm@x))
	#print(paste(Sys.time(),"Header:",paste(header,collapse=" ")))
	groups <- diff(tdm@p)
	groups <- rep(seq(groups),groups)
	i <- split(tdm@i,groups)
	names(i) <- NULL
	gc()
	x <- split(tdm@x,groups)
	names(x) <- NULL
	rm(tdm)
	rm(groups)
	gc()
	
	con <- file(fpath,open="wb")
	on.exit(close(con))
	options(scipen=99)
	options(digits=4)
	writeBin(header[1], con, size = 4, endian = "big")
	writeBin(header[2], con, size = 4, endian = "big")
	writeBin(header[3], con, size = 4, endian = "big")
	#print(paste(Sys.time(),"Printing content to file"))
	for (idx in seq(i)) {
		di <- as.integer(i[[idx]])
		dx <- as.double(x[[idx]])
		writeBin(length(di), con, size = 4, endian = "big")		
		for(idx2 in seq(di)) {
			writeBin(di[idx2], con, size = 4, endian = "big")
			writeBin(dx[idx2], con, size = 4, endian = "big")
		}
	}
}

read.DT <- function(file, encoding = "unknown") {
    if (is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("Argument 'file' must be a character string or connection.")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    ## read first non-blank line
    dim <- scan(file, what = NA_integer_, nlines = 1L,
        quiet = TRUE, encoding = encoding)
    if (length(data) > 2)
        stop("invalid declaration")
    ## read remaining non-blank lines
    data <- scan(file, what = NA_real_, quiet = TRUE,
        encoding = encoding)
    ## Vector
    if (length(dim) == 1) {
        if (length(data) != dim)
            stop("data does not match declaration")
        return(data)
    }
    ## Matrix
    if (length(data) != prod(dim))
        stop("data does not match declaration")
    matrix(data, nrow = dim[1], byrow = TRUE)
}

writeToFile <- function (x, tmpdir = tempdir(), inputformat = "st") {
	if(inputformat == "st") {
		file <- file.path(tmpdir, "SVD.txt")
		write.S.ST(x, fpath = file)
	} else if (inputformat == "sb") {
		file <- file.path(tmpdir, "SVD.bin")
		write.S.SB(x, fpath = file)
	} else {
		stop(paste("unknown input format",inputformat))
	}
	#print(paste(Sys.time(),"Done writing to file"))
	return(TRUE)
}

svd.call <- function(ndim, verbose = 1, tmpdir = tempdir(), inputformat = "st", getRHS = TRUE) {
	if (inputformat == "st") {
		fpath <- file.path(tmpdir, "SVD.txt")
		header <- as.integer(unlist(strsplit(readLines(fpath,n=1)," ")))
		if(header[2]>1.2*header[1])
			transpose=TRUE
		else
			transpose=FALSE
	} else if (inputformat == "sb") {
		fpath <- file.path(tmpdir, "SVD.bin")
		transpose = FALSE
	}
	
	if (transpose) {
		command <- paste("/data/web/lib/svd/svd", "-d", ndim, "-r", inputformat, "-v ", verbose, "-t -o", file.path(tmpdir, ""), fpath)
	} else {
		command <- paste("/data/web/lib/svd/svd", "-d", ndim, "-r", inputformat, "-v", verbose, "-o", file.path(tmpdir, ""), fpath)
	}
	#print(paste(Sys.time(),"Executing:",command))
	system(command)
	#print(paste(Sys.time(),"Reading SVD files"))
	
	SVD <- NULL
	SVD$d <- read.DT(file = file.path(tmpdir, "-S"))
	if(transpose) {
		SVD$u <- t(read.DT(file = file.path(tmpdir, "-Vt")))
		if(getRHS) SVD$v = t(read.DT(file = file.path(tmpdir, "-Ut")))
	} else {
		SVD$u <- t(read.DT(file = file.path(tmpdir, "-Ut")))
		if(getRHS) SVD$v = t(read.DT(file = file.path(tmpdir, "-Vt")))
	}
	unlink(file.path(tmpdir, c("SVD.*","-S", "-Ut", "-Vt")))
	return(SVD)
}

lsa_sparse <- function(tdm, ndim, verbose = 0, tmpdir = tempdir(), inputformat = "st", getRHS = TRUE) {
	#print(paste(Sys.time(),"Writing sparse matrix to file"))
	writeToFile(tdm, tmpdir, inputformat) # tdm@Data
	
	#free memory (potential bottleneck)
	terms <- rownames(tdm) # @Data
	docs <- colnames(tdm) # @Data
	#control <- tdm@Control
   
   rm(tdm)
	gc()
	
	#print(paste(Sys.time(),"Performing SVD using svdlib.c"))
	SVD <- svd.call(ndim, verbose, tmpdir, inputformat, getRHS)
	
	#print(paste(Sys.time(),"Building space"))
	require(lsa)
	space = NULL
	space$sk = SVD$d
	space$tk = SVD$u
	rownames(space$tk) = terms
	if(getRHS) {
		space$dk = SVD$v
		rownames(space$dk) = docs
	}
	class(space) = "LSAspace"
	
	#space$control <- control
	#space$control$dictionary <- terms
	
	return(space)
}
