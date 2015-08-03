\name{plotwidthEstimation}
\alias{plotwidthEstimation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot the estimation of peak width based on the CWT}
\description{
  Plot the the estimation of peak returned by \code{\link{widthEstimationCWT}}
}
\usage{
plotwidthEstimation(x,peakWidth)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the values of x-axis}
  \item{peakWidth}{ returned by \code{\link{widthEstimationCWT}} }
}

\value{
Whether the drawing is successful or not.
}

\author{\Zhimin Zhang, Xia Tong}

\seealso{\code{\link{widthEstimationCWT, cwt, extendNBase, extendLength}}}

\keyword{plotwidthEstimation}

\examples
	y=c(dnorm(seq(-5,5,by=0.1),sd=0.4),dnorm(seq(-5,5,by=0.1),sd=0.7),
		dnorm(seq(-5,5,by=0.1),sd=0.2),dnorm(seq(-5,5,by=0.1),sd=0.3),
		dnorm(seq(-5,5,by=0.1),sd=0.5));
	y=y+8*rnorm(length(y))*0.001;
	x=1:length(y);
	plot(x,y,type='l',ylim=c(-2,2), cex.axis=1,cex.lab=1.1,main='Simulated data', xlab='Data points',ylab='Height')
	
	position <- peakdetection(x,y)$position;
	majorPeakInfo <- peakdetection(x,y)$majorPeakInfo;
	peakWidth <- widthEstimationCWT(y,majorPeakInfo)
	widthpoints <- plotwidthEstimation(x,y,peakWidth)

	wCoefs_haar <- cwt(y, 1:max(majorPeakInfo$peakScale), wavelet='haar')
	peakScale <- majorPeakInfo$peakScale[findInterval(majorPeakInfo$peakIndex,majorPeakInfo$allPeakIndex)]
	peakIndex <- majorPeakInfo$peakIndex

	peakScale.1.3=3*peakScale[1]
	x1= (peakIndex[1]-peakScale.1.3):(peakIndex[1]+peakScale.1.3)
	y1=  wCoefs_haar[x1,peakScale[1]]
	y1a=  wCoefs_haar[,peakScale[1]]
	lines(x1,y1-1,lwd=c(1.5))
	peakWidth.1 = peakWidth[[paste(peakIndex[1])]]
	LR1=c(peakWidth.1[c(1,length(peakWidth.1))])
	points(LR1,y1a[LR1]-1,col='blue',pch=20)
	text(mean(x1)-28,mean(y1)-5,paste("Scale=",peakScale[1]))

	peakScale.2.3=3*peakScale[2]
	x2= (peakIndex[2]-peakScale.2.3):(peakIndex[2]+peakScale.2.3)
	y2=  (wCoefs_haar[x2,peakScale[2]])
	y2a= ( wCoefs_haar[,peakScale[2]])
	lines(x2,y2-1,lwd=c(1.5))  
	peakWidth.2 = peakWidth[[paste(peakIndex[2])]]
	LR2=c(peakWidth.2[c(1,length(peakWidth.2))])
	points(LR2,y2a[LR2]-1,col='blue',pch=20)
	text(mean(x2)-28,mean(y1)-5,paste("Scale=",peakScale[2]))

	peakScale.3.3=3*peakScale[3]
	x3= (peakIndex[3]-peakScale.3.3):(peakIndex[3]+peakScale.3.3)
	y3= (wCoefs_haar[x3,peakScale[3]])
	y3a= ( wCoefs_haar[,peakScale[3]])
	lines(x3,y3-1,lwd=c(1.5))  
	peakWidth.3 = peakWidth[[paste(peakIndex[3])]]
	LR3=c(peakWidth.3[c(1,length(peakWidth.3))])
	points(LR3,y3a[LR3]-1,col='blue',pch=20)
	text(mean(x3)-28,mean(y1)-5,paste("Scale=",peakScale[3]))

	peakScale.4.3=3*peakScale[4]
	x4= (peakIndex[4]-peakScale.4.3):(peakIndex[4]+peakScale.4.3)
	y4= (wCoefs_haar[x4,peakScale[4]])
	y4a= ( wCoefs_haar[,peakScale[4]])
	lines(x4,y4-1,lwd=c(1.5))  
	peakWidth.4 = peakWidth[[paste(peakIndex[4])]]
	LR4=c(peakWidth.4[c(1,length(peakWidth.4))])
	points(LR4,y4a[LR4]-1,col='blue',pch=20)
	text(mean(x4)-28,mean(y1)-5,paste("Scale=",peakScale[4]))

	peakScale.5.3=3*peakScale[5]
	x5= (peakIndex[5]-peakScale.5.3):(peakIndex[5]+peakScale.5.3)
	y5= (wCoefs_haar[x5,peakScale[5]])
	y5a= ( wCoefs_haar[,peakScale[5]])
	lines(x5,y5-1,lwd=c(1.5))  
	peakWidth.5 = peakWidth[[paste(peakIndex[5])]]
	LR5=c(peakWidth.5[c(1,length(peakWidth.5))])
	points(LR5,y5a[LR5]-1,col='blue',pch=20)
	text(mean(x5)-28,mean(y1)-5,paste("Scale=",peakScale[5]))
 
peakdetection <- function(x,y){
	# library(MassSpecWavelet)
	scales <- seq(1,16,1)
	wCoefs <- cwt(y, scales=scales, wavelet='mexh')
	localMax <- getLocalMaximumCWT(wCoefs)
	ridgeList <- getRidge(localMax)
	SNR.Th <- 1
	majorPeakInfo <- identifyMajorPeaks(y, ridgeList, wCoefs, 
		SNR.Th=SNR.Th, ridgeLength =4)
	peakIndex <- majorPeakInfo$peakIndex
	position <- matrix(x[peakIndex])
	points(x[peakIndex],y[peakIndex],pch=15)
	output <- list(position=position,majorPeakInfo=majorPeakInfo,
		peakIndex=peakIndex)
	return(output)
}

extendNBase <- function(x, nLevel=1, base=2, ...) {
	if (!is.matrix(x)) x <- matrix(x, ncol=1)	
	nR <- nrow(x)
	if (is.null(nLevel)) {
		nR1 <- nextn(nR, base)		
	} else {
		nR1 <- ceiling(nR / base^nLevel) * base^nLevel		
	}
	if (nR != nR1) {
		x <- extendLength(x, addLength=nR1-nR, ...)
	}
	return(x)
}

extendLength <- function(x, addLength=NULL, method=c('reflection', 'open', 'circular'), direction=c('right', 'left', 'both')) {
	if (is.null(addLength)) stop('Please provide the length to be added!')
	if (!is.matrix(x)) x <- matrix(x, ncol=1)	
	method <- match.arg(method)
	direction <- match.arg(direction)
	
	nR <- nrow(x)
	nR1 <- nR + addLength
	if (direction == 'both') {
		left <- right <- addLength
	} else if (direction == 'right') {
		left <- 0
		right <- addLength
	} else if (direction == 'left') {
		left <- addLength
		right <- 0
	}
	
	if (right > 0) {
		x <- switch(method,
			reflection =rbind(x, x[nR:(2 * nR - nR1 + 1), , drop=FALSE]),
			open = rbind(x, matrix(rep(x[nR,], addLength), ncol=ncol(x), byrow=TRUE)),
			circular = rbind(x, x[1:(nR1 - nR),, drop=FALSE]))
	}

	if (left > 0) {
		x <- switch(method,
			reflection =rbind(x[addLength:1, , drop=FALSE], x),
			open = rbind(matrix(rep(x[1,], addLength), ncol=ncol(x), byrow=TRUE), x),
			circular = rbind(x[(2 * nR - nR1 + 1):nR,, drop=FALSE], x))
	}
	if (ncol(x) == 1)  x <- as.vector(x)
	
	return(x)
}

cwt <- function(ms, scales=1, wavelet='mexh') {
	## Check for the wavelet format
	if (wavelet == 'mexh') {
		psi_xval <- seq(-8, 8, length=1024)
		psi <- (2/sqrt(3) * pi^(-0.25)) * (1 - psi_xval^2) *exp(-psi_xval^2/2)
		#plot(psi_xval, psi)
	} else if (wavelet=='haar') {
    	psi_xval <- seq(0,1,length=1024)
    	psi <- c(0,rep(1,511),rep(-1,511),0)
	}else if (is.matrix(wavelet)) {
		if (nrow(wavelet) == 2) {
			psi_xval <- wavelet[1,]
			psi <- wavelet[2,]
		} else if (ncol(wavelet) == 2) {
			psi_xval <- wavelet[,1]
			psi <- wavelet[,2]
		} else {
			stop('Unsupported wavelet format!')
		}
	} else {
		stop('Unsupported wavelet!')
	}
		
    	oldLen <- length(ms)
	## To increase the computation effeciency of FFT, extend it as the power of 2
	## because of a strange signal length 21577 makes the FFT very slow!
	#ms <- extend.nBase(ms, nLevel=1, base=2)
	ms <- extendNBase(ms, nLevel=NULL, base=2)
	len <- length(ms)
    	nbscales <- length(scales)
    	wCoefs <- NULL

    	psi_xval <- psi_xval - psi_xval[1]
    	dxval <- psi_xval[2]
    	xmax  <- psi_xval[length(psi_xval)]
    	for (i in 1:length(scales)) {
		scale.i <- scales[i]
		f <- rep(0, len)
        j <- 1 + floor((0:(scale.i * xmax))/(scale.i * dxval))
        if (length(j) == 1)		j <- c(1, 1)
		lenWave <- length(j)
        f[1:lenWave] <- rev(psi[j]) - mean(psi[j])
		if (length(f) > len) stop(paste('scale', scale.i, 'is too large!'))
		wCoefs.i <- 1/sqrt(scale.i) * convolve(ms, f)
		## Shift the position with half wavelet width
		wCoefs.i <- c(wCoefs.i[(len-floor(lenWave/2) + 1) : len], wCoefs.i[1:(len-floor(lenWave/2))])
		wCoefs <- cbind(wCoefs, wCoefs.i)
    	}
	if (length(scales) == 1) wCoefs <- matrix(wCoefs, ncol=1)
	colnames(wCoefs) <- scales
	wCoefs <- wCoefs[1:oldLen,,drop=FALSE]
	return(wCoefs)
}

widthEstimationCWT <- function(x,majorPeakInfo) {
    	wCoefs_haar <- cwt(x, 1:max(majorPeakInfo$peakScale), wavelet='haar')
    	peakIndex <- majorPeakInfo$peakIndex
    	peakScale <- majorPeakInfo$peakScale[findInterval(majorPeakInfo$peakIndex,majorPeakInfo$allPeakIndex)]
  
    	peakWidth <- list()
    	peakWidth[["peakIndex"]]= majorPeakInfo$peakIndex
    	peakWidth[["peakIndexLower"]]= majorPeakInfo$peakIndex
    	peakWidth[["peakIndexUpper"]]= majorPeakInfo$peakIndex
  
    	for(i in 1:length(peakIndex)){
    		peakIndex.i=peakIndex[i]
    		peakScale.i=peakScale[i]
    		wCoefs_haar.i=wCoefs_haar[,peakScale.i]
    		wCoefs_haar.i.abs=abs(wCoefs_haar.i)
    		localmax=localMaximum(-wCoefs_haar.i.abs,winSize=5)      
		#localmax=localmax & abs(wCoefs_haar.i)<(mean(wCoefs_haar.i.abs[localmax==1])+0.5*sd(wCoefs_haar.i.abs[localmax==1]))
    		localmax=as.numeric(localmax)
    		localmax[peakIndex]=0
    		localmax[(peakIndex.i-peakScale.i/2+1):(peakIndex.i+peakScale.i/2-1)]=0
        
    		Lef =0
    		Rig =0
    
    		peakScale.i.3=2*peakScale.i
     
    		if(i==1){
       		maxIndexL=max(c((peakIndex.i-peakScale.i.3),1))
    		}else{
       	maxIndexL=max(c((peakIndex.i-peakScale.i.3),peakIndex[i-1]))
    		}
    
    		if(i==length(peakIndex)){
       		minIndexR=min(c((peakIndex.i+peakScale.i.3),length(localmax)))
    		} else{
       		minIndexR=min(c((peakIndex.i+peakScale.i.3),peakIndex[i+1]))
    		}
    		ignoreL=1:maxIndexL
    		ignoreR=minIndexR:length(localmax)        
    		localmax[c(ignoreL,ignoreR)]=0
    		localmax[c(peakIndex.i,(peakIndex.i-(peakScale.i/2)):(peakIndex.i+(peakScale.i/2)))]=0
    		bi=which(localmax==1)
        
    		biLeft=bi[bi<peakIndex.i]
    		useL= maxIndexL:peakIndex.i
    		minIndexLeft=useL[which(min(x[useL])==x[useL])]
    
    		if(length(biLeft)==0){
      		Lef=minIndexLeft
    		}else{
      		minbaselineIndexLeft=biLeft[which(min(x[biLeft])==x[biLeft])]
      	if(minIndexLeft>=(peakIndex.i-peakScale.i/2+1)){
        		Lef=minbaselineIndexLeft
      	}else{
        		Lef=max(c(minIndexLeft,minbaselineIndexLeft))
      	}   
    	}
    
    	biRight=bi[bi>peakIndex.i]
    	useR= peakIndex.i:minIndexR
    	minIndexRight=useR[which(min(x[useR])==x[useR])]
      
    	if(length(biRight)==0){
      	Rig= minIndexRight
    	}else{
      	minbaselineIndexRight=biRight[which(min(x[biRight])==x[biRight])] 
      if(minIndexRight<=(peakIndex.i+peakScale.i/2-1)){
        	Rig=minbaselineIndexRight
      }else{
        	Rig=min(c(minIndexRight,minbaselineIndexRight))
      }      
    	}
    	peakWidth[[paste(peakIndex.i)]]=Lef:Rig
    	peakWidth[["peakIndexLower"]][i]= Lef
    	peakWidth[["peakIndexUpper"]][i]= Rig
  	}      
  	return(peakWidth)		
}

plotwidthEstimation <- function(x,y,peakWidth) {
	y=as.vector(y)
      lmindex=1:length(y)
      peakIndex=peakWidth$peakIndex
      LR = NULL
      for (i in 1:length(peakIndex)){
           peakWidth.i=peakWidth$peakIndexLower[i]:peakWidth$peakIndexUpper[i]
           LR=c(LR,peakWidth.i[c(1,length(peakWidth.i))])
      }
	points(x[peakWidth$peakIndexUpper],y[peakWidth$peakIndexUpper],pch=16)
	points(x[peakWidth$peakIndexLower],y[peakWidth$peakIndexLower],pch=21)
      return ("successful")
}


