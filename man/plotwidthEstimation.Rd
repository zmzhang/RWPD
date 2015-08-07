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

\author{Zhimin Zhang, Xia Tong}

\seealso{\code{\link{widthEstimationCWT, cwt, extendNBase, extendLength}}}

\keyword{plotwidthEstimation}

\examples{
	y=c(dnorm(seq(-5,5,by=0.1),sd=0.4),dnorm(seq(-5,5,by=0.1),sd=0.7),
		dnorm(seq(-5,5,by=0.1),sd=0.2),dnorm(seq(-5,5,by=0.1),sd=0.3),
		dnorm(seq(-5,5,by=0.1),sd=0.5));
	y=y+8*rnorm(length(y))*0.001;
	x=1:length(y);
	plot(x,y,type='l',ylim=c(-2,2), cex.axis=1,cex.lab=1.1,main='Simulated data', xlab='Data points',ylab='Height')
	
	position <- peakdetection(x,y,16)$position;
	majorPeakInfo <- peakdetection(x,y,16)$majorPeakInfo;
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
 
}
