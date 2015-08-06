\name{startend}
\alias{startend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{extract the segment of the analyzed signal and the corresponding peak position and width}
\description{
	extract the segment of the analyzed signal and the corresponding peak position and width
}
\usage{
startend(x,y,width,startpoints,endpoints)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The values of x-axis}
  \item{y}{The values of y-axis}
  \item{width}{estimate the peak width using the CWT with Haar wavelet}
  \item{startpoints}{The startingpoints and endpoints mean the starting point and the end point of the selected segment.}
  \item{endpoints}{The startingpoints and endpoints mean the starting point and the end point of the selected segment.}
}

\value{
  the fitted vector
}

\author{Zhimin Zhang, Xia Tong}

\seealso{\code{\link{peakdetection}}}

\keyword{startend}


\examples{
	y=c(dnorm(seq(-5,5,by=0.1),sd=0.4),dnorm(seq(-5,5,by=0.1),sd=0.7),
		dnorm(seq(-5,5,by=0.1),sd=0.2),dnorm(seq(-5,5,by=0.1),sd=0.3),
		dnorm(seq(-5,5,by=0.1),sd=0.5));
	y=y+8*rnorm(length(y))*0.001;
	x=1:length(y);
	plot(x,y,type='l', cex.axis=1,cex.lab=1.1,main='Simulated data',
		xlab='Data points',ylab='Height')
	scales <- seq(1,16,1)
	wCoefs <- cwt(y, scales=scales, wavelet='mexh')
	localMax <- getLocalMaximumCWT(wCoefs)
	ridgeList <- getRidge(localMax)
	SNR.Th <- 1
	majorPeakInfo <- identifyMajorPeaks(y, ridgeList, wCoefs, 
		SNR.Th=SNR.Th, ridgeLength =4)
	peakIndex <- majorPeakInfo$peakIndex
	posi <- matrix(x[peakIndex])
	points(x[peakIndex],y[peakIndex],pch=15)
	position <- x[posi];
	s <- 200;
	e <- 400;
	rs_start = which.min(abs(x-s));
	rs_end   = which.min(abs(x-e));
	peaks=posi[posi > rs_start & posi < rs_end]-rs_start+1;
	xa=matrix(x[rs_start:rs_end]);
	ya=matrix(y[rs_start:rs_end]);
	positiona <- matrix(xa[peaks]);
	plot(xa,ya,type='l', cex.axis=1,cex.lab=1.1,main='Simulated data',
		xlab='Data points',ylab='Height')
	points(xa[peaks],ya[peaks],pch=15)

}
