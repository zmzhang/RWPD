\name{peakdetection}
\alias{peakdetection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{detect the peak position using the CWT with Mexican Hat wavelet}
\description{
detect the peak position using the CWT with Mexican Hat wavelet
}
\usage{
peakdetection(x,y,sca)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The values of x-axis}
  \item{y}{The values of y-axis}
}

\value{
  the fitted vector
}

\author{Zhimin Zhang, Xia Tong}

\seealso{\code{\link{cwt}}}

\keyword{peakdetection}

\examples{
	y=c(dnorm(seq(-5,5,by=0.1),sd=0.4),dnorm(seq(-5,5,by=0.1),sd=0.7),
		dnorm(seq(-5,5,by=0.1),sd=0.2),dnorm(seq(-5,5,by=0.1),sd=0.3),
		dnorm(seq(-5,5,by=0.1),sd=0.5));
	y=y+8*rnorm(length(y))*0.001;
	x=1:length(y);
	plot(x,y,type='l', cex.axis=1,cex.lab=1.1,main='Simulated data', xlab='Data points',ylab='Height')
	library(MassSpecWavelet)
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
}