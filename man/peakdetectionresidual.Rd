\name{peakdetectionresidual}
\alias{peakdetectionresidual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{detect the peak position in the residual signal}
\description{
	detect the peak position in the residual signal
}
\usage{
peakdetectionresidual(x,y)
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

\keyword{peakdetectionresidual}

\examples{
	data(simulatedxa)
	data(simulatedyf)
	data(simulatedyr)
	library(MassSpecWavelet)
	residual <- yr-colSums(yf);
	scales <- seq(1,8,1)
	wCoefs <- cwt(residual, scales=scales, wavelet='mexh')
	localMax <- getLocalMaximumCWT(wCoefs)
	ridgeList <- getRidge(localMax)
	SNR.Th <- 1.5;
	majorPeakInfo <- identifyMajorPeaks(residual, ridgeList, wCoefs, 
		SNR.Th=SNR.Th, ridgeLength =4)
	peakIndex <- majorPeakInfo$peakIndex
	position <- matrix(xa[peakIndex])
	plot(xa,residual,type='l')
	points(xa[peakIndex],residual[peakIndex],pch=15)
}