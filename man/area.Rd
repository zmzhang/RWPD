\name{areayf}
\alias{areayf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{the shape and area of the fitting peaks}
\description{
 To compute the peak shape and area of each fitting peaks.
}
\usage{
areayf(fitresults,x,peakheight)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitresults}{The fitresults are the fitting results of GA. }
  \item{x}{The values of x-axis}
  \item{peakheight}{The peakheight is the results of the PEAKHEIGHTS function.}
}


\value{
  the fitted vector
}

\author{Zhimin Zhang, Xia Tong}

\seealso{\code{\link{peakshape, PEAKHEIGHTS, integration}}}

\keyword{areayf}

\example{
	y=c(dnorm(seq(-5,5,by=0.1),sd=0.2))
	x=1:length(y)
	peakheight=c(2)
	fitresults=matrix(c(51,5),nrow=1)
	plot(x,y,type='l')
	NumPeaks <- length(peakheight);
	g <- matrix(0,NumPeaks,length(x))
	yf <- matrix(0,NumPeaks,length(x))
	area <- matrix(0,1,NumPeaks)
	peakshape <- function(x,position,width){
		exp(-((x-position)/(0.6005612*width))^2);
		# Gaussian function
	}
	for(m in 1:NumPeaks){
		g[m,] <- peakshape(x,fitresults[2*m-1],fitresults[2*m]);
		yf[m,] <- g[m,]*peakheight[m];
		area[m] <- trapezoidal.integration(x,yf[m,]);
	}
}
