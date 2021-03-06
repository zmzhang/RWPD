\name{areayf}
\alias{areayf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{the shape and area of the fitting peaks}
\description{
 To compute the peak shape and area of each fitting peaks.
}
\usage{
peakarea(fitresults,x,peakheight)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitresults}{The fitresults are the fitting results of GA. }
  \item{x}{The values of x-axis}
  \item{peakheight}{The peakheight is the results of the peakheights function.}
}


\value{
  the fitted vector
}

\author{Zhimin Zhang, Xia Tong}

\seealso{\code{\link{peakshape, peakheights, integration}}}

\keyword{peakarea}

\examples{
	y=c(dnorm(seq(-5,5,by=0.1),sd=0.2))
	x=1:length(y)
	peakheight=c(2)
	fitresults=matrix(c(51,5),nrow=1)
	plot(x,y,type='l')
	NumPeaks <- length(peakheight);
	g <- matrix(0,NumPeaks,length(x))
	yf <- matrix(0,NumPeaks,length(x))
	area <- matrix(0,1,NumPeaks)
	for(m in 1:NumPeaks){
		g[m,] <- peakshape(x,fitresults[2*m-1],fitresults[2*m],1);
		yf[m,] <- g[m,]*peakheight[m];
		area[m] <- integration(x,yf[m,]);
		lines(x,yf[m,],col='red')
	}
}
