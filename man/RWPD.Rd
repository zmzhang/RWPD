\name{RWPD}
\alias{RWPD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{find the undetected peaks which is not detected in the first iteration, and estimate the peak width based on the results in the first iteration}
\description{
	find the undetected peaks which is not detected in the first iteration, and estimate the peak width based on the results in the first iteration
}
\usage{
RWPD(x,yr,yf,width,position)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The values of x-axis}
  \item{yr}{The yr denotes the results of baseline_remove or the airPls.}
  \item{yf}{The yf is the collection of each fitting peaks.}
  \item{width}{the fitting width of the first iteration}
  \item{position}{the fitting position of the first iteration}
}

\value{
  the fitted vector
}

\author{Zhimin Zhang, Xia Tong}

\seealso{\code{\link{peakdetectionresidual}}}

\keyword{RWPD}
\examples{

	data(simulatedxa)
	data(simulatedyr)
	data(simulatedyf)
	data(simulatedposition)
	data(simulatedwidth)
	plot(xa,yr,type='l',cex.axis=1.2,cex.lab=1.2,main='Simulated data', 
		xlab='Data points',ylab='Height')
	residual <- yr-colSums(yf);
	positionresidual <- peakdetectionresidual(xa,residual)$position;
	p <- matrix(which(xa %in% positionresidual));
	ymax <- matrix(0,nrow(p),ncol(p));
	for (i in 1:length(p)) {
     		lowerbound <- xa[p[i]-5];
     		upperbound <- xa[p[i]+5];
		xrange <- which(xa >= lowerbound & xa <= upperbound);
		ymax[i] <- max(yr[xrange]);
		pos <- which(yr %in% ymax);
		positionresidualmax <- matrix(xa[pos]);
	}
	posr <- which(abs(positionresidual-positionresidualmax)<10);
	positionr <- matrix(positionresidual[posr]);
	allposition <- sort(rbind(matrix(position),positionr));
	position <- matrix(allposition)[-which(diff(allposition)<10),];
	position <- matrix(position);
	width <- matrix(c(sum(width)/length(position)),nrow=length(position));
}