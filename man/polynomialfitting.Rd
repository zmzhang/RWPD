\name{baseline_remove}
\alias{baseline_remove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ To remove the baseline by the linear model}
\description{
 To remove the baseline by the linear model
}
\usage{
baseline_remove(x,y)
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

\keyword{baseline_remove}

\examples{
	main="Simulated data"
	xlab = expression("Wavenumber / cm"^-1)
	ylab="Raman Intensity/Arbitr. Units"

	y=c(40*dnorm(seq(-5,5,by=0.1),sd=0.2),60*dnorm(seq(-5,5,by=0.1),sd=0.5),30*dnorm(seq(-5,5,by=0.1),sd=0.7),100*rep(0.001,100))
	x=1:length(y)
	plot(x,y,type='l', main="pure signal",xlab=xlab,ylab=ylab)
	y=y+40*rnorm(length(x))*0.01

	yl=30*(1:length(y)/100)
	y=y+yl
	plot(x,y,type='l', main="pure signal with curved background and random noise",xlab=xlab,ylab=ylab)

	lx <- length(x)
	bkgsize <- round(length(y)/10)
	x1 <- x[1:round(lx/bkgsize)]
	x2 <- x[(lx-round(lx/bkgsize)):lx]
	y1 <- y[1:(round(length(x)/bkgsize))]
	y2 <- y[(lx-round(lx/bkgsize)):lx]
	lm.sol <- lm(c(y1,y2)~c(x1,x2))
	bkg <- lm.sol$coef[-1]*x + lm.sol$coef[1]
	yr <- y - bkg;
	lines(x,yr,col='red')
}