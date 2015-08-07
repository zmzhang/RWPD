\name{cwt}
\alias{cwt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Continuous Wavelet Transform (CWT) }
\description{
  CWT(Continuous Wavelet Transform) with Mexican Hat wavelet (by default) to match the peaks in Mass Spectrometry spectrum
}
\usage{
cwt(ms, scales = 1, wavelet = "mexh")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ms}{ Mass Spectrometry spectrum (a vector of MS intensities)}
  \item{scales}{ a vector represents the scales at which to perform CWT. }
  \item{wavelet}{The wavelet base, Mexican Hat by default. User can provide wavelet Psi(x) as a form of two row matrix. The first 
	row is the x value, and the second row is Psi(x) corresponding to x.  }
}

\value{
The return is the 2-D CWT coefficient matrix, with column names as the scale. Each column is the CWT coefficients at that scale.
}

\author{Pan Du, Simon Lin}

\examples{
	main="Simulated data"
	xlab = expression("Wavenumber / cm"^-1)
	ylab="Raman Intensity/Arbitr. Units"
	y=c(40*dnorm(seq(-5,5,by=0.1),sd=0.2),60*dnorm(seq(-5,5,by=0.1),sd=0.5),30*dnorm(seq(-5,5,by=0.1),sd=0.7),100*rep(0.001,100))
	x=1:length(y)
	plot(x,y,type='l', main="pure signal",xlab=xlab,ylab=ylab)
	y=y+40*rnorm(length(x))*0.01
	scales <- seq(1, 20, 1)
	wCoefs <- cwt(y, scales=scales, wavelet='mexh')
	image(x, scales, wCoefs, col=terrain.colors(256), axes=FALSE, xlab='m/z index', ylab='CWT coefficient scale', main='CWT coefficients')
	box()
}
\keyword{ methods }
