\name{airPLS}
\alias{airPLS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{adaptive iteratively reweighted penalized least squares}
\description{
 adaptive iteratively reweighted penalized least squares for baseline fitting
}
\usage{
airPLS(x,y,lambda=10,differences=1, itermax=20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The values of x-axis}
  \item{y}{The values of y-axis}
  \item{lambda}{lambda is an adjustable parameter, it can be adjusted by user. The larger lambda is,  the smoother z will be }
  \item{differences}{ an integer indicating the order of the difference of penalties}
}

\value{
  the fitted vector
}


\author{Zhimin Zhang, Xia Tong}

\seealso{\code{\link{WhittakerSmooth}}}

\keyword{airPLS}

\example
	main="Simulated data"
	xlab = expression("Wavenumber / cm"^-1)
	ylab="Raman Intensity/Arbitr. Units"

	y=c(40*dnorm(seq(-5,5,by=0.1),sd=0.2),60*dnorm(seq(-5,5,by=0.1),sd=0.5),30*dnorm(seq(-5,5,by=0.1),sd=0.7),100*rep(0.001,100))
	x=1:length(y)
	plot(x,y,type='l', main="pure signal",xlab=xlab,ylab=ylab)
	y=y+40*rnorm(length(x))*0.01

	yc=30*sin(1:length(y)/100)
	yc=y+yc
	y=yc
	plot(x,y,type='l', main="pure signal with curved background and random noise",xlab=xlab,ylab=ylab)

	lambda=10
	differences=1
	itermax=20
	y = as.vector(y)
	m = length(y)
	w = rep(1,m)
	control = 1
	i = 1
	while(control==1){
		z = WhittakerSmooth(y,w,lambda,differences)
		d = y-z
		sum_smaller = abs(sum(d[d<0])) 
			if(sum_smaller<0.001*sum(abs(y))||i==itermax)
				{
				control = 0
				}
		w[d>=0] = 0
		w[d<0] = exp(i*abs(d[d<0])/sum_smaller)
		w[1] = exp(i*max(d[d<0])/sum_smaller)
		w[m] = exp(i*max(d[d<0])/sum_smaller)
		i=i+1
	}
	yr <- y-z;
	lines(x,yr)
