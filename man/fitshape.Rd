\name{fitshape}
\alias{fitshape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{the optimized function by GA}
\description{
	the results of least squares norm of the difference between the fitting analytical signal and raw analytical signal
}
\usage{
	fitshape(lambda,x,y,shape)
%- maybe also 'usage' for other objects documented here.
}
\arguments{
  \item{lambda}{lambda is a row matrix including peak position and width in above step.}
  \item{x}{The values of x-axis}
  \item{y}{The values of y-axis}
  \item{shape}{The shape of peak}
}

\value{
  the fitted vector
}

\author{Zhimin Zhang, Xia Tong}

\seealso{\code{\link{fitnessfunction}}}

\keyword{fitshape}



