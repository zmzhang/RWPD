\name{fiterror}
\alias{fiterror}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{To compute the error between the fitting signal and the raw signal}
\description{
	To compute the difference between the fitting signal and the raw signal
}
\usage{
fiterror(x,y,yr,yf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The values of x-axis}
  \item{y}{The values of y-axis}
  \item{yr}{The yr denotes the results of baseline_remove or the airPls.}
  \item{yf}{The yf is the collection of each fitting peaks.}
}

\value{
  the fitted vector
}

\author{Zhimin Zhang, Xia Tong}

\seealso{\code{\link{baseline_remove,airPls,areayf}}}

\keyword{fiterror}
