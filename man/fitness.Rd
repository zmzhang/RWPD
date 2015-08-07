\name{fitness}
\alias{fitness}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{the optimized function by GA}
\description{
	the optimized function by GA
}
\usage{
	fitness <- -fitshape(lambda,x,y,shape)
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

\seealso{\code{\link{fitshape}}}

\keyword{fitness}



