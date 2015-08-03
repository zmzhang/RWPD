\name{startend}
\alias{startend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{extract the segment of the analyzed signal and the corresponding peak position and width}
\description{
	extract the segment of the analyzed signal and the corresponding peak position and width
}
\usage{
startend(x,y,width,startpoints,endpoints)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The values of x-axis}
  \item{y}{The values of y-axis}
  \item{width}{estimate the peak width using the CWT with Haar wavelet}
  \item{startpoints}{The startingpoints and endpoints mean the starting point and the end point of the selected segment.}
  \item{endpoints}{The startingpoints and endpoints mean the starting point and the end point of the selected segment.}
}

\value{
  the fitted vector
}

\author{Zhimin Zhang, Xia Tong}

\seealso{\code{\link{peakdetection}}}

\keyword{startend}

