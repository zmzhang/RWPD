\name{peakrange }
\alias{peakrange }
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Set the boundary of peak position and width}
\description{
	Set the upper and lower boundary of peak position and width
}
\usage{
peakrange(position,width,pos,wid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{position}{The position and width are the estimated results from peakdetection and widthEstimation or the results from GAs. }
  \item{width}{The position and width are the estimated results from peakdetection and widthEstimation or the results from GAs. }
  \item{pos}{The pos and wid are the coefficients of position and width, and both of them are between 0 and 1.}
  \item{wid}{The pos and wid are the coefficients of position and width, and both of them are between 0 and 1.}

}

\value{
  the fitted vector
}

\author{Zhimin Zhang, Xia Tong}

\keyword{peakrange}


