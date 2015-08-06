areayf <- function(fitresults,x,peakheight,peakshape){
	# If the function is lorentzian function, the fitness should be defined as follows.
	# The fitresults are the fitting results of GA.
	# The peakheight is the results of the PEAKHEIGHTS function.
	# The yf is a collection of each fitting peak.
	NumPeaks <- length(peakheight);
	g <- matrix(0,NumPeaks,length(x))
	yf <- matrix(0,NumPeaks,length(x))
	area <- matrix(0,1,NumPeaks)
	if (peakshape==1){
		peakshape <- function(x,position,width){
			exp(-((x-position)/(0.6005612*width))^2);
		# Gaussian function
		}
	}
	else{
		peakshape <- function(x,position,width){
			1/(1+((x-position)/(0.5*width))^2)
		# Lorentzian function
		}
	}
	for(m in 1:NumPeaks){
		g[m,] <- peakshape(x,fitresults[2*m-1],fitresults[2*m]);
		yf[m,] <- g[m,]*peakheight[m];
		area[m] <- trapezoidal.integration(x,yf[m,]);
	}
	output <- list(area=area,yf=yf)
	return(output)
}