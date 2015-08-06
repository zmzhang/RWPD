peakheights <- function(positionwidth,x,y,peakshape){
	# If the function is Gaussian function, the fitness should be defined as follows.
	# The positionwidth is the results of the estimation or GAs for position or width.
	NumPeaks <- length(positionwidth)/2;
	lambda <- matrix(positionwidth,nrow=NumPeaks,byrow=TRUE)
	A <- matrix(0,length(x),round(length(lambda)/2))
	if (peakshape==1){
		peakshape <- function(x,position,width){
			exp(-((x-position)/(0.6005612*width))^2);  
		}
	}
	else{
		peakshape <- function(x,position,width){
			1/(1+((x-position)/(0.5*width))^2)
		}
	}
	for(j in 1:(length(lambda)/2))
	{
		A[ ,j] <- peakshape(x,lambda[j,1],lambda[j,2]);
	}
	lf=lsfit(A,y)
	PEAKHEIGHTS=abs(lf$coef[-1])
	return(matrix(PEAKHEIGHTS))
}
