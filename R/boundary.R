boundary <- function(position,width,pos,wid){
	# Set the boundary of peak position and width
	# The position and width are the estimated results from peakdetection and widthEstimation or the results from GAs.
	lambda <- cbind(position,width);
	NumPeaks <- nrow(lambda);
	lb <- as.vector(lambda);
	ub <- as.vector(lambda);
	for(i in 1:NumPeaks){
		ub[2*i-1] <- lambda[i,1]+pos*lambda[i,1];
		lb[2*i-1] <- lambda[i,1]-pos*lambda[i,1];
		ub[2*i] <- lambda[i,2]+wid*lambda[i,2];
		lb[2*i] <- lambda[i,2]-wid*lambda[i,2];
	}
	output <- list(ub=ub,lb=lb)
	return(output)
}