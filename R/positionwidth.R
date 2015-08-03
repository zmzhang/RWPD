positionwidth <- function(fitresults){
	# The fitresults are the fitting results of GA. 
	# This function can extract the fitting position and width of peaks.
	for (i in (1:(length(fitresults)/2))){
		position[i] <- fitresults[1,(2*i-1)];
		width[i] <- fitresults[1,(2*i)];
	}
	output <- list(position=position,width=width)
	return(output)
}