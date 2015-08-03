fiterror <- function(x,y,yr,yf){
	# The yr denotes the results of baseline_remove or the airPls. 
	# The yf are the individual fitting peaks and can obtain it by calling the function of area.
	fitmodel <- colSums(yf)+y-yr;
	fiterror <- 100*sqrt(sum(y-fitmodel)^2/length(y))
	return(fiterror)
}
