fitlorentzian <- function(lambda,x,y){
	# If the function is lorentzian function, the fitness should be defined as follows.
	# lambda <- as.vector(t(cbind(position,width)));
	A <- matrix(0,length(x),round(length(lambda)/2))
	for(j in 1:(length(lambda)/2))
	{
		position <-lambda[2*j-1] ; width <- lambda[2*j];
		A[ ,j] <- lorentzian(x,lambda[2*j-1],lambda[2*j]);
	}
	lf=lsfit(A,y)
	PEAKHEIGHTS=abs(lf$coef[-1])
	e=y-A%*%PEAKHEIGHTS
	return(base::norm(as.matrix(e),'2'))
}

fitlorentzian <- function(lambda,x,y){
	# If the function is Gaussian function, the fitness should be defined as follows.
	# lambda <- as.vector(t(cbind(position,width)));
	A <- matrix(0,length(x),round(length(lambda)/2))
	for(j in 1:(length(lambda)/2))
	{
		position <-lambda[2*j-1] ; width <- lambda[2*j];
		A[ ,j] <- Gaussian(x,lambda[2*j-1],lambda[2*j]);
	}
	lf=lsfit(A,y)
	PEAKHEIGHTS=abs(lf$coef[-1])
	e=y-A%*%PEAKHEIGHTS
	return(base::norm(as.matrix(e),'2'))
}

fitness <- function(lambda,x,y){
	-fitlorentzian(lambda,x,y)
}



