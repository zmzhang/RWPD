fitshape <- function(lambda,x,y,shape){
	# lambda <- as.vector(t(cbind(position,width)));
	A <- matrix(0,length(x),round(length(lambda)/2))
	for(j in 1:(length(lambda)/2))
	{
		position <-lambda[2*j-1] ; width <- lambda[2*j];
		A[ ,j] <- peakshape(x,lambda[2*j-1],lambda[2*j],shape);
	}
	lf=lsfit(A,y)
	PEAKHEIGHTS=abs(lf$coef[-1])
	e=y-A%*%PEAKHEIGHTS
	return(base::norm(as.matrix(e),'2'))
}

