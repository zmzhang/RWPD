library(GA)
fitshape <- function(lambda,x,y,peakshape){
	# lambda <- as.vector(t(cbind(position,width)));
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
		position <-lambda[2*j-1] ; width <- lambda[2*j];
		A[ ,j] <- peakshape(x,lambda[2*j-1],lambda[2*j]);
	}
	lf=lsfit(A,y)
	PEAKHEIGHTS=abs(lf$coef[-1])
	e=y-A%*%PEAKHEIGHTS
	return(base::norm(as.matrix(e),'2'))
}

fitness <- function(lambda,x,y,peakshape){
	-fitshape(lambda,x,y,peakshape)
}

