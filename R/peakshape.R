peakshape <- function(x,position,width,shape){
	y=seq(1,1,1)
	if (shape==1){
			y=exp(-((x-position)/(0.6005612*width))^2);
		}
	else{
			y=1/(1+((x-position)/(0.5*width))^2)
	}
	return(y)
}