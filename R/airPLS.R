airPLS <- function(x,y,lambda=10,differences=1, itermax=20){
	y = as.vector(y)
	m = length(y)
	w = rep(1,m)
	control = 1
	i = 1
	while(control==1){
		z = WhittakerSmooth(y,w,lambda,differences)
		d = y-z
		sum_smaller = abs(sum(d[d<0])) 
			if(sum_smaller<0.001*sum(abs(y))||i==itermax)
				{
				control = 0
				}
		w[d>=0] = 0
		w[d<0] = exp(i*abs(d[d<0])/sum_smaller)
		w[1] = exp(i*max(d[d<0])/sum_smaller)
		w[m] = exp(i*max(d[d<0])/sum_smaller)
		i=i+1
	}
	yr <- y-z;
	return(yr)
}
