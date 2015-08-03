Gaussian <- function(x,position,width){
# Gaussian function
	exp(-((x-position)/(0.6005612*width))^2);  
}

lorentzian <- function(x,position,width){
# lorentzian function
	1/(1+((x-position)/(0.5*width))^2)
}