trapezoidal.integration <- function(x,yf){
	# The yf is a collection of each fitting peaks by calling the area function.
	### 3 checks to ensure that the arguments are numeric and of equal lengths
	# check if the variable of integration is numeric
	if (!is.numeric(x))
	{
	stop('The variable of integration "x" is not numeric.')
	}

	# check if the integrand is numeric
	if (!is.numeric(yf))
	{
	stop('The integrand "yf" is not numeric.')
	}

	# check if the variable of integration and the integrand have equal lengths
	if (length(x) != length(yf))
	{
	stop('The lengths of the variable of integration and the integrand do not match.')
	}

	### finish checks

	# obtain length of variable of integration and integrand
	n = length(x)

	# integrate using the trapezoidal rule
	integral = 0.5*sum((x[2:n] - x[1:(n-1)]) * (yf[2:n] + yf[1:(n-1)]))

	# print the definite integral
	return(integral)
}

