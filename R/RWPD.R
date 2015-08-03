RWPD <- function(x,yr,yf,width,position){
	# This function can find the undetected peaks which is not detected in the first iteration, and estimates the peak width based on the results in the first iteration. 
	residual <- yr-colSums(yf);
	# The residual signal can be calculated which from the results of baseline remove deduct the fitting signal.
	positionresidual <- peakdetectionresidual(x,residual)$position;
	# using the CWT to detecting the position in the residual signal
	p <- matrix(which(x %in% positionresidual));
	ymax <- matrix(0,nrow(p),ncol(p));
	for (i in 1:length(p)) {
    		lowerbound <- x[p[i]-5];
    		upperbound <- x[p[i]+5];
		xrange <- which(x >= lowerbound & x <= upperbound);
		ymax[i] <- max(yr[xrange]);
	pos <- which(yr %in% ymax);
	positionresidualmax <- matrix(x[pos]);
	}
	# search the maximum of yr in a defined region
	posr <- which(abs(positionresidual-positionresidualmax)<10);
	# remove the position which is not a local maximum point
	positionr <- matrix(positionresidual[posr]);
	# obtain the final position in residual signal 
	allposition <- sort(rbind(matrix(position),positionr));
	# sort the matrix which combines the position in first time and the position in residual signal
	position <- matrix(allposition)[-which(diff(allposition)<10),];
	# remove the same peaks in first time and in residual signal, 
	position <- matrix(position);
	width <- matrix(c(sum(width)/length(position)),
		nrow=length(position));
	output <- list(position=position,width=width);
	return(output)
}
