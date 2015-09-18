startend <- function(x,y,sca,width,startpoints,endpoints){
	# The startingpoints and endpoints mean the starting point and the end point of the selected segment.
	# The width is the width in 
	posi <- peakdetection(x,y,sca)$peakIndex;
	position <- x[posi];
	s <- startpoints;
	e <- endpoints;
	rs_start = which.min(abs(x-s));
	rs_end   = which.min(abs(x-e));
	peaks=posi[posi > rs_start & posi < rs_end]-rs_start+1;
	xa=matrix(x[rs_start:rs_end]);
	ya=matrix(y[rs_start:rs_end]);
	positiona <- matrix(xa[peaks]);
	posi <- which(position %in% positiona);
	widtha <- matrix(width[posi])
	output <- list(positiona=positiona,widtha=widtha,xa=xa,ya=ya);
	return(output)
}

