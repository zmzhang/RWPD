polynomial fitting <- function(x,y){
	lx <- length(x)
	bkgsize <- round(length(y)/10)
	x1 <- x[1:round(lx/bkgsize)]
	x2 <- x[(lx-round(lx/bkgsize)):lx]
	y1 <- y[1:(round(length(x)/bkgsize))]
	y2 <- y[(lx-round(lx/bkgsize)):lx]
	lm.sol <- lm(c(y1,y2)~c(x1,x2))
	bkg <- lm.sol$coef[-1]*x + lm.sol$coef[1]
	yr <- y - bkg
	return(yr)
}
