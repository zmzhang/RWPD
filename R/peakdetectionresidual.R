peakdetectionresidual <- function(x,y){
	# The detected signal is a segment of the whole signal and the scale also should decrease.
	scales <- seq(1,8,1)
	wCoefs <- cwt(y, scales=scales, wavelet='mexh')
	localMax <- getLocalMaximumCWT(wCoefs)
	ridgeList <- getRidge(localMax)
	SNR.Th <- 1;
	majorPeakInfo <- identifyMajorPeaks(y, ridgeList, wCoefs, 
		SNR.Th=SNR.Th, ridgeLength =4)
	peakIndex <- majorPeakInfo$peakIndex
	position <- matrix(x[peakIndex])
	output <- list(position=position,majorPeakInfo=majorPeakInfo,
		peakIndex=peakIndex)
	return(output)
}