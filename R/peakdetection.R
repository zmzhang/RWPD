peakdetection <- function(x,y,sca){
	scales <- seq(1,sca,1)
	wCoefs <- cwt(y, scales=scales, wavelet='mexh')
	localMax <- getLocalMaximumCWT(wCoefs)
	ridgeList <- getRidge(localMax)
	SNR.Th <- 2
	majorPeakInfo <- identifyMajorPeaks(y, ridgeList, wCoefs, 
		SNR.Th=SNR.Th, ridgeLength =4)
	peakIndex <- majorPeakInfo$peakIndex
	position <- matrix(x[peakIndex])
	output <- list(position=position,majorPeakInfo=majorPeakInfo,
		peakIndex=peakIndex)
	return(output)
}