plotwidthEstimation <- function(x,y,peakWidth) {
	y=as.vector(y)
    lmindex=1:length(y)
    peakIndex=peakWidth$peakIndex
    LR = NULL 
	plot(x,y,type='l')     
    points(x[peakIndex],y[peakIndex],pch=15)
    for (i in 1:length(peakIndex)){
        peakWidth.i=peakWidth$peakIndexLower[i]:peakWidth$peakIndexUpper[i]
        LR=c(LR,peakWidth.i[c(1,length(peakWidth.i))])
    }
	points(LR,x[LR])
    return ("successful")
}
