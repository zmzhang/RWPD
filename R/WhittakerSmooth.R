WhittakerSmooth <- function(x,w,lambda,differences=1) {
	x=matrix(x,nrow = 1, ncol=length(x))
	L=length(x)
	E=spMatrix(L,L,i=seq(1,L),j=seq(1,L),rep(1,L))
	D=as(diff(E,1,differences),"dgCMatrix")
	W=as(spMatrix(L,L,i=seq(1,L),j=seq(1,L),w),"dgCMatrix")
	background=solve((W+lambda*t(D)%*%D),t((w*x)));
	return(as.vector(background))
}
