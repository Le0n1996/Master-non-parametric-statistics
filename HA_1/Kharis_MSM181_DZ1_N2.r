#N2
#(i)
rm(list=ls())
p=function(x){
	p=dnorm(x)/2 + dchisq(x+3, df=2)/4 + dchisq(-x+3, df=2)/4
	return(p)
}
S=seq(-5, 5, by=0.1)
D=c(rep(0, 100))
for (i in (1:length(S))){
	D[i]=p(S[i])
}
plot(D, type='l')

#(ii)
sample=approx(cumsum(D)/sum(D),S,runif(1000))$y

#(iii)
res=hist(sample, breaks='Sturges') 
mids=res$mids 
X=seq(-4, 4, by=0.0008) 
MISE=0 
for (i in (1:length(X))){ 
	p_x=res$density[which(abs(mids-X[i])==min(abs(mids-X[i])))[1]] 
	P_x=p(X[i]) 
	MISE=MISE+(p_x-P_x)**2/10000 
} 
MISE 

#(iv) 
cross=c(rep(0, 20)) 
for (J in (1:20)){ 
	sample=approx(cumsum(D)/sum(D),S,runif(1000))$y 
	res=hist(sample, breaks='Sturges') 
	mids=res$mids 
	MISE=0 
	for (i in (1:length(X))){ 
		p_x=res$density[which(abs(mids-X[i])==min(abs(mids-X[i])))[1]] 
		P_x=p(X[i]) 
		MISE=MISE+(p_x-P_x)**2/10000 
	} 
	cross[J]=MISE 
} 
cross_MISE=sum(cross)/20 
cross_MISE 

#(v)
dif_MISE=c(0, 0, 0) 
band=c('Sturges', 'Scott', 'FD')
for (B in (1:3)){
	cross=c(rep(0, 20))
	for (J in (1:20)){ 
		sample=approx(cumsum(D)/sum(D),S,runif(1000))$y 
		res=hist(sample, breaks=band[B]) 
		mids=res$mids 
		MISE=0 
		for (i in (1:length(X))){ 
			p_x=res$density[which(abs(mids-X[i])==min(abs(mids-X[i])))[1]] 
			P_x=p(X[i]) 
			MISE=MISE+(p_x-P_x)**2/10000 
		} 
		cross[J]=MISE 
	} 
	cross_MISE=sum(cross)/20 
	dif_MISE[B]=cross_MISE
}
dif_MISE
band[which(dif_MISE==min(dif_MISE))]

#(vi)
sample=approx(cumsum(D)/sum(D),S,runif(1000))$y
kernel_bd=seq(0.01, 1, by=0.01)
results=c(rep(0, 99))
for (j in (1:99)){
	d=density(sample, kernel='epanechnikov', bw=kernel_bd[j])
	MISE=0
	for (i in (1:length(sample))){ 
		p_x=d$y[which(abs(d$x-sample[i])==min(abs(d$x-sample[i])))[1]] 
		P_x=p(sample[i]) 
		MISE=MISE+(p_x-P_x)**2/10000 
	} 
	results[j]=MISE
}
kernel_bd[which(results==min(results))]

#(vii)
band[which(dif_MISE==min(dif_MISE))]
kernel_bd[which(results==min(results))]

plot(hist(sample, breaks=band[which(dif_MISE==min(dif_MISE))]), freq=FALSE)
lines(density(sample, kernel='epanechnikov', bw=kernel_bd[which(results==min(results))]), col=2)
par(new=TRUE)
plot(D, col=3, type='l', axes=FALSE)




