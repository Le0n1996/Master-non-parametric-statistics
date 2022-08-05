#N1
#(i)
rm(list=ls())
data=LakeHuron
n=length(data)
hist(data, breaks='Sturges')

#(ii)
K=eval(formals(density.default)$kernel)
plot(density(data), xlab="")
for (i in (2:length(K))){
  lines(density(data, kernel=K[i]), col=i)
}
legend("topleft", 0.25, legend=K, col=seq(K), lty=1, cex=0.5)

#(iii)
plot(density(data, bw='nrd0'), xlab="")
bandwidth=c('nrd', 'ucv', 'bcv', 'SJ-ste', 'SJ-dpi')
for (i in (2:length(bandwidth))){
  lines(density(data, bw=bandwidth[i]), col=i)
}
legend("topleft", 0.25, legend=bandwidth, col=seq(bandwidth), lty=1, cex=0.5)

#(iv)
plot(density(data, bw='nrd0'), xlab="")
bandwidth=c('nrd', 'ucv', 'SJ-ste', 'SJ-dpi')
for (i in (2:length(bandwidth))){
  lines(density(data, bw=bandwidth[i]), col=i)
}
legend("topleft", 0.25, legend=bandwidth, col=seq(bandwidth), lty=1, cex=0.5)
lines(density(rnorm(10000, mean=mean(data), sd=sd(data))), col=12)

#(v)
res=hist(data, breaks='Sturges')
M=matrix(nrow=length(K), ncol=length(bandwidth))
for (i in (1:length(K))){
	for (j in (1:length(bandwidth))){
		d=density(data, kernel=K[i], bw=bandwidth[j])
		mise=0
		points=c(575.5, 576.5, 577.5, 578.5, 579.5, 580.5, 581.5)
		for (p in (1:length(points))){
			x=d$x
			value=d$y[which(abs(points[p]-x)==min(abs(points[p]-x)))]
			mise=mise+(value-res$density[p])**2
		}
		M[i, j]=mise/length(points)
	}
}
M
min(M)
coordinates=which(M == min(M), arr.ind = TRUE)

d=density(rnorm(10000, mean=mean(data), sd=sd(data)))
mise=0
for (p in (1:length(points))){
	x=d$x
	value=d$y[which(abs(points[p]-x)==min(abs(points[p]-x)))]
	mise=mise+(value-res$density[p])**2
}
if (min(M)<mise){
print(K[coordinates[1]])
print(bandwidth[coordinates[2]])}else{
print("Normal density approximation is the best")
}


