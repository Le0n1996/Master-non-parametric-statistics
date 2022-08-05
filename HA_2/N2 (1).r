N2

X=runif(100, min=0, max=1)
e=rnorm(100, sd=0.05)
Y=sin(2*X)+e

#(1)
k=3

#(2)
reoder=function(i){
	X2=abs(X-X[i])
	M=cbind(X, e, X2)
	M=M[order(M[,3]),]
	return(M)
}

#(3)
f=rep(0, 100)
for(i in (1:100)){
	f[i]=mean(sin(2*reoder(i)[,1][2:k+1])+reoder(i)[,2][2:k+1])
	}
f
M=cbind(X, f)
M=M[order(M[,1]),]
plot(X, Y)
lines(M, col=2)
#MSE of this estimation:
mean((Y-f)**2)

#(4)
e1=f-Y
sigma=rep(0, 100)
for (i in (1:100)){
	if (abs(e1[i])<1){
		sigma[i]=(1-abs(e1[i]))**4
		}
	}
sigma

#(5)
ki=round(k/sigma)
f=rep(0, 100)
for(i in (1:100)){
	f[i]=mean(sin(2*reoder(i)[,1][2:ki[i]+1])+reoder(i)[,2][2:ki[i]+1])
	}
M=cbind(X, f)
M=M[order(M[,1]),]
lines(M, col=3)

#(6)
for (j in (1:10)){
	e1=f-Y
	sigma=rep(0, 100)
	for (i in (1:100)){
		if (abs(e1[i])<1){
			sigma[i]=(1-abs(e1[i]))**4
			}
		}
	ki=round(k/sigma)
	f=rep(0, 100)
	for(i in (1:100)){
		f[i]=mean(sin(2*reoder(i)[,1][2:ki[i]+1])+reoder(i)[,2][2:ki[i]+1])
		}
	}
M=cbind(X, f)
M=M[order(M[,1]),]
lines(M, col=4)
#MSE of final estimation:
mean((Y-f)**2)

