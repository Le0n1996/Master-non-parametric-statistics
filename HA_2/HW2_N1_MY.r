library("ggplot2")
library("psych")
library("dplyr")
library("GGally")

# N1 (i)
d<-LifeCycleSavings
x=d$pop15
x1=d$pop75
x2=d$dpi
x3=d$ddpi
y=d$sr
plot(x, y)
spanvec=c(0.05,0.2,0.5)
for (j in (1:3)){
  lines(supsmu(x,y,span=spanvec[j]),col=j)
}
legend(23,5, legend=spanvec, col=seq(spanvec),lty=1,cex=0.6)

plot(x, y)
bassvec=c(1:10)
for (j in (1:10)){
  lines(supsmu(x,y,bass=bassvec[j], span='cv'),  col=j)
}
legend(23.5,10, legend=bassvec, col=seq(bassvec),lty=1,cex=0.5)

MSE<-function(y0,y1){
  e=0
  for (i in (1:50)){
    e=e+(y0[i]-y1[i])^2
  }
  return(e=e/50)
}

res=100
for (i in (1:3)){
  for (j in (1:10)){
    model=supsmu(x,y, span=spanvec[i], bass=bassvec[j])
    z=rep(0, 50)
    for (k in (1:50)){
      z[k]=model$y[which(model$x==x[k])]
    }
    m=MSE(y,z)
    if (m<=res){
      res=m
      spanMSEmin=i
      bassMSEmin=j
    }
  }
}
spanvec[spanMSEmin]
bassvec[bassMSEmin]
res

# N1 (ii)
library("np")
kernelvec = c("gaussian", "epanechnikov")
bwvec=c("cv.aic", "cv.ls")
res=1000
G = function(x, param_vec1, param_vec2){
for (i in (1:2)){
    for (j in (1:2)){
        model = npreg(txdat=x, tydat=y, 
        ckertype=param_vec1[i],
        bwmethod=param_vec2[j])
        m = mean((fitted(model)-y)^2)
        if (m<res)
        {
        res=m
        vec=c(i,j)
        }
     }
   }
        print(vec)
        print(res)
}
Result=G(x, kernelvec, bwvec)
        

# N1 (iii)
supsmu_model=function(vec_x, vec_y){
  res=100
  for (i in (1:3)){
    for (j in (1:10)){
      model1=supsmu(vec_x, vec_y, span=spanvec[i], bass=bassvec[j])
      z=rep(0, 50)
      for (k in (1:50)){
        z[k]=model1$y[which(model1$x==vec_x[k])]
      }
      m=MSE(vec_y, z)
      if (m<=res){
        res=m
        spanMSEmin=i
        bassMSEmin=j
      }
    }
  }
  m1<-c(spanvec[spanMSEmin], bassvec[bassMSEmin], res)
  return(m1)
}

kernel_regress1=function(vec_x, vec_y){
  res=100
  for (i in (1:2)){
    for (j in (1:2)){
      model2=npreg(txdat=vec_x, tydat=vec_y,
                   ckertype=kernelvec[kernelMSEmin],
                   bwmethod=bwvec[bandMSEmin])
      m=MSE(vec_y, fitted(model2))
      if (m<=res){
        res=m
        kernelMSEmin=i
        bandMSEmin=j
      }
    }
  }
  m1<-c(kernelvec[kernelMSEmin], bwvec[bandMSEmin], res)
}

table<-matrix(nrow=4, ncol=6)
for (n in (1:4)){
  x1<-d[,n+1]
  table[n,]<-c(supsmu_model(x1,y), kernel_regress1(x1,y))
  best_model2<-npreg(txdat=x1, tydat=y, ckertype=table[n,4], bwmethod=table[n,5])
}
table

#N1 (iv)
install.packages("fANCOVA")
library(fANCOVA)
x=d$pop15
x3=d$ddpi
y=d$sr
s=loess.as(cbind(x,x3),y,plot=TRUE)
mean((s$fitted-y)^2)

#N1 (v)
install.packages("scatterplot3d")
library(scatterplot3d)
s1 = scatterplot3d(x,x3,y,angle=65)
l=lm(y~x+x3)
length(l$fitted.values)
s1$plane3d(l, col='red')
mean((l$fitted.values-y)^2)

#N1 (vi)
# boxplot и выбросы дл€ ddpi
boxplot(x3)
q=boxplot.stats(x3)$out
q

# boxplot и выбросы дл€ sr
boxplot(y)
q1=boxplot.stats(y)$out
q1

# найдем выбросы с помощью CookТs distances
d1<-data.frame(ddpi=d$ddpi, sr=d$sr)
m0=npreg(d1$sr~d1$ddpi)
A=cbind(d1$ddpi, fitted(m0), d1$sr)
A=A[order(A[,1]),]
A
plot(d1$ddpi, d1$sr, xlab="ddpi", ylab="sr")
points(A[,1], A[,2], type="l", col="blue")
abline(lm(sr~ddpi, data=d1), col="red")
v=A[,1]
boxplot(v)
q=boxplot.stats(v)$out
A=A[-which(v %in% q),1:3]
plot(A[,1], A[,3], xlab="ddpi", ylab="sr")
points(A[,1], A[,2], type="l", col="blue")
abline(lm(sr~ddpi, data=d1), col="red")
#Cook's distance
m1=npreg(A[,3]~A[,1])
A[,2]=fitted(m1)
res=rep(0, nrow(A))
for (i in (1:(dim(A)[1]))){
  d2=data.frame(ddpi=A[-i,1], sr=A[-i,3])
  m2=npreg(sr~ddpi, data=d2)
  p=predict(m2, newdata=data.frame(ddpi=A[i,1]))
  res[i]=(p-A[i,2])^2
}
res
which.max(res)
A[which.max(res),1]
plot(A[,1], A[,3], xlab="ddpi", ylab="sr")
points(A[,1],A[,2], type="l", col="red")

# Compare Nadarya-Watson est for clean and default data
plot(d$ddpi, d$sr)
lines(ksmooth(d$ddpi, d$sr, kernel = "normal", bandwidth = 0.3), col = "blue", lwd = 2)
lines(ksmooth(A[,1], A[,3], kernel = "normal", bandwidth = 0.3), col = "red", lwd = 2)
