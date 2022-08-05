# seminar 3
# seminar 3
# seminar 3
# seminar 3
# seminar 3
faithful
x=faithful$eruptions
x
plot(density(x))
density(x)$bw
band=c("nrd0", "nrd", "ucv", "bcv", "SJ")
for (j in (2:5)){
  lines(density(x, bw=band[j]),col=j)
  print(density(x, bw=band[j])$bw)
}

N=1000
xi=sample(0:5,size=N,replace=TRUE, 
          prob = c(rep(0.1,5),0.5))
table(xi)
m=c(-1,-1/2,0,1/2,1,0)
s=c(rep(0.1,5),1)
X=rep(0,N)
for (j in (1:N)){
  X[j]=rnorm(1, mean=m[xi[j]+1], 
             sd=s[xi[j]+1])
}
K=eval(formals(density.default)$kernel)
plot(density(X))
for (j in (2:5)){
  #lines(density(X, kernel=K[j]), col=j)
  lines(density(X, bw=band[j]),col=j)
}

p=function(x){
  y=dnorm(x)/2
  for (j in (0:4)){
    y=y+dnorm(x,mean=m[j+1],sd=0.1)/10
  }
  return(y)
}

J=function(d){
  R=0
  for (i in (1:length(d$x))){
    R=R+(p(d$x[i])-d$y[i])^2
  }
  return(R)
}

M=matrix(data=NA, nrow=7, ncol=5)
for (j in (1:7)){
  for (k in (1:5)){
    M[j,k]=J(density(X, kernel=K[j], bw=band[k]))
  }
}
which.min(M)
M
j=6
k=3
plot(density(X, kernel=K[j], bw=band[k]))
density(X, kernel=K[j], bw=band[k])$bw

vec=seq(from=0.0001, to=0.2, length=100)
M2=rep(0,100)
for (j in (1:100)){
  M2[j]=J(density(X, bw=vec[j]))
}
which.min(M2)
plot(density(X, bw=vec[26]))

# seminar 5
# seminar 5
# seminar 5
# seminar 5
# seminar 5

cars
plot(cars)
x=cars$speed
y=cars$dist
spanvec=c(0.05, 0.2, 0.5, 1)
for (j in (1:4)){
  lines(supsmu(x, y, span=spanvec[j]),col=j)
}
legend(18,40, legend=spanvec, col=seq(spanvec),
       lty=1,cex=.5)
plot(cars)
lines(supsmu(x, y))
lines(supsmu(x, y, span=0.2), col="red")

plot(cars)
lines(supsmu(x, y, span=1), col="red")
abline(lm(y~x))

plot(cars)
lines(supsmu(x, y, bass=10)))
lines(supsmu(x, y, bass=0), col="red")

l1=loess(dist~speed, cars, 
         control=loess.control(surface="direct"))
plot(cars)
lines(l1)
s=seq(from=5, to=30, by=1)
s
p=predict(l1, data.frame(speed=s), se=TRUE)
plot(x,y, xlim=c(4,31))
lines(s, p$fit, col="red")

install.packages("fANCOVA")
library(fANCOVA)
loess.as(x, y, plot=TRUE)
l2=loess.as(x, y, criterion="gcv", plot=TRUE)
loess.as(x, y, criterion="aicc", plot=TRUE)
l2$fitted

install.packages("np")
library(np)
bvec=c("cv.aic", "cv.ls")
kervec=c("epanechnikov","gaussian")
polvec=c("lc","ll")
res=1000
for (i in (1:2)){
  for (j in (1:2)){
    for (k in (1:2)){
      model=npreg(txdat=x, tydat=y,
                  ckertype=kervec[i],
                  regtype=polvec[j],
                  bwmethod=bvec[k])
      m=mean((fitted(model)-y)^2)
      if (m<res){
        res=m
        vec=c(i,j,k)
      }
    }
  }
}
model=npreg(txdat=x, tydat=y,
            ckertype=kervec[vec[1]],
            regtype=polvec[vec[2]],
            bwmethod=bvec[vec[3]])
plot(cars)
points(x, fitted(model), type="l", col="red")

r=function(v, cc){
  print(mean((y-v)^2))
  points(x, v, type="l", col=cc)
}
plot(x,y)
r(fitted(model), "red")
model2=loess(y~x)
r(predict(model2),"blue")
model3=supsmu(x,y)
yy=rep(0,length(x))
for (j in (1: length(x))){
  yy[j]=model3$y[which(model3$x==x[j])]
}
r(yy,"green”)

# seminar 6
# seminar 6
# seminar 6
# seminar 6
# seminar 6
# seminar 6

install.packages("fANCOVA")
library(fANCOVA)
install.packages("np")
library(np)
N=1000
x1=runif(N, min=0, max=3)
x2=runif(N, min=0, max=3)
e=rnorm(N, sd= 0.25)
y=sin(2*x1)+sin(2*x2)+e
install.packages("scatterplot3d")
library(scatterplot3d)
s=scatterplot3d(x1,x2,y,angle=65)
l=lm(y~x1+x2)
s$plane3d(l, col="red")
mean((l$fitted.values-y)^2)
s2=loess.as(cbind(x1,x2),y,plot=TRUE)
mean((s2$fitted-y)^2)

plot(cars)
carsout=data.frame(speed=c(5,19,19,20,20,20), dist=c(100, 190,186,210,220,218))
cars2=rbind(cars,carsout)
plot(cars2)
abline(lm(dist~speed, data=cars2), col="red")
model=npreg(cars2$speed, cars2$dist)
points(cars2$speed, fitted(model),type="l", col="blue")
A=cbind(cars2$speed, fitted(model), cars2$dist)
A=A[order(A[,1]),]
A
plot(cars2)
abline(lm(dist~speed, data=cars2), col="red")
points(A[,1], A[,2], type="l", col="blue")
y=A[,3]
boxplot(y)
q=boxplot.stats(y)$out
A=A[-which(y %in% q),1:3]
plot(A[,1], A[,3])

model3=npreg(A[,3]~A[,1])
A[,2]=fitted(model3)

res=rep(0,nrow(A))
for (i in (1:(dim(A)[1]))){
  d=data.frame(speed=A[-i,1], dist=A[-i,3])
  model2= npreg(dist~speed, data=d)
  p=predict(model2, newdata=data.frame(speed=A[i,1]))
  res[i]=(p-A[i,2])^2
}
res
A[which.max(res),1]
plot(A[,1], A[,3])
points(A[,1], A[,2], type="l", col="red")

# seminar 7
# seminar 7
# seminar 7
# seminar 7
# seminar 7
# seminar 7
# seminar 7
# seminar 7
# seminar 7

?mtcars
x=mtcars[,1]
ks.test(unique(x),"pnorm")
q=rnorm(100000, mean(x), sd(x))
ks.test(unique(x),q)

plot(density(x))

qqnorm(x)
x
qqline(x, col="red")

install.packages("SuppDists")
library(SuppDists)
pKendall( -16/36, N=9, lower.tail = T)

x=seq(-1,1, length=50)
y=pKendall(x, N=3, lower.tail=TRUE)
plot(x, y, type="l")

x=mtcars[,1]
?mtcars
y=mtcars[,6]
plot(x,y)
rho=cor.test(x, y, method="pearson")$estimate
rho*sd(y)/sd(x)
lm(y~x)

cor.test(x,y, method="kendall")
M=cbind(x,y)
duplicated(x)
M=M[-which(duplicated(x)),]
M=M[-which(duplicated(M[,2])),]
cor.test(M[,1], M[,2], method="kendall")

res=matrix(data=NA, nrow=6, ncol=6)
M=mtcars[,c(1,3:7)]
for (i in (1:6)){
  for (j in (1:6)){
    w=cor.test(M[,i],M[,j],method="kendall")
    res[i,j]=round(w$estimate,2)
    if (w$p.value<0.05){
      res[i,j]=paste(toString(res[i,j]),'*')
    }
    if (w$p.value<0.001){
      res[i,j]=paste(toString(res[i,j]),'*')
    }
  }
}
res

install.packages("NSM3")
library(NSM3)
kendall.ci(x, y, alpha=0.1, type="t", bootstrap=T, B=100)

# seminar 8
# seminar 8
# seminar 8
# seminar 8
# seminar 8
# seminar 8
# seminar 8
# seminar 8

?mtcars
x=mtcars[,1]
y=mtcars[,6]
cor.test(x, y, method="spearman",exact=NULL)
cor.test(rank(x), rank(y), method="pearson")
M=cbind(x,y)
M=M[-which(duplicated(x)),]
M=M[-which(duplicated(M[,2])),]
cor.test(M[,1],M[,2], method="spearman",exact=NULL)

install.packages("pspearman")
library(pspearman)
spearman.test(M[,1], M[,2], approximation = "exact")
spearman.test(M[,1], M[,2], approximation = "t-distribution")
spearman.test(M[,1], M[,2], approximation = "AS89")

N=10
f=function(u){
  pspearman(u, N, approximation="exact")
}
maxval=(N^3-N)/3
int=seq(0, maxval, length=100)
plot(int, sapply(int, f), type="l")

ldeaths
mdeaths
fdeaths

ldeaths-mdeaths-fdeaths

matrm = matrix(data=mdeaths, ncol=6, nrow=12)
matrf = matrix(data=fdeaths, ncol=6, nrow=12)
m=colSums(matrm)
f=colSums(matrf)
plot(m, f, type="b", xlim=c(15500, max(m)+500))
text(m, f,labels=1974:1979, pos=2)

cor.test(m, f, method="pearson")
cor.test(m, f, method="kendall")
cor.test(m, f, method="spearman")

airquality
?airquality
head(airquality)
x=airquality[,1]
names=c("Pearson", "Kendall", "Spearman")
res=rep(0,3)
for (i in (2:4)){
  y=airquality[,i]
  c1=cor.test(x,y, method="pearson")
  c2=cor.test(x,y, method="kendall")
  c3=cor.test(x,y, method="spearman")
  vec1=c(c1$p.value, c2$p.value, c3$p.value)
  vec2=c(c1$estimate, c2$estimate, c3$estimate)
  ind=which.max(vec1)
  res[i-1]=paste(names[ind], toString(round(vec2[ind],2)), toString(round(vec1[ind],2)))
}
res

# seminar 9
# seminar 9
# seminar 9
# seminar 9
# seminar 9
# seminar 9
# seminar 9
# seminar 9
# seminar 9

?wilcox.test
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
wilcox.test(x, y, paired = TRUE, alternative = "greater")
wilcox.test(x-y,  alternative = "greater")
wilcox.test(x-y, alternative="greater", conf.int=TRUE)

x = c(1042,1617,1180,973,1552)
y = c(874,389,612)
wilcox.test(x, y)
boxplot(x, y, col="yellow")
grid()
wilcox.test(x,y, exact=FALSE, alternative="greater")

?kruskal.test
x <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
kruskal.test(list(x, y, z))
boxplot(x,y,z)
f = function(a,b){
  print(wilcox.test(a, b, alternative="greater")$p.value)
}
f(x,y)
f(x,z)
f(y,z)

head(airquality)
aver=rep(0,5)
air2=airquality
for (i in 5:9){
  ind=which(airquality$Month==i)
  aver[i-4]=mean(airquality$Ozone[ind],na.rm = TRUE)
}
for (j in 1:153){
  if (is.na(airquality$Ozone)[j]==T){
    air2[j,1]=aver[airquality$Month[j]-4]
  }
}

M=airquality[,2:4]
M=scale(M)
s=rep(0,123)
res=rep(0,30)
for (i in 124:153){
  x=M[i,]
  for (j in 1:123){
    s[j]=dist(rbind(x,M[j,]))
  }
  res[i-123]=air2[which.min(s),1]
}
res
y=airquality$Ozone[124:153]
plot(res, y)
wilcox.test(res, y, paired=TRUE, alternative="greater")
wilcox.test(res, y, paired=TRUE, alternative="less")

boxplot(Ozone~Month, data=airquality,col="yellow")
for (i in 5:9){
  ind=(5:9)[-(i-4)]
  x = airquality$Ozone[which(airquality$Month==i)]
  y = airquality$Ozone[which(airquality$Month %in% ind)]
  print(wilcox.test(x,y)$p.value)
}
ind2=c(7,8)
ind3=c(5,6,9)
x = airquality$Ozone[which(airquality$Month %in% ind2)]
y = airquality$Ozone[which(airquality$Month %in% ind3)]
wilcox.test(x,y)

# seminar 10
# seminar 10
# seminar 10
# seminar 10
# seminar 10
# seminar 10
# seminar 10
# seminar 10
# seminar 10
# seminar 10

?kruskal.test
x <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
kruskal.test(list(x, y, z))
boxplot(x,y,z)

?friedman.test

RoundingTimes <- matrix(c(5.40, 5.50, 5.55, 5.85, 5.70, 5.75, 5.20, 5.60, 5.50,
5.55, 5.50, 5.40, 5.90, 5.85, 5.70, 5.45, 5.55, 5.60, 5.40, 5.40, 5.35, 5.45, 5.50, 5.35,
5.25, 5.15, 5.00, 5.85, 5.80, 5.70, 5.25, 5.20, 5.10, 5.65, 5.55, 5.45, 5.60, 5.35, 5.45,
5.05, 5.00, 4.95, 5.50, 5.50, 5.40, 5.45, 5.55, 5.50, 5.55, 5.55, 5.35,
5.45, 5.50, 5.55, 5.50, 5.45, 5.25, 5.65, 5.60, 5.40, 5.70, 5.65, 5.55, 6.30, 6.30, 6.25),
nrow = 22, byrow = TRUE, dimnames = list(1 : 22, c("Round Out", "Narrow Angle", "Wide Angle")))
friedman.test(RoundingTimes)

pdf("Plot1.pdf", width=40, height=100)
plot(1:3, RoundingTimes[1,], type="b", ylim=c(min(RoundingTimes), max(RoundingTimes)))
for (j in 2:22){
  lines(1:3, RoundingTimes[j,], type="b",col=j)
}
dev.off()

install.packages("waveslim")
library(waveslim)
r=function(x){
  sqrt(x*(1-x))*sin(2.1*pi/(x+0.05))
}
plot(r)
curve(r, n=10000)
x1=seq(from=0,to=1,length=10000)
y=sapply(x1,r)
m=mra(y[1:(2^13)], method="dwt", wf="haar", J=13)
plot(m[[11]])
for (j in (13:1)){
  plot(m[[j]],main=j,type="l")
  readline(prompt="press enter")
}

y=rep(0, length(m[[1]]))
for (j in (13:1)){
  y=y+m[[j]]
  plot(y,main=j,type="l")
  readline(prompt="press enter")
}

data(ibm)
plot(ibm)
d=diff(log(ibm))
plot(d)
m=mra(d, wf="haar", J=4,method="dwt")
y=rep(0, length(m[[1]]))
for (j in (4:1)){
  y=y+m[[j]]
  plot(y,main=j,type="l")
  readline(prompt="press enter")
}
lines(d, type="l", col="red")