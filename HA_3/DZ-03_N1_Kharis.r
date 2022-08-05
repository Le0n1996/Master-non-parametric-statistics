#N1
?swiss
data=swiss
#(i)
X=cbind(data$Fertility, data$Agriculture, data$Examination, data$Education, data$Catholic, data$Infant.Mortality)
rhos=rep(0, 5)
taus=rep(0, 5)
for (i in 1:5){
  rhos[i]=cor.test(X[,1], X[,i+1], method = 'spearman')$estimate
  taus[i]=cor.test(X[,1], X[,i+1], method = 'kendall')$estimate
}

#(ii)
agriculture=data$Agriculture
M=cbind(fertility, agriculture)
o=boxplot.stats(fertility)$out
o
M=M[-which(fertility %in% o), 1:2]
o=boxplot.stats(agriculture)$out
o
cor.test(M[,1], M[,2], method = 'spearman')$estimate
cor.test(M[,1], M[,2], method = 'kendall')$estimate

#(iii)
X1=scale(X)
res=rep(0, 10)
s=rep(0, 37)
for (i in 1:10){
  x=X1[i,]
  for (j in 11:47){
    s[j-10]=dist(rbind(x, X1[j,]))
  }
  res[i]=X[which.min(s)+10,1]
}
res
fer=X[1:10,1]
wilcox.test(res, fer,paired=TRUE, alternative="greater")
wilcox.test(res, fer,paired=TRUE, alternative="less")

#(iv)

C=X[which(X[,5]>80),]
P=X[which(X[,5]<20),]
M=X[-which(X[,5]>80),]
M=M[-which(M[,5]<20),]

friedman.test(cbind(C[,1], P[,1], M[,1]))
kruskal.test(list(C[,1], P[,1], M[,1]))

friedman.test(cbind(C[,1], P[,1]))
kruskal.test(list(C[,1], P[,1]))

friedman.test(cbind(P[,1], M[,1]))
kruskal.test(list(P[,1], M[,1]))

friedman.test(cbind(C[,1], M[,1]))
kruskal.test(list(C[,1], M[,1]))


#(v)
q=quantile(X[,6])[2]

Cmean=c(mean(C[which(C[,2]>50 & C[,6]>q),1]), mean(C[which(C[,2]<50 & C[,6]<q),1]), 
        mean(C[which(C[,2]>50 & C[,6]<q),1]), mean(C[which(C[,2]<50 & C[,6]>q),1]))

Pmean=c(mean(P[which(P[,2]>50 & P[,6]>q),1]), mean(P[which(P[,2]<50 & P[,6]<q),1]), 
        mean(P[which(P[,2]>50 & P[,6]<q),1]), mean(P[which(P[,2]<50 & P[,6]>q),1]))

Mmean=c(mean(M[which(M[,2]>50 & M[,6]>q),1]), mean(M[which(M[,2]<50 & M[,6]<q),1]), 
        mean(M[which(M[,2]>50 & M[,6]<q),1]), mean(M[which(M[,2]<50 & M[,6]>q),1]))

kruskal.test(list(Cmean, Pmean, Mmean))

