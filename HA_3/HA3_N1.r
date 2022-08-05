#N1

?swiss
swiss

y <- swiss$Fertility
x1 <- swiss$Agriculture
x2 <- swiss$Examination
x3 <- swiss$Education
x4 <- swiss$Catholic
x5 <- swiss$Infant.Mortality

X <- cbind(x1, x2, x3, x4, x5)

#i

res=rep(0,5)

for (i in (1:5)){
  s=cor.test(X[,i],y, method="spearman")
  k=cor.test(X[,i],y, method="kendall")
  res[i]=paste("x", i, "spearman estimate:",
                 toString(round(s$estimate,2)),
               " p-value:",
                 toString(round(s$p.value,2)),
                 "kendall estimate:",
                 toString(round(k$estimate,2)),
                 "p-value:",
                 toString(round(k$p.value,2)))
}

res

#ii

boxplot(y, x1)

M <- cbind(y, X)

q=boxplot.stats(y)$out
M=M[-which(y %in% q),]

s=cor.test(M[,2],M[,1], method="spearman")
k=cor.test(M[,2],M[,1], method="kendall")
paste("spearman estimate:",
             toString(round(s$estimate,2)),
             " p-value:",
             toString(round(s$p.value,2)),
             "kendall estimate:",
             toString(round(k$estimate,2)),
             "p-value:",
             toString(round(k$p.value,2)))

plot(y, x1)

#iii

XS=scale(X)
s=rep(0,length(y) - 10)
y_res=rep(0,10)
for (i in 1:10){
  x=XS[i,]
  for(j in 11:47){
    s[j - 10]=dist(rbind(x,XS[j,]))
  }
  y_res[i]=y[which.min(s)]
}

plot(y_res,y[1:10])
wilcox.test(y_res,y[1:10],paired = T, alternative = "two.sided")

#iv

