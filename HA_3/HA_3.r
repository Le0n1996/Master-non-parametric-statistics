?swiss
data=swiss
# N1 (i)
spear_res = rep(0,5) 
kend_res = rep(0,5)
for (i in range(2:6)){
  spear_res[i-1] = cor.test(data[,1], data[,i], method = 'spearman')$p.value
  kend_res[i-1]= cor.test(data[,1], data[,i], method = 'kendall')$p.value
}
spear_res
kend_res
# So, we can see, that H0 (no correlation) should be rejected in each 
# kind of test for all variables except 'Agricultuce' (with alpha=0.05).
# But data has ties and there is a problem for computing this nonparams tests. 

# N1 (ii)
x1=data[,1]
x2=data[,2]

# drop outliers using boxplot method
boxplot(x1)
boxplot(x2) # there are no outliers in Agriculture variable
q=boxplot.stats(x1)$out 
data_clear = data[-which(x1 %in% q),1:2] #drop outliers

cor.test(data_clear[,1], data_clear[,2], method = 'spearman')
cor.test(data_clear[,1], data_clear[,2], method = 'kendall')
# So, H0 could not be rejected in all test when we use data without
# outliers. This result means that the variables have some correlation.

# N1 (iii)
M = data[,2:6]
M = scale(M, center=FALSE,scale=colSums(M)) # scaled params matrix
# find the same provience
for (i in (1:10)){ # the indexes of proviences which we are interested in
  x = M[i,]
  min = 10000
  ind = 1000
  for (j in 11:47){
    m = dist(rbind(x, M[j,]))
    if (m<min){
      min=m
      ind=j
    } 
  }
  res[i] = ind
} 
res # result of searching similar proviences
# test the differences with wilcow test
wilcox.test(data[1:10,1],data[res,1], paired=T, alternative = "two.sided")
# So, according to test, we really find the similar provences.

# N1 (iv)
C_fert = data[which(data[,5]>80), 1]
P_fert = data[which(data[,5]<20), 1]
M_fert = data[which((data[,5]>=20)&(data[,5]<=60)), 1]
# compare densities using the boxplot
boxplot(C_fert, P_fert, M_fert, names=c('Catholic', 'Protestant', 'Mixed'))

# Use kruskal test to compare 3 dictributions
kruskal.test(list(C_fert,P_fert,M_fert))

# Pairwise wilcoxon tests ('two-sided')
wilcox.test(C_fert, P_fert) # H0 is rejected
wilcox.test(C_fert, M_fert) # H0 is rejected on alpha = 0.05 
wilcox.test(P_fert, M_fert)
# for last pair compare other alternatives
wilcox.test(P_fert, M_fert, alternative = 'greater')
wilcox.test(P_fert, M_fert, alternative = 'less')
# So H0 cannot be rejected for each alternatives for this pair

# N1 (v)
# for Catholic
quant_C = quantile(data[which(data[,5]>80), 6], 0.25)
med_C = quantile(data[which(data[,5]>80), 2], 0.5)
C_fert_1 = data[which((data[,5]>80)&
                      (data[,6]<quant_C)&
                      (data[,2]>med_C)), 1]
C_fert_2 = data[which((data[,5]>80)&
                        (data[,6]<quant_C)&
                        (data[,2]<med_C)), 1]
C_fert_3 = data[which((data[,5]>80)&
                        (data[,6]>quant_C)&
                        (data[,2]>med_C)), 1]
C_fert_4 = data[which((data[,5]>80)&
                        (data[,6]>quant_C)&
                        (data[,2]<med_C)), 1]

avg_C = c(mean(C_fert_1),mean(C_fert_2), mean(C_fert_3), mean(C_fert_4))

# for protestant
quant_P = quantile(data[which(data[,5]<20), 6], 0.25)
med_P = quantile(data[which(data[,5]<20), 2], 0.5)
P_fert_1 = data[which((data[,5]<20)&
                        (data[,6]<quant_C)&
                        (data[,2]>med_C)), 1]
P_fert_2 = data[which((data[,5]<20)&
                        (data[,6]<quant_C)&
                        (data[,2]<med_C)), 1]
P_fert_3 = data[which((data[,5]<20)&
                        (data[,6]>quant_C)&
                        (data[,2]>med_C)), 1]
P_fert_4 = data[which((data[,5]<20)&
                        (data[,6]>quant_C)&
                        (data[,2]<med_C)), 1]

avg_P = (c(mean(P_fert_1),mean(P_fert_2), mean(P_fert_3), mean(P_fert_4)))

# mixed group
quant_M = quantile(data[which((data[,5]>=20)&(data[,5]<=60)), 6], 0.25)
med_M = quantile(data[which((data[,5]>=20)&(data[,5]<=60)), 2], 0.5)
M_fert_1 = data[which((data[,5]>=20)&(data[,5]<=60)&
                        (data[,6]<quant_C)&
                        (data[,2]>med_C)), 1]
M_fert_2 = data[which((data[,5]>=20)&(data[,5]<=60)&
                        (data[,6]<quant_C)&
                        (data[,2]<med_C)), 1]
M_fert_3 = data[which((data[,5]>=20)&(data[,5]<=60)&
                        (data[,6]>quant_C)&
                        (data[,2]>med_C)), 1]
M_fert_4 = data[which((data[,5]>=20)&(data[,5]<=60)&
                        (data[,6]>quant_C)&
                        (data[,2]<med_C)), 1]

avg_M = (c(mean(M_fert_1),mean(M_fert_2), mean(M_fert_3), mean(M_fert_4)))
# We will use Friedman test to compare the medians
friedman.test(cbind(avg_C, avg_P, avg_M))
# So H0 cannot be rejected, medians in 
# each groups are the same.
