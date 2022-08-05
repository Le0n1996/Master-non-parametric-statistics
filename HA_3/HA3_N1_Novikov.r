fert <- swiss[,1]
agr <- swiss[,2]
exam <- swiss[,3]
educ <- swiss[,4]
relig <- swiss[,5]
mort <- swiss[,6]

features <- cbind(agr, exam, educ, relig, mort)
features_names <- c("Agriculure:      ", "Examination:     ", "Education:       ", "Religion:        ", "Infant mortality:")

#(i)

res <- rep(0,5)
for (i in (1:5)){
  spear = cor.test(features[,i], fert, method = "spearman")
  kend = cor.test(features[,i], fert, method = "kendall")
  res[i] = paste(features_names[i], 
        "correlation (spearman)", toString(round(spear$estimate, 2)),
        " p-value:", toString(round(spear$p.value, 2)),
        "correlation (kendall)", toString(round(kend$estimate, 2)),
        "p-value:", toString(round(kend$p.value, 2)))
  }

res # ������������ ���������� ���� ��������� c fertility

#(ii)

boxplot(fert, agr) # ��������� � "�����"
# ��� ����� ������, ���� ��������� �������� �� fertility, �� �������� agriculture �������� ���.

M <- cbind(fert, features)

q <- boxplot.stats(fert)$out 
M <- M[-which(fert %in% q),] # ����������� �� ��������

spear = cor.test(M[,2], M[,1], method = "spearman")
kend = cor.test(M[,2], M[,1], method = "kendall")
paste("Spearman correlation:", toString(round(spear$estimate, 2)),
      " p-value:", toString(round(spear$p.value, 2)),
      "Kendall correlation:", toString(round(kend$estimate, 2)),
      "p-value:",toString(round(kend$p.value, 2)))

plot(fert, agr)
# ���� ���������� ����� ���������� � ����, ��� ��������, � ���� �� 
# � ����� ������ p-value ������ 0.1. ������� ���������� ��������� ��������.

# ��� �� �����, ���������� ����� �������� �������� ������� �����������.
# ����������� ���������� - ���� �������� ����������� ���� ����������, � �� ������ ������ ���������,
# ������� ���������� ����� ��������� ���������� �� ����������� ����� �� ���� ������.
# ��� ��� ������� �������� ������ �� ������� ��������, �������� ���������� �� ��� �� ������
# ������ ������ �� ���������, ������� ����� �� �� ������, ���������� �������������. ����� � �������.

#(iii)

scaled_feat <- scale(features)
dst <- rep(0, 37)
res <- rep(0,10)
for (i in 1:10){
  x = scaled_feat[i,]
  for(j in 1:37){
    dst[j] = dist(rbind(x, scaled_feat[j+10,]))
  }
  res[i] = fert[which.min(dst)]
}

plot(res, fert[1:10])
wilcox.test(res, fert[1:10], alternative = "two.sided", paired = TRUE)
# p-value ������ ��� 0.1, �� ���� ���� �� 10%-��� ������ ���������� ������� �������� � ���,
# ��� �������� fertility � 10 ����� ������� ��������� ���������, �� �����������.

#(iv)

C <- swiss[which(relig > 80),]
P <- swiss[which(relig < 20),]
M <- swiss[-which(relig > 80),]
M <- M[-which(M[,5] < 20),]

kruskal.test(list(C[,1], P[,1], M[,1])) # ��� ���������� (>2) �������

# ����� ����������
wilcox.test(C[,1], P[,1]) # H0 ���������
wilcox.test(C[,1], M[,1]) # H0 ��������� �� ������ ���������� 5%
wilcox.test(P[,1], M[,1]) # ����� ������� p-��������!!
# ���������, ����� �� ��������� ������������ �������:
wilcox.test(P[,1], M[,1], alternative = 'greater') # p-value ������
wilcox.test(P[,1], M[,1], alternative = 'less') # p-value ������� 0.9
# ��� ���� ���� ������� �������� �� ����������� ���� ��� ������ �����������

#(v)

mort_1st_quart_C <- quantile(swiss[which(relig > 80), 6], 0.25) # ������� �� 0.25-��������
med_agr_C <- quantile(swiss[which(relig > 80), 2], 0.5)
mort_1st_quart_P <- quantile(swiss[which(relig < 20), 6], 0.25) # ������� �� 0.25-��������
med_agr_P <- quantile(swiss[which(relig < 20), 2], 0.5)
mort_1st_quart_M <- quantile(swiss[which((relig>=20)&(relig<=80)), 6], 0.25)
med_agr_M <- quantile(swiss[which((relig>=20)&(relig<=80)), 2], 0.5)

# ��������
C_1 <- swiss[which((relig > 80) & (agr > med_agr_C) & (mort < mort_1st_quart_C)), 1]
C_2 <- swiss[which((relig > 80) & (agr < med_agr_C) & (mort < mort_1st_quart_C)), 1]
C_3 <- swiss[which((relig > 80) & (agr > med_agr_C) & (mort > mort_1st_quart_C)), 1]
C_4 <- swiss[which((relig > 80) & (agr < med_agr_C) & (mort > mort_1st_quart_C)), 1]

# �����������
P_1 <- swiss[which((relig < 20) & (agr > med_agr_P) & (mort < mort_1st_quart_P)), 1]
P_2 <- swiss[which((relig < 20) & (agr < med_agr_P) & (mort < mort_1st_quart_P)), 1]
P_3 <- swiss[which((relig < 20) & (agr > med_agr_P) & (mort > mort_1st_quart_P)), 1]
P_4 <- swiss[which((relig < 20) & (agr < med_agr_P) & (mort > mort_1st_quart_P)), 1]

# ���������� ����
M_1 <- swiss[which((relig>=20)&(relig<=80) & (agr > med_agr_M) & (mort < mort_1st_quart_M)), 1]
M_2 <- swiss[which((relig>=20)&(relig<=80) & (agr < med_agr_M) & (mort < mort_1st_quart_M)), 1]
M_3 <- swiss[which((relig>=20)&(relig<=80) & (agr > med_agr_M) & (mort > mort_1st_quart_M)), 1]
M_4 <- swiss[which((relig>=20)&(relig<=80) & (agr < med_agr_M) & (mort > mort_1st_quart_M)), 1]

avg_C <- c(mean(C_1), mean(C_2), mean(C_3), mean(C_4))
avg_P <- c(mean(P_1), mean(P_2), mean(P_3), mean(P_4))
avg_M <- c(mean(M_1), mean(M_2), mean(M_3), mean(M_4))

avg_C
avg_P
avg_M

friedman.test(cbind(avg_C, avg_P, avg_M))
# ��� ������, ��� ������� �������� �� �� ���������, � ������� ������������� ����� ������� �������
