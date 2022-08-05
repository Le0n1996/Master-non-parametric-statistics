# 1: Displaying the graph of the function p(x)
p = function(x){
	p <- 0.5*dnorm(x) + 0.25*dchisq(x+3, df=2) + 0.25*dchisq(-x+3, df=2)
	return(p) # definition of function p(x) with dnorm & dchisq
}
S <- seq(-5, 5, by=0.1) # ������ �� ������ � ������ ���� a,b. 
D <- c(rep(0, 100)) # ������ �� �����, � ������� ����� ������������ ��������
for (i in (1:length(S))){ # ��������� � ��������������� ������
	D[i] <- p(S[i]) # ��� ���������� ������ ���������� �������� �������.
}
plot(S, D, type='l', main=paste("Graph of density p(x)"), xlab="x", ylab="p(x)") 
# �� ������ ���. ������, ������� ������������� 0.

# 2: Simulating a sample of length N = 1000
sample <- approx(cumsum(D)/sum(D), S, runif(1000))$y # ����� ��, ������� �� S � D
print(head(sample, n=7)) # ��������� 7 ������ ���������
print(tail(sample, n=7)) # � ��������� 7 ���������

# 3: Constructing the histogram estimator and calculating empirical MISE
res <- hist(sample) # 'Sturges' is a default method
middles <- res$mids # �� ����������� ������� ������
Q <- 10000 # �� �������
S <- seq(-4, 4, by=8/Q) # ����� ������� - 8 - �������� �� Q ������
MISE_emp <- 0 # �������� �� ������
for (i in (1:length(S))){ # ��� ���� Q ��������� �������, ����� ����� ��� ������ � MISE_emp
	p_n <- res$density[which(abs(middles-S[i])==min(abs(middles-S[i])))[1]] 
	p_real <- p(S[i]) 
	MISE_emp <- MISE_emp + (p_n-p_real)**2/Q # ��� ���������� � �����������
} 
print(MISE_emp) # ���������� �������� ������������� MISE
