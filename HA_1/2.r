# 1: Displaying the graph of the function p(x)
p = function(x){
	p <- 0.5*dnorm(x) + 0.25*dchisq(x+3, df=2) + 0.25*dchisq(-x+3, df=2)
	return(p) # definition of function p(x) with dnorm & dchisq
}
S <- seq(-5, 5, by=0.1) # задаем ее только в точках вида a,b. 
D <- c(rep(0, 100)) # вектор из нулей, в который потом записываются значения
for (i in (1:length(S))){ # плотности в соответствующих точках
	D[i] <- p(S[i]) # Это достаточно хорошо приближает реальную картину.
}
plot(S, D, type='l', main=paste("Graph of density p(x)"), xlab="x", ylab="p(x)") 
# Не путаем оси. Кстати, среднее действительно 0.

# 2: Simulating a sample of length N = 1000
sample <- approx(cumsum(D)/sum(D), S, runif(1000))$y # опять же, создаем по S и D
print(head(sample, n=7)) # посмотрим 7 первых элементов
print(tail(sample, n=7)) # и посмотрим 7 последних

# 3: Constructing the histogram estimator and calculating empirical MISE
res <- hist(sample) # 'Sturges' is a default method
middles <- res$mids # из гистограммы находим центры
Q <- 10000 # по условию
S <- seq(-4, 4, by=8/Q) # длина отрезка - 8 - дробится на Q частей
MISE_emp <- 0 # значение на старте
for (i in (1:length(S))){ # для всех Q элементов смотрим, какой вклад они вносят в MISE_emp
	p_n <- res$density[which(abs(middles-S[i])==min(abs(middles-S[i])))[1]] 
	p_real <- p(S[i]) 
	MISE_emp <- MISE_emp + (p_n-p_real)**2/Q # Все складываем и накапливаем
} 
print(MISE_emp) # полученная величина эмпирического MISE
