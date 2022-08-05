N <- 150 #возьмем 150 наблюдений, а не 100
X <- runif(N, min=0, max=1) #генерируем равномерное
e <- rnorm(N, sd=0.05) #генерируем случайные нормальные ошибки
Y <- sin(2*X)+e #строим модель

#1,2
k <- 5 #будем брать пятерых соседей, как предложено
reord <- function(i){
  d <- abs(X-X[i])
  data <- cbind(X, e, d)
  data <- data[order(data[,3]),]
  return(data) #now ordered
}

#3
f5 <- rep(0, N) #сначала заполним нулями
for(i in (1:N)){
  f5[i] <- mean(sin(2*reord(i)[,1][2:k+1])+reord(i)[,2][2:k+1])
}
data <- cbind(X, f5)
data <- data[order(data[,1]),]
plot(X, Y)
lines(data, col = "green")

#4
err <- f5 - Y #получаем вектор ошибок
delta <- rep(0, N) #сначала заполним нулями
for (i in (1:N)){
  if (abs(err[i])<1){ #не забываем про носитель
    delta[i] <- (1-abs(err[i]))**4
  }
}
print(err) # посмотрим что получилось
print(delta) # посмотрим что получилось
#Как мы видим, полученные значения весьма близки к 1..

#5
kvar <- round(k/delta)
fk <- rep(0, N) #сначала заполним нулями
for(i in (1:N)){
  fk[i] <- mean(sin(2*reord(i)[,1][2:kvar[i]+1])+reord(i)[,2][2:kvar[i]+1])
}
data <- cbind(X, fk)
data <- data[order(data[,1]),]
lines(data, col = "blue")

#6
for (step in (1:15)){ #сделаем на всякий случай 15 раз, а не 10
  err <- fk - Y #получаем вектор ошибок
  delta <- rep(0, N) #сначала заполним нулями
  for (i in (1:N)){
    if (abs(err[i])<1){ #не забываем про носитель
      delta[i] <- (1-abs(err[i]))**4
    }
  }
  kvar <- round(k/delta)
  fk <- rep(0, N) #сначала заполним нулями
  for(i in (1:N)){
    fk[i] <- mean(sin(2*reord(i)[,1][2:kvar[i]+1])+reord(i)[,2][2:kvar[i]+1])
  }
}
data <- cbind(X, fk)
data <- data[order(data[,1]),]
lines(data, col = "red")

#Ответ: Если сравнить по MSE, то
#MSE of estimator with k = 5:
mean((f5 - Y)**2)
#MSE of estimator with k = k(i):
mean((fk - Y)**2)

#Как мы видим, MSE уменьшилось, и наша оценка улучшилась