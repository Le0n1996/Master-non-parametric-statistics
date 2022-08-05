N <- 150 #������� 150 ����������, � �� 100
X <- runif(N, min=0, max=1) #���������� �����������
e <- rnorm(N, sd=0.05) #���������� ��������� ���������� ������
Y <- sin(2*X)+e #������ ������

#1,2
k <- 5 #����� ����� ������� �������, ��� ����������
reord <- function(i){
  d <- abs(X-X[i])
  data <- cbind(X, e, d)
  data <- data[order(data[,3]),]
  return(data) #now ordered
}

#3
f5 <- rep(0, N) #������� �������� ������
for(i in (1:N)){
  f5[i] <- mean(sin(2*reord(i)[,1][2:k+1])+reord(i)[,2][2:k+1])
}
data <- cbind(X, f5)
data <- data[order(data[,1]),]
plot(X, Y)
lines(data, col = "green")

#4
err <- f5 - Y #�������� ������ ������
delta <- rep(0, N) #������� �������� ������
for (i in (1:N)){
  if (abs(err[i])<1){ #�� �������� ��� ��������
    delta[i] <- (1-abs(err[i]))**4
  }
}
print(err) # ��������� ��� ����������
print(delta) # ��������� ��� ����������
#��� �� �����, ���������� �������� ������ ������ � 1..

#5
kvar <- round(k/delta)
fk <- rep(0, N) #������� �������� ������
for(i in (1:N)){
  fk[i] <- mean(sin(2*reord(i)[,1][2:kvar[i]+1])+reord(i)[,2][2:kvar[i]+1])
}
data <- cbind(X, fk)
data <- data[order(data[,1]),]
lines(data, col = "blue")

#6
for (step in (1:15)){ #������� �� ������ ������ 15 ���, � �� 10
  err <- fk - Y #�������� ������ ������
  delta <- rep(0, N) #������� �������� ������
  for (i in (1:N)){
    if (abs(err[i])<1){ #�� �������� ��� ��������
      delta[i] <- (1-abs(err[i]))**4
    }
  }
  kvar <- round(k/delta)
  fk <- rep(0, N) #������� �������� ������
  for(i in (1:N)){
    fk[i] <- mean(sin(2*reord(i)[,1][2:kvar[i]+1])+reord(i)[,2][2:kvar[i]+1])
  }
}
data <- cbind(X, fk)
data <- data[order(data[,1]),]
lines(data, col = "red")

#�����: ���� �������� �� MSE, ��
#MSE of estimator with k = 5:
mean((f5 - Y)**2)
#MSE of estimator with k = k(i):
mean((fk - Y)**2)

#��� �� �����, MSE �����������, � ���� ������ ����������