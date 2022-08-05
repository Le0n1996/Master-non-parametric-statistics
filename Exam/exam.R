?mtcars
mtcars # почему-то везде было cyl = 6, исправлено
mpg = mtcars$mpg
cyl = c(6,6,4,6,8,6,8,4,4,6,6,8,8,8,8,8,8,4,4,4,4,8,8,8,8,4,4,4,8,6,8,4) # bug??
car = cbind(mpg, cyl)
for (i in 1:9){
  car = cbind(car, mtcars[,i+2])
}
car #OK
# car <- mtcars
with_8_cyl = car[which(car[,2] == 8), 1] # все, у которых 8 цилиндров
with_4_or_6_cyl = car[-which((car[,2] == 8)), 1] # все, у которых не 8
length(with_8_cyl) # есть 14 машин с 8 цилиндрами
length(with_4_or_6_cyl) # и 18 --- с 4 или 6 цилиндрами
boxplot(with_8_cyl, with_4_or_6_cyl, col="yellow")
wilcox.test(with_8_cyl, with_4_or_6_cyl, exact=FALSE, alternative="greater")
# Медианы не совпадают, причем p-значение равно 1

length(with_8_cyl) # для этих 14 машин надо найти максимально похожие среди оставшихся 18
features = cyl
for (i in 1:9){
  features = cbind(features, mtcars[,i+2])
}
scaled <- scale(features)
with_4_or_6_all = car[-which((car[,2] == 8)), ]
not_eight <- scale(with_4_or_6_all)
dst <- rep(0, length(with_4_or_6_cyl))
res <- rep(0, length(with_8_cyl))
for (i in 1:length(with_8_cyl)){
  x = scaled[i,]
  for(j in 1:length(with_4_or_6_cyl)){
    dst[j] = dist(rbind(x, not_eight[j,]))
  }
  res[i] = mpg[which.min(dst)]
}
wilcox.test(res, mpg[1:length(with_8_cyl)], alternative = "two.sided", paired = TRUE)
# p-значение меньше 0.05, поэтому на 5%-ном уровне значимости гипотеза о равенстве
# mpg в группах 1 и 2 уверенно отвергается

#2
mpg <- mtcars$mpg
disp <- mtcars$disp
spanvec = c(0.05, 0.2, 0.5)
bassvec = seq(1, 10)
gr_supsmu <- function(x, y) {
  plot(x, y, xlim = c(min(x), max(x)+(max(x)-min(x))/6))
  for (j in (1:length(spanvec))){
    lines(supsmu(x, y, span=spanvec[j]), col=j)
  }
  legend("topright", legend=spanvec, col=seq(spanvec), lty=1, cex=.75, title = "Span")
}
gr_supsmu(disp, mpg)

reord_y <- function(x,y) { # вспомогательная функция
  x1 <- sort(x)
  y1 <- rep(0, length(y))
  for(i in 1:length(y)) {
    for(j in 1:length(x)) {
      if(x[i] == x1[j]) y1[j] <- y[i]
    }
  }
  return(cbind(x1, y1))
}

all_possible_mse <- function(x,y) {
  reord <- reord_y(x, y)
  x <- reord[,1]
  y <- reord[,2]
  M=cbind(x,y)
  if(sum(duplicated(x)) == 0)
    x <- x
  else {
    M=M[-which(duplicated(x)),]
    x <- M[,1]
    y <- M[,2]
  }
  #M=M[-which(duplicated(M[,2])),]
  m <- rep(0, length(spanvec))
  
  #all possible span
  for (j in (1:length(spanvec))){
    model = supsmu(x, y, span = spanvec[j])
    m[j] = mean((model$y-y)^2)
  }
  
  #all possible bass and span choosed by cross-validation (by default)
  # for (i in (1:length(bassvec))){
  #   model = supsmu(x, y, bass=bassvec[i])
  #   m[length(spanvec)+ i] = mean((model$y-y)^2)
  # }
  mse1 <- m # vector of mse for c(all possible span, all possible bass)
  res1 <- c(min(mse1), which(mse1 == min(mse1)))
  if(res1[2] >= 4) res2 <- c("bass = ", bassvec[res1[2]-3])
  else res2 <- c("span = ", spanvec[res1[2]])
  
  list(mse1, res1, res2)
}

all_possible_mse(disp, mpg)
# Лучшая по MSE модель с разными span -- модель с параметром span = 0.05