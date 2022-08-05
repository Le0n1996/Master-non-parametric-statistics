install.packages("fANCOVA")
library(fANCOVA)
install.packages("np")
library(np)
install.packages("scatterplot3d")
library(scatterplot3d)
LifeCycleSavings
sr <- LifeCycleSavings$sr
pop15 <- LifeCycleSavings$pop15
pop75 <- LifeCycleSavings$pop75
dpi <- LifeCycleSavings$dpi
ddpi <- LifeCycleSavings$ddpi

# i)
spanvec = c(0.05, 0.2, 0.5)
bassvec = seq(1, 10)
#GRAPHICS SUPSMU
gr_supsmu <- function(x, y) {
  plot(x,y, xlim = c(min(x), max(x)+(max(x)-min(x))/6))
  for (j in (1:length(spanvec))){
    lines(supsmu(x, y, span=spanvec[j]),col=j)
  }
  legend("topright", legend=spanvec, col=seq(spanvec),lty=1,cex=.75, title = "Span")
}
gr_supsmu(pop15, sr)

gr_supsmu_bass <- function(x, y) {
  plot(x,y, xlim = c(min(x), max(x)+(max(x)-min(x))/6)) #íà îòäåëüíîì ãðàôèêå
  for (j in (1:length(bassvec))) {
    lines(supsmu(x, y, bass=bassvec[j]), col=j+4)
  }
  legend("topright", legend=bassvec, col=seq(bassvec),lty=1,cex=.6, title = "Bass")
}
gr_supsmu_bass(pop15, sr)

#function reordering y vector according to vector x
#ÂÑÏÎÌÎÃÀÒÅËÜÍÀß ÔÓÍÊÖÈß, ÐÀÍÆÈÐÓÅÒ ÈÃÐÅÊ Â ÑÎÎÒÂÅÒÑÒÂÈÈ Ñ ÐÀÍÆÈÐÎÂÀÍÍÛÌ ÈÊÑÎÌ
reord_y <- function(x,y) {
  x1 <- sort(x)
  y1 <- rep(0, length(y))
  for(i in 1:length(y)) {
    for(j in 1:length(x)) {
      if(x[i] == x1[j]) y1[j] <- y[i]
    }
  }
  return(cbind(x1, y1))
}

all_possible_mse <- function(x,y) { reord <- reord_y(x,y) x <- reord[,1] y <- reord[,2] M=cbind(x,y)
#ÓÁÈÐÀÅÌ ÄÓÁËÈÊÀÒÛ X, ÅÑËÈ ÎÍÈ ÂÄÐÓÃ ÅÑÒÜ, ÏÎÒÎÌÓ ×ÒÎ ÏÎ ÓÌÎË×ÀÍÈÞ SUPSMU ÝÒÎ ÄÅËÀÅÒ if(sum(duplicated(x)) == 0) x <- x else { M=M[-which(duplicated(x)),] x <- M[,1] y <- M[,2] } #M=M[-which(duplicated(M[,2])),] m <- rep(0, (length(bassvec) + length(spanvec))) #all possible span for (j in (1:length(spanvec))){ model = supsmu(x, y, span = spanvec[j]) m[j] = mean((model$y-y)^2) } #all possible bass and span choosed by cross-validation (by default) for (i in (1:length(bassvec))){ model = supsmu(x,y, bass=bassvec[i]) m[length(spanvec)+ i] = mean((model$y-y)^2) } mse1 <- m # vector of mse for c(all possible span, all possible bass) res1 <- c(min(mse1), which(mse1 == min(mse1))) if(res1[2] >= 4) res2 <- c("bass = ", bassvec[res1[2]-3]) else res2 <- c("span = ", spanvec[res1[2]]) list(mse1, res1, res2) } all_possible_mse(pop15, sr)

