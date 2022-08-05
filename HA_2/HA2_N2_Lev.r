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
all_possible_mse <- function(x,y) {
  reord <- reord_y(x,y)
  x <- reord[,1]
  y <- reord[,2]
  M=cbind(x,y)
  #ÓÁÈÐÀÅÌ ÄÓÁËÈÊÀÒÛ X, ÅÑËÈ ÎÍÈ ÂÄÐÓÃ ÅÑÒÜ, ÏÎÒÎÌÓ ×ÒÎ ÏÎ ÓÌÎË×ÀÍÈÞ SUPSMU ÝÒÎ ÄÅËÀÅÒ
  if(sum(duplicated(x)) == 0) x <- x
  else {
    M=M[-which(duplicated(x)),]
  x <- M[,1]
  y <- M[,2]
  }
  #M=M[-which(duplicated(M[,2])),]
  m <- rep(0, (length(bassvec) + length(spanvec)))
  
  #all possible span
  for (j in (1:length(spanvec))){
    model = supsmu(x, y, span = spanvec[j])
    m[j] = mean((model$y-y)^2)
  }
  
  #all possible bass and span choosed by cross-validation (by default)
  for (i in (1:length(bassvec))){
    model = supsmu(x,y, bass=bassvec[i])
    m[length(spanvec)+ i] = mean((model$y-y)^2)
  }
  mse1 <- m # vector of mse for c(all possible span, all possible bass)
  res1 <- c(min(mse1), which(mse1 == min(mse1)))
  if(res1[2] >= 4) res2 <- c("bass = ", bassvec[res1[2]-3])
  else res2 <- c("span = ", spanvec[res1[2]])
  
  list(mse1, res1, res2)
}

all_possible_mse(pop15, sr)

# ËÓ×ØÀß ÌÎÄÅËÜ ÑÐÅÄÈ ÌÎÄÅËÅÉ Ñ ÐÀÇÍÛÌÈ SPAN È BASS (â ñìûñëå MSE) Ñ ÏÀÐÀÌÅÒÐÎÌ SPAN = 0.05


#ii)
bvec = c("cv.aic", "cv.ls")
kervec = c("epanechnikov","gaussian")
all_possible_mse_kernel <- function(x,y) {
  reord <- reord_y(x,y)
  x <- reord[,1]
  y <- reord[,2]
  
  M=cbind(x,y) #ÓÁÈÐÀÅÌ ÄÓÁËÈÊÀÒÛ X, ÅÑËÈ ÎÍÈ ÂÄÐÓÃ ÅÑÒÜ, ÏÎÒÎÌÓ ×ÒÎ ÏÎ ÓÌÎË×ÀÍÈÞ SUPSMU ÝÒÎ ÄÅËÀÅÒ
  if(sum(duplicated(x)) == 0) x <- x
  else {
    M=M[-which(duplicated(x)),]
    x <- M[,1]
    y <- M[,2]
  }
  res = 1000
  m <- rep(0, 4)
  for (i in (1:2)){
    for (k in (1:2)){
      model=npreg(txdat=x, tydat=y,
                  ckertype=kervec[i],
                  bwmethod=bvec[k])
      if(i ==1) { m[i+k-1] = mean((fitted(model)-y)^2); m1 = m[i+k-1]}
      else { m[i+k] = mean((fitted(model)-y)^2); m1 = m[i+k] }
      if (m1 < res){
        res = m1
        vec = c(i,k)
      }
    }
  }
  list(m, res, vec, c(kervec[vec[1]],bvec[vec[2]]))
}

all_possible_mse_kernel(pop15, sr)

# ËÓ×ØÀß ÌÎÄÅËÜ epanechnikov, aic

best_kernel_model <- function(x,y) {
  reord <- reord_y(x,y)
  x <- reord[,1]
  y <- reord[,2]
  
  M=cbind(x,y) #ÓÁÈÐÀÅÌ ÄÓÁËÈÊÀÒÛ X, ÅÑËÈ ÎÍÈ ÂÄÐÓÃ ÅÑÒÜ, ÏÎÒÎÌÓ ×ÒÎ ÏÎ ÓÌÎË×ÀÍÈÞ SUPSMU ÝÒÎ ÄÅËÀÅÒ
  if(sum(duplicated(x)) == 0) x <- x
  else {
    M=M[-which(duplicated(x)),]
    x <- M[,1]
    y <- M[,2]
  }
  
  mod = all_possible_mse_kernel(x, y)
  model=npreg(txdat=x, tydat=y,
              ckertype=kervec[mod[[3]][1]],
              bwmethod=bvec[mod[[3]][2]])
  plot(x,y)
  yy <- fitted(model)
  points(x, yy, type="l", col="red")
}

best_kernel_model(pop15, sr)


# iii)
#ONLY GRAPHS
main_grafs <- function(x,y) {
  gr_supsmu(x,y)
  gr_supsmu_bass(x,y)
  best_kernel_model(x,y)
}
#main_grafs(pop75,sr)
main_grafs(dpi,sr)
#main_grafs(ddpi,sr)
# ÍÀÏÐÈÌÅÐ, ÄËß DPI

#WITHOUT GRAPHS
main_f <- function(x,y) {
  list(all_possible_mse(x,y), all_possible_mse_kernel(x,y))
}
p15 = main_f(pop15, sr)
p75 = main_f(pop75, sr)
dp = main_f(dpi, sr)
ddp = main_f(ddpi, sr)
res_table <- cbind(pop15 = p15[[1]][[1]], pop75 = p75[[1]][[1]], pdi = dp[[1]][[1]], ddpi = ddp[[1]][[1]])
row.names(res_table) <- c(paste0("Span = ", spanvec[1:3]),
                          paste0("Bass = ", bassvec[1:10]))
res_table1 <- rbind(res_table,
                    min_span = apply(res_table[1:3,], 2, min),
                    min_bass = apply(res_table[4:10,], 2, min),
                    min_span_bass = apply(res_table, 2, min),
                    mean_span_bass = apply(res_table, 2, mean))
res_table1

#ÏÎ ÒÀÁËÈÖÅ ÂÛØÅ ÄËß ÊÀÆÄÎÉ ÏÅÐÅÌÅÍÍÎÉ ÌÎÆÍÎ ÂÛÁÐÀÒÜ ËÓ×ØÓÞ ÌÎÄÅËÜ (Â ÑÌÛÑËÅ MSE), ÂÛÁÈÐÀß ÒÎÒ ÏÀÐÀÌÅÒÐ SPAN ÈËÈ BASS, ÏÐÈ ÊÎÒÎÐÎÌ MSE ÄÎÑÒÈÃÀÅÒ ÌÈÍÈÌÓÌÀ
#ÏÎÑËÅÄÍßß ÑÒÐÎÊÀ ÏÎÇÂÎËßÅÒ ÐÀÑÑÌÎÒÐÅÒÜ ÑÐÅÄÍÈÅ ÇÍÀ×ÅÍÈß ÏÎËÓ×ÅÍÍÛÕ MSE ÂÑÅÕ ÏÅÐÅÌÅÍÍÛÕ, À ÇÍÀ×ÈÒ ÂÛÁÐÀÒÜ ÒÅ ÄÂÅ ÏÅÐÅÌÅÍÍÛÅ, ÄËß ÊÎÒÎÐÛÕ ÝÒÎ ÇÍÀ×ÅÍÈÅ ÍÀÈÌÅÍÜØÅÅ, ÎÄÍÀÊÎ, ÑÒÎÈÒ ÂÊËÞ×ÈÒÜ Â ÐÀÑÑÌÎÒÐÅÍÈÅ ÐÅÇÓËÜÒÀÒÛ ßÄÅÐÍÎÉ ÐÅÃÐÅÑÑÈÈ È ÏÎÄÐÎÁÍÅÅ ÈÇÓ×ÈÒÜ ÃÐÀÔÈÊÈ, ÑÄÅËÀÒÜ ÝÌÏÈÐÈ×ÅÑÊÈÉ ÂÛÂÎÄ
res_table_kernel <- cbind(pop15 = p15[[2]][[1]], pop75 = p75[[2]][[1]], dpi = dp[[2]][[1]], ddpi = ddp[[2]][[1]])
row.names(res_table_kernel) <- c(paste0(kervec[1],", ", bvec[1]),
                                 paste0(kervec[1],", ", bvec[2]),
                                 paste0(kervec[2],", ", bvec[1]),
                                 paste0(kervec[2],", ", bvec[2]))
res_table_kernel1 <- rbind(res_table_kernel,
                           mean_ker = apply(res_table_kernel, 2, mean),
                           min_ker = apply(res_table_kernel, 2, min))
table <- rbind(res_table, res_table_kernel)
rbind(table,
      mean = apply(table, 2, mean),
      min = apply(table, 2, min))
rbind(res_table1, res_table_kernel1)

#ÏÎÑËÅ ÒÙÀÒÅËÜÍÎÃÎ ÈÇÓ×ÅÍÈß ÃÐÀÔÈÊÎÂ ÁÛËÎ ÐÅØÅÍÎ ÂÇßÒÜ Â ÊÀ×ÅÑÒÂÅ ÄÂÓÕ ÏÅÐÅÌÅÍÍÛÕ – POP75 È DPI, ÏÎÑÊÎËÜÊÓ, ÍÀÏÐÈÌÅÐ, ÂÒÎÐÀß ÏÅÐÅÌÅÍÍÀß ÏÎËÓ×ÈËÀ ÍÅÁÎËÜØÈÅ ÇÍÀ×ÅÍÈß ÎØÈÁÊÈ Â ÑËÓ×ÀÅ ßÄÅÐÍÎÉ ÐÅÃÐÅÑÑÈÈ, ÊÎÒÎÐÀß ÁÎËÅÅ ÃËÀÄÊÎ ÎÏÈÑÛÂÀÅÒ ÏÎÂÅÄÅÍÈÅ ÒÎ×ÅÊ Â ÎÒËÈ×ÈÅ ÎÒ SUPSMU, À ÄËß ÃÐÀÔÈÊÎÂ ÏÅÐÅÌÅÍÍÎÉ POP75 ÂÑÅ ÌÎÄÅËÈ ÎÊÀÇÀËÈÑÜ ÊÐÀÉÍÅ ÏÎÕÎÆÈÌÈ, ×ÒÎ ÍÅ ÑÊÀÇÀÒÜ Î ÌÎÄÅËßÕ ÄÐÓÃÈÕ ÏÅÐÅÌÅÍÍÛÕ.


# iv)
x1 <- pop75
x2 <- dpi
y <- sr
M=cbind(x1,x2,y)
if(sum(duplicated(x1)) != 0){
  M=M[-which(duplicated(x1)),]
  x1 <- M[,1]
  x2 <- M[,2]
  y <- M[,3]
}
if(sum(duplicated(x2)) != 0) {
  M=M[-which(duplicated(x2)),]
  x1 <- M[,1]
  x2 <- M[,2]
  y <- M[,3]
}
s2=loess.as(cbind(x1,x2),y,plot=TRUE)
#mean((s2$fitted-y)^2)


# v)
s=scatterplot3d(x1,x2,y,angle=65)
l=lm(y~x1+x2)
s$plane3d(l, col="red")
#mean((l$fitted.values-y)^2)