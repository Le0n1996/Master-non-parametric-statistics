# 1: Constructing the histogram estimator
level <- LakeHuron
n=length(level)
hist(level, main = paste("Histogram of" , "Lake Huron's level"), xlab = "Level of lake Huron in feet")
# As "Sturges" is a default method for hist()

# 2: Constructing the kernel estimators
K <- eval(formals(density.default)$kernel)
plot(density(level), xlab="")
for (i in (2:length(K))){
  lines(density(level, kernel=K[i]), col=i) # col=i from lection
}
legend("topright", 0.25, legend=K, col=seq(K), lty=1, cex=0.5)
# Kernel's list: like here https://stat.ethz.ch/R-manual/R-devel/library/stats/html/density.html
# About legend: https://stackoverflow.com/questions/5520637/legend-in-base-r-can-fill-refrain-from-drawing-boxes-on-some-lines-can-fill-dr

# 3: Constructing the kernel estimators with different bandwidth parameters h
plot(density(level)) # Default bw='nrd0' would be selected here
h <- c('nrd', 'ucv', 'bcv', 'SJ-ste', 'SJ-dpi') # all possible h
for (i in (2:length(h))){
  lines(density(level, bw=h[i]), col=i) # same as ¹2
}
legend("topright", 0.25, legend=h, col=seq(h), lty=1, cex=0.5)

# 4: Adding pdf of N(0,1)
plot(density(level)) # Default bw='nrd0' would be selected here
h <- c('nrd', 'ucv', 'bcv', 'SJ-ste', 'SJ-dpi') # all possible h
for (i in (2:length(h))){
  lines(density(level, bw=h[i]), col=i) # same as ¹3
}
legend("topright", 0.25, legend=h, col=seq(h), lty=1, cex=0.5)
lines(density(rnorm(10000, mean=mean(level), sd=sd(level))), col=12)
# rnorm method as it was in classes
