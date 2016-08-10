copy1 <- function(vec) {
  vec1 <- c()
  for (i in 1:length(vec)) {
    vec1[i] <- vec[i]
  }
  return(vec1)
}

copy2 <- function(vec) {
  vec1 <- numeric(length(vec))
  for (i in 1:length(vec)) {
    vec1[i] <- vec[i]
  }
  return(vec1)
}

copy3 <- function(vec) {
  vec1 <- sapply(vec, function(i) {j <- i})
  return(vec1)
}

bm1 <- c()
bm2 <- c()
bm3 <- c()

values <- c(1, 1e+2, 1e+3, 1e+4, 1e+5)

for (i in 1:length(values)) {
  vec <- c(1:values[i])
  
  bm1[i] <- median(microbenchmark::microbenchmark(copy1(vec), times = 5)$time)/1000/1000
  bm2[i] <- median(microbenchmark::microbenchmark(copy2(vec), times = 5)$time)/1000/1000
  bm3[i] <- median(microbenchmark::microbenchmark(copy3(vec), times = 5)$time)/1000/1000
}

df <- data.frame(
  length = values,
  copy1 = bm1,
  copy2 = bm2,
  copy3 = bm3
)

plot(df$length, df$copy1, log = "xy", xlab = "vector length", ylab = "ms", col = 1)
lines(df$length, df$copy1, col = 1)
points(df$length, df$copy2, pch = 2, col = 2)
lines(df$length, df$copy2, col = 2)
points(df$length, df$copy3, pch = 3, col = 3)
lines(df$length, df$copy3, col = 3)
legend('topleft', c("copy1 (for-loop ohne init)", "copy2 (for-loop mit init)", "copy3 (sapply)"), lty = 1, bty = "n", y.intersp=2, col = c(1:3))
