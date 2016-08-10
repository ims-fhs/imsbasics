left_join_col1 <- function(df1, df2) {
  levels <- unique(df1$foreign_key)
  for (i in 1:length(levels)) {
    df1$property_to_add[i] <- df2$property_to_add[df2$key == df1$foreign_key[i]]
  }
  return(df1)
}

left_join_col2 <- function(df1, df2) {
  df2 <- df2[ , c("key", "property_to_add")]
  df <- merge(df1, df2, by.x = "foreign_key", by.y = "key")[, union(names(df1), names(df2))]
  return(df)
}

left_join_col3 <- function(df1, df2) {
  df1$property_to_add <- df2$property_to_add[match(df1$foreign_key, df2$key)]
  return(df1)
}

bm1 <- c()
bm2 <- c()
bm3 <- c()

rep <- c(1, 10, 100)


for (i in 1:length(rep)) {
  df1 <- data.frame(
    key = rep(c(1:5), rep[i]),
    foreign_key = rep(c(1,2,3,2,3), rep[i]),
    stringsAsFactors = F
  )

  df2 <- data.frame(
    key = c(1:3),
    property_to_add = c("a", "b", "a"),
    property_not_to_add = c("x", "y", "z"),
    stringsAsFactors = F
  )

  print(df1) # print them: so R will evaluate them: this corrects the penalty the first run gets..
  print(df2)

  bm1[i] <- median(microbenchmark::microbenchmark(left_join_col1(df1, df2))$time)/1000
  bm2[i] <- median(microbenchmark::microbenchmark(left_join_col2(df1, df2))$time)/1000
  bm3[i] <- median(microbenchmark::microbenchmark(left_join_col3(df1, df2))$time)/1000
}

df <- data.frame(
  length = rep*5,
  merge1 = bm1,
  merge2 = bm2,
  merge3 = bm3
)

plot(df$length, df$merge1, log = "x", xlab = "vector length", ylab = "us", col = 1, ylim = c(0,1200))
lines(df$length, df$merge1, col = 1)
points(df$length, df$merge2, pch = 2, col = 2)
lines(df$length, df$merge2, col = 2)
points(df$length, df$merge3, pch = 3, col = 3)
lines(df$length, df$merge3, col = 3)
legend('topleft', c("merge1 (for-loop)", "merge2 (merge)", "merge3 (match)"), lty = 1, bty = "n", y.intersp = 2, col = c(1:3))
