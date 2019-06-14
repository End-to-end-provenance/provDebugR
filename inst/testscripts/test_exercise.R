# test exercise

a <- x
b <- log(a)
c <- 4 + b
d <- a - c
e <- d * 2
f <- "3"
g <- as.integer(e)
h <- f + e

row1 <- c(a, b, c, d)
row2 <- c(e, f, g, h)

df <- data.frame(row1, row2, stringsAsFactors = F)

df

data <- mtcars
