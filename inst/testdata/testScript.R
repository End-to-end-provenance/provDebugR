x <- 1
a <- 5
y <- 2
z <- 3 + y
c <- 6

i <- 7 + y
j <- i

x <- 5 + z

y <- "a test"

z <- 6:67

j <- list("another test", 5L, 6.5)

names(j) <- c("name", "length", "value")

df <- data.frame(1:500, 501:1000, 1001:1500)
names(df) <- c("foo", "bar", "foobar")
df1 <- data.frame(c(0, 0, 0, 0), c(1, 3, 5, 8), c("x <- 1", "y <- 2",
  "z <- 3", "cor(c(x, x), c(y, z))"), stringsAsFactors = F)

names(df1) <- c("script", "line", "code")

warning.trace <- function(x, forward = F) {
  return(df1)
}


source("./inst/testdata/test3.R")

y <- 9

#og <- rstudioapi::getActiveDocumentContext()$contents
