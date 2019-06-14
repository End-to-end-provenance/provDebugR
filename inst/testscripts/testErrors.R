x <- 1
a <- 5
y <- 2
z <- 3 + y
c <- 6

i <- 7 + y
j <- i

x <- 5 + z

df1 <- data.frame(c(0,0,0,0), c(1,3,5,8), c("x <- 1", "y <- 2", "z <- 3", "cor(c(x, x), c(y, z))"), stringsAsFactors = F)
names(df1) <- c("script", "line", "code")

warning.trace <- function(x, forward = F) {return(df1)}

source("test3.R")

write.csv(df1, "df1.csv")

warning("this is a test")
warning("this is also a test")

x <- b + y
