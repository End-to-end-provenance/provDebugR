# just 1 assignment (no type change)
a <- 1L

# not a variable
plot(1:10)

# no type change
cc <- 2L
cc <- 3L

# container change
d <- 4
d <- as.list(d)

# dimension change
e <- matrix(c(1:100), 4)
e <- matrix(c(1:100), 5)

# type change
f <- 5
f <- as.integer(f)

# multiple type changes in sequence
g <- 6
g <- "six"
g <- TRUE

# multiple type changes, with no type changes
h <- FALSE
h <- TRUE
h <- "seven"
h <- "eight"
h <- 8L
h <- 9L

# special valTypes
# null, environment, function, factor, posixct
s <- NULL
s <- new.env()
s <- function(x) print(x)
s <- factor(c("red","green","blue","red","green","red"))
s <- as.POSIXct("080406 10:11", tz = "UTC", format = "%y%m%d %H:%M")
