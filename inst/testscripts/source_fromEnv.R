d <- 10
vector.1 <- c(a:d)
print(vector.1)

e <- a + 10
f <- d + 10
vector.2 <- c(e:f)
print(vector.2)

# create a plot
X11()
plot(vector.1, vector.2)
title("Plot 1")
dev.off()

# create another plot, end using dev.off
pdf("plot.pdf")
vector.3 <- vector.1 + 20L
vector.4 <- vector.2 + 30L
plot(vector.3, vector.4)
title("pdf plot")
dev.off()

# use b so that there's a forward lineage that does not end in a plot
g <- b + 50
print(g)

# a line that isn't related to anything else
print("End of script!")
