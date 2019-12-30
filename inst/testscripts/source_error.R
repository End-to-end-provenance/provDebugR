get.error.df <- function()
{
	col2 <- rep(c("ll",12,13,14,15), 2)
	col3 <- c(21:30)
	return(data.frame(col2, col3, stringsAsFactors=FALSE))
}

df <- cbind(col1, get.error.df(), stringsAsFactors=FALSE)

sum(df, col1)
