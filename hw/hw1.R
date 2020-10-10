library(datasets)
data(iris)
summary(iris)
df <- iris[1:4]
head(df)
SummarizeData <- function(x){
  summary.df <- data.frame(x_mean = mean(x, na.rm = TRUE),
                           x_var = var(x, na.rm = TRUE),
                           x_max = max(x, na.rm = TRUE),
                           x_min = min(x, na.rm = TRUE),
                           x_median = median(x, na.rm = TRUE))
  return(summary.df)
}

SummarizeData(iris[,1])
SummarizeData(iris[,2])
SummarizeData(iris[,3])
SummarizeData(iris[,4])

dim(iris)

a <- matrix(0, nrow=150, ncol=150)
for (i in 1:150){
  for (j in 1:150){
    for (k in 1:4){
      a[i,j] <- a[i,j] + (iris[i,k] - iris[j,k])^2
    }
  }
}




