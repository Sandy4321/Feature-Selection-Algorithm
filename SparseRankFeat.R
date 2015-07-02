SparseRankFeat <- function(dataset, a = 1e-1, prop = c(1,1,1)) {
  n <- nrow(dataset)
  p <- ncol(dataset)
  z <- apply(dataset, 2, function(x) sum(x==0))
  smallVal <- apply(dataset, 2, function(x) sum(abs(x) < a * max(x)/sqrt(p)))
  largeVal <- apply(dataset, 2, function(x) sum(abs(x[x!=0]) > (1-a) * max(x)/sqrt(p)))
  lowSd <- apply(dataset, 2, function(x) sd(x[x!=0])) < a * apply(dataset, 2, max) / (sqrt(p)*(n - z))
  score <- prop[1] * smallVal * z + prop[2] * largeVal * z + prop[3] * lowSd * (n - z)^2
  return(order(score))
}