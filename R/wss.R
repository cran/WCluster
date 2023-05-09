# Calculate weighted Sums of squares of residuals with respect to mean
# x is a data matrix or data frame, w is weight vector
wss = function(x,w = rep(1,nrow(x))){

  x = as.data.frame(x)

  if (length(w) != nrow(x)){
    stop("The length of weights should be the same as the number of observations. ")
  }
  x = x[order(w),]
  w = w[order(w)]

  sum(((t(x) - c(t(x)%*%w)/sum(w) )^2)%*%w)

}

