# Function to calculate the Weighted Sum of Squares
# x is a data matrix or data frame, w is weight vector

wss = function(x, w = rep(1,nrow(x))){

  x = as.data.frame(x)

  if (length(w) != nrow(x)){
    stop("The length of weights should be the same as the number of observations. ")
  }
  
  weighted_mean <- c(t(x)%*%w)/sum(w)
  sum(t(sweep(x, 2, weighted_mean)^2)%*%w)

}

