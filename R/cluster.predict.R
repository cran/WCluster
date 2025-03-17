
# predict clusters for new data given cluster assignments for raw data
# x is the raw data matrix, cl is the cluster assignment vector,  w is the weight vector
# newx is a data frame of new data

cluster.predict = function(x, w = rep(1,nrow(x)), cl, newx){

  x = as.matrix(x)

  if (length(w) != nrow(x)){

    stop("The length of weights should be the same as the number of observations. ")

  }

  if (length(cl) != nrow(x)){

    stop("The length of cluster assignments should be the same as the number of observations. ")

  }

  if(is.null(newx) & !is.data.frame(newx)){
    stop("newdata should be a data frame.")
  }

  if(ncol(newx) != ncol(x)){
    stop("newdata should have the same variables as the learning dataset.")
  }

  #get the weighted centers for each cluster
  clcenter = wmean(x, cl, w)

  #calculate distance of new data to each weighted cluster center
  z = NULL
  z = wdist_mat(x = newx, centers = clcenter, w = rep(1, nrow(newx))) 
  
  #choose the cluster with the minimal distance
  clnew = apply(z, 1, which.min)

  #output the cluster assignments for new data
  clnew

}

# Function to compute the weighted distance matrix of the observations from the cluster centers
# x is the data matrix, centers are the cluster centers, w is the weight vector

wdist_mat <- function(x, centers, w){
  
  x <- as.matrix(x)
  k <- ncol(centers)
  w_mat <- diag(w)
  
  m11 <- array(x^2 %*% rep(1,ncol(x)), dim = c(nrow(x),k))
  m22 <- matrix(rep(1,ncol(x))%*% centers^2, nrow = nrow(x), k, byrow = TRUE)
  m12 <- x%*%centers
  
  dist_mat <- m11 + m22 - 2 * m12
  return (w_mat %*% dist_mat)
}
