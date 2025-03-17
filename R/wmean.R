
# Function to get the weighted centers for the clusters
# x is the data matrix, cl is the cluster assignment vector,  w is the weight vector

wmean = function (x, cl, w = rep(1,length(x))) {

  x = as.data.frame(x)

  if (length(w) != nrow(x)){

    stop("The length of weights should be the same as the number of observations. ")

  }

  if (length(cl) != nrow(x)){

    stop("The length of cluster assignments should be the same as the number of observations. ")

  }
  
  #get the number of clusters
  cl1 = as.numeric(factor(cl))

  res = NULL
  
  for(i in 1:max(cl1))
    res = cbind(res,apply(x[cl1==i,,drop=F],2,weighted.mean,w=w[cl1==i]))


  colnames(res) = paste("Cluster Center.",1:max(cl1),sep = "")

  res
}
