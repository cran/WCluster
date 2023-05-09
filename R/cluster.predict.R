
#predict clusters for new data given cluster assignments for raw data
#x is the raw data matrix, cl is the cluster assignment vector,  w is the weight vector
#newx is a data frame of new data
cluster.predict = function(x,w = rep(1,nrow(x)),cl,newx){

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

  #get the number of clusters
  k = max(cl)

  #get the weighted centers for each cluster
  clcenter = wmean(x,cl,w)

  #calculate distances for new data to each weighted cluster center
  z = NULL

  for(i in 1:k)
     z =cbind(z,apply((t(t(newx)-clcenter[,i]))^2,1,sum))

  #choose the cluster with the minimal distance
  clnew=apply(z,1,which.min)

  #output the cluster assignments for new data
  clnew

}


