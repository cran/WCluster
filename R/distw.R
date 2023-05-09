
# Weighted distance matrix between clusters used in the ward method given by
# the within-cluster sum of squares difference
# x is the data matrix, cl is the cluster assignment vector,  w is the weight vector
distw = function(x,cl,w = rep(1,length(x))) {

  x = as.matrix(x)

  if (length(w) != nrow(x)){

    stop("The length of weights should be the same as the number of observations. ")

  }

  if (length(cl) != nrow(x)){

    stop("The length of cluster assignments should be the same as the number of observations. ")

  }

  #get the number of clusters
  cl0 =sort(unique(cl))


  k=length(cl0)


  dd = array(NA,dim=c(k,k))

  for(i in 2:k ) for(j in 1:(i-1))
    dd[i,j]= distwij(cl0[i],cl0[j],cl,x,w)


  colnames(dd) = rownames(dd) = paste("Cluster", 1:k)

  dd

}

# Weighted distance between two clusters given by the difference of weighted within-cluster
# sum of squares after merging two clusters
# i is first cluster index, j is the second cluster, cl is the cluster number for all observations.
# x is the data matrix,  w is the weight vector
distwij = function(i,j, cl, x,w){

  if (length(w) != nrow(x)){

    stop("The length of weights should be the same as the number of observations. ")

  }

  if (length(cl) != nrow(x)){

    stop("The length of cluster assignments should be the same as the number of observations. ")

  }

  if (i > max(cl) | j > max(cl)){

    stop("The cluster index i or j should be smaller than the number of clusters.")

  }

  l<- (cl==i| cl==j)

  wss(x[l,,drop=F],w[l]) - wwcss(x[l,,drop=F],cl[l],w[l])

}

