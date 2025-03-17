# Function to calculate the Weighted Within-cluster Sum of Squares(WWCSS)
# x is the data matrix, cl is the cluster assignment vector,  w is the weight vector
# groupSum is to indicate whether the output includes the WWCSS for each cluster

wwcss = function (x, cl, w = rep(1,length(x)), groupSum = FALSE) {

  x = as.data.frame(x)

  if (length(w) != nrow(x)){

    stop("The length of weights should be the same as the number of observations. ")

  }

  if (length(cl) != nrow(x)){

    stop("The length of cluster assignments should be the same as the number of observations. ")

  }

  #get the number of clusters
  cl1 = as.numeric(factor(cl))

  #calculate wwcss for each cluster
  output = sapply(1:max(cl1), function(t){wss(x[cl1==t,,drop=F],w=w[cl1==t])})

  names(output) = 1:max(cl1)

  if(groupSum)list(WWCSS = output,TotalWWCSS = sum(output)) else sum(output)

}
