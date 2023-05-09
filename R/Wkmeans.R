
#Function of k-means for obervations with weight, considering multiple initializations
# dataset is the data matrix,  obs.weights is the weight vector, k is the number of desired clusters,
# cl.centers is chosen initial cluster centers by users. If not NULL, must be a k by ncol(dataset) matrix
# containing only entries of class numeric.
# max.iterations is the maximum number of iterations attempted for convergence before quitting.
# num.init is the number of initial clusters to attempt.
Wkmeans <- function (dataset, k, cl.centers = NULL, obs.weights = rep(1, nrow(dataset)), num.init = 1,
                     max.iterations = 10, seed = 291102) {

  # convert the data set to a data matrix
  dataset = as.matrix(dataset)

  # Argument checking/fixing ####

  # make sure k is of class numeric
  if (!is.numeric(k)){

    stop('k must be of class "numeric"')

  }

  # make sure cl.centers is of proper dimension if it is not NULL
  if (!is.null(cl.centers)){

    if (nrow(cl.centers) != k |
        ncol(cl.centers) != ncol(dataset)){

      stop('cl.centers must be a k by ncol(dataset) numeric matrix')

    }

  }

  # make sure obs.weights is of class numeric or table or integer
  if (!is.numeric(obs.weights)&
      !is.table(obs.weights) &
      !is.integer(obs.weights)){

    stop('obs.weights must be of class "numeric" or "table"')

  }

  # make sure obs.weights is length nrow(dataset)
  if (length(obs.weights) != nrow(dataset)){

    stop(stop("The length of weights should be the same as the number of observations. "))

  }

  # make sure num.init is of class numeric
  if (!is.numeric(num.init)){

    stop('num.init must be of class "numeric"')

  }

  # make sure max.iterations is of class numeric
  if (!is.numeric(max.iterations)){

    stop('max.iterations must be of class "numeric"')

  }


  # make sure seed is numeric
  if (!is.numeric(seed)){

    stop('seed must be of class "numeric"')

  }

  # Function ####

  # set the random seed
  set.seed(seed)

  # convert k, num.init, and max.itertations to integers
  k = floor(k)
  num.init = floor(num.init)
  max.iterations = floor(max.iterations)
  if(!is.null(cl.centers)) num.init = 1

  obs.weights = as.vector(obs.weights)

  # initialize the best set of clusters
  best.clusters = NULL

  # cycle through the initial cluster centers
  #need to do parallel for different initialization!!
  for (U in 1:num.init) {

    if(num.init > 1){message(paste("Initialization ",
                  U, ":", sep = ""))}

    #k-means with observation weights
    result = Wkmeans_single(x = dataset, u = cl.centers, w = obs.weights,k=k,K=max.iterations)

    #get cluster assignments and wwcss
    clusterAss = result[[2]]
    wss = result[[1]]

    #print total wwcss for each initialization
    print(wss)

    # check if current results are better, if so, replace them
    if (is.null(best.clusters) == TRUE) {
      best.clusters = clusterAss
      best.WWCSS.results = wss
    }
    else if (best.WWCSS.results > wss) {
      best.clusters = clusterAss
      best.WWCSS.results = wss
    }
  }

  # Get the Cluster Centers for the best clustering result
  best.cl.centers = t(wmean(x = dataset,cl = best.clusters,w = obs.weights))

  # Get the total wwcss and also the wwcss for each cluster for the best clustering result
  best.WWCSS.results = wwcss(x = dataset, cl =  best.clusters,w = obs.weights,groupSum = TRUE)

  #create output
  output = list(best.clusters, best.cl.centers, best.WWCSS.results)
  # give names to the output
  names(output) = c("Cluster Assignments", "Cluster Centers","Weighted WCSS")

  return(output)
}


#Function to initialize clusters
#x is the data matrix,  w is the weight vector, k is the number of desired clusters,
#u is chosen cluster centers by users. If not NULL, must be a k by ncol(dataset) matrix
#containing only entries of class numeric.
init= function(x,w=rep(1,nrow(x)),k=3, u = NULL){
  clcenter = u
  zz = NULL
  n = nrow(x)
  ff = function(k,n)  sample(c(1:k,sample(k,n-k,replace =TRUE)))
  while(length(unique(zz)) <k){
    if(is.null(u)){
      y = ff(k,n)
      while(min(table(y))<2)  y = ff(k,n)
      clcenter = wmean(x,y,w)}
    z = NULL
    for(i in 1:k)
      z =cbind(z,apply(w*(t(t(x)-clcenter[,i]))^2,1,sum))
    zz=apply(z,1,which.min)
    if(length(unique(zz)) <k & !is.null(u)){
      stop("For the initialization, no data assigned to one(multiple) of clusters based on the weighted
         distance to the initial cluster centers chosen by users.")
    }
  }
  zz
}


#Wkmeans for one initialization
#x is the data matrix, y is cluster assignment vector, w is the weight vector,
#k is the number of desired clusters,
#u is chosen cluster centers by users. If not NULL, must be a k by ncol(dataset) matrix
#containing only entries of class numeric,
#K is number of iterations.
Wkmeans_single = function(x,y,u = NULL, w = rep(1, length(y)),k=3,K=30) {
  if(missing(y)) y = init(x,w,k,u)
  n = length(y)
  #if(missing(K)) K=2^(40/log10(n))
  k = max(y)
  tb= table(y)
  yy=y
  for(l in 1:K) {
    message(paste("Iteration ",
                  l, sep = ""))
    uu0 = uu00 = wwcss(x,yy,w)
    #C code for this part!!!
    for(i in 1:n) { for (j in 1:k){
      y2 = yy;
      if(y2[i]!=j & tb[y2[i]]>1) {
        y2[i]=j
        uu1= wwcss(x, y2,w)
        if(uu1< uu0) {uu0= uu1; yy = y2; tb= table(yy)  }
      }
    }
   ##
    }
    if(uu0>=uu00) break;
  }
  list(uu0,yy)
}
