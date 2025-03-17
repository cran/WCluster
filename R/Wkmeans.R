
# Function of k-means clustering for observations with weight, considering multiple initialisations
# dataset is the data matrix, obs.weights is the weight vector, k is the number of desired clusters,
# cl.centers is chosen initial cluster centers by users. If not NULL, must be a k by ncol(dataset) matrix
# containing only entries of class numeric.
# max.iterations is the maximum number of iterations attempted for convergence before quitting.
# num.init is the number of initial clusters to attempt.

Wkmeans <- function (dataset, k, cl.centers = NULL, 
                     obs.weights = rep(1, nrow(dataset)), num.init = 1,
                     max.iterations = 10, seed = 291102) {

  # make sure k is of class numeric
  if (!is.numeric(k)){

    stop('k must be of class "numeric"')

  }

  # make sure cl.centers is of proper dimension if it is not NULL
  if (!is.null(cl.centers)){

    if (nrow(cl.centers) != ncol(dataset) |
        ncol(cl.centers) != k){

      stop('cl.centers must be a ncol(dataset) by k numeric matrix')

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
  
  dataset <- as.matrix(dataset)
  # Function #

  # set the random seed
  set.seed(seed)
  
  # convert k, num.init, and max.iterations to integers
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
    result = Wkmeans_single(x = dataset, u = cl.centers, w = obs.weights,
                            k = k, K = max.iterations)

    #get cluster assignments and wwcss
    clusterAss = result[[2]]
    wwcss_cl = result[[1]]

    #print total wwcss for each initialization
    print(wwcss_cl)

    # check if current results are better, if so, replace them
    if (is.null(best.clusters)) {
      best.clusters = clusterAss
      best.WWCSS.results = wwcss_cl
    }
    else if (best.WWCSS.results > wwcss_cl) {
      best.clusters = clusterAss
      best.WWCSS.results = wwcss_cl
    }
  }

  # Get the Cluster Centers for the best clustering result
  best.cl.centers = t(wmean(x = dataset,cl = best.clusters,w = obs.weights))

  # Get the total wwcss and also the wwcss for each cluster for the best clustering result
  best.WWCSS.results = wwcss(x = dataset, cl =  best.clusters,w = obs.weights,groupSum = TRUE)

  #create output
  output = list(best.clusters, best.cl.centers, best.WWCSS.results)
  # give names to the output
  names(output) = c("Cluster Assignments", "Cluster Centers","WWCSS")

  return(output)
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




# Function to initialize cluster assignments
# x is the data matrix,  w is the weight vector, k is the number of desired clusters,
# u is chosen cluster centers by users. If not NULL, must be a ncol(dataset) by k matrix
# containing only entries of class numeric.

init = function(x, w = rep(1,nrow(x)), k, u = NULL){
  
  clcenter <- u 
  z = NULL ; zz = NULL
  n <- nrow(x) 
  
  # The functions controls for at least one observation per cluster
  ff <- function(k,n)  sample(c(1:k, sample(k, n-k, replace =TRUE)))
  
  while(length(unique(zz)) < k){
    
    if(is.null(u)){y <- ff(k,n) 
      while(min(table(y))<2)  y <- ff(k,n) # if only one observation in any cluster, do reassignment
      
      clcenter <- wmean(x, cl = y, w) # if no user defined cluster centers
    }
    
    z = wdist_mat(x, centers = clcenter, w = w)
    zz = apply(z,1,which.min)
    zz = setNames(zz, row.names(x))
    
    if(length(unique(zz)) <k & !is.null(u)){
      stop("For the initialization, no data assigned to one(or multiple) of the clusters based on the weighted
         distance to the initial cluster centers chosen by users.")
    }
  }
  return (zz)
}




# Wkmeans for one initialization
# x is the data matrix, y is cluster assignment vector, w is the weight vector,
# k is the number of desired clusters,
# u is chosen cluster centers by users. If not NULL, must be a ncol(dataset) by k matrix
# containing only entries of class numeric,
# K is number of iterations.

Wkmeans_single = function(x, y, u = NULL, w = rep(1, length(y)), k, K = 30) {
  
  if(missing(y)) y = init(x, w, k, u)
  n = length(y)
  #if(missing(K)) K=2^(40/log10(n))
  
  k <- max(y) # number of clusters
  tb <- table(y) # cluster sizes
  
  yy = y2 = y
  
  for(l in 1:K) {
    message(paste("Iteration ",
                  l, sep = ""))
    
    uu0 <- uu00 <- wwcss(x, cl = yy, w) # weighted within cluster SS
    
    for(ll in 1:20) {
      #message(paste("iteration ", ll, sep = ""))
      yy0 <- yy
      
      z = NULL
      
      kcl_centers <- wmean(x, cl = yy0, w = w) # cluster centers
      z = wdist_mat(x, centers = kcl_centers, w = w)
      yy = apply(z,1,which.min) # new cluster assignments
    
      uu0 = wwcss(x,cl = yy,w) # new wwcss
      
      #      if( all(yy==yy0)) break
      if(uu0 > uu00) yy = yy0 
      else if(uu0<=uu00) uu00 = uu0
    }

    uu0 = uu00 = wwcss(x,cl = yy, w)
    # message(paste( uu0, sep=" "))
    # local search optimization
    for(i in 1:n){ 
      for (j in 1:k){y2 = yy;
        if(y2[i]!=j & tb[y2[i]]>1) {
          y2[i]=j
          uu1= wwcss(x, y2,w)
          if(uu1<= uu0) {uu0= uu1; yy = y2; tb= table(yy)  }
        }
      }
    }
    if(all(yy0==yy)) break;
  }
  yy = setNames(yy, row.names(x))
  list(uu0,yy)
}

