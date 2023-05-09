
# function producing the hierarchical tree for observations with weights
# by weighted agglomerative hierarchical clustering
# x is the data matrix,  w is the weight vector
Whclust <- function(x,w = rep(1,nrow(x))){

  # convert the data set to a data matrix
  x = as.matrix(x)

  if (length(w) != nrow(x)){

    stop("The length of weights should be the same as the number of observations. ")

  }

  match.call()

  #Observations are treated separately as singleton clusters at the first
  y = -(1:( n<-nrow(x)))
  kk=NULL
  y0= sort(unique(y))
  m=n

  ws2 = rep(NA,n-1)

  # obtain the initial distance matrix for the ward method
  dd= distw(x,y,w)

  # agglomerative hierarchical clustering with observation weights
  for(ii in 1:(n-1)) {

    #find the pair of clusters with minimal distance to merge
    k=which.min(dd)

    # get cluster numbers of the pair with minimal distance to merge
    u=y0[k0<-k%%m]; v = y0[k1 <-k%/%m+if(k0==0) 0 else 1]; if(k0==0) {u=y0[m];k0 <- m}

    kk =rbind(kk,c(u,v))

    #merge the pair of clusters and update cluster numbers
    y[y==u] = y[y==v]=ii
    y0= sort(unique(y))
    m=m-1

    #update the distance matrix between clusters
    if(ii<n-1){
      dd=dd[-c(k0,k1),-c(k0,k1)]
      newdd= cbind(rbind(dd,0),Inf)
      newdd[m, 1:(m-1)] = sapply(1:(m-1),function(t){distwij(y0[t],y0[m],y,x,w)})
      # for (i in 1:(m-1)) {
      #   newdd[m,i] = distwij(y0[i],y0[m],y,x,w)
      # }
      dd=newdd
    }

    #calculate and record the weighted within-cluster sum of squares
    ws2[ii]=wwcss(x,y,w)

  }

  res=list(merge=kk, height =ws2, order =forder(kk), labels=NULL,method="ward.D",call= match.call(),dist.method="euclidean")

  class(res)="hclust"

  res
}


# Takes a merge matrix from Whclust or hclust and produces one order that makes the hierarchical tree plot
# merge: is a merge matrix
forder=  function(merge) {
  u=list(-merge[1,])
  for(i in 2:nrow(merge)) {
    v=merge[i,]
    u[[i]]= c(if(v[1]>0) u[[v[1]]] else -v[1],
          + if(v[2]>0) u[[v[2]]] else -v[2])
    }
  u[[nrow(merge)]]
}
