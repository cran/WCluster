

#predict clusters for new data points or new data nuggets given cluster assignments for data nuggets
#datanugget is a "datanugget" object, cl is the cluster assignment vector for the nuggets
#newx is either a data frame of new data points or a new datanugget object
DNcluster.predict <- function(datanugget, cl, newx){

  # Argument checking/fixing ####

  # make sure datanugget is of class "datanugget"
  if (!inherits(datanugget,"datanugget")){

    stop('datanugget must be of class "datanugget"')

  }

  if (length(cl) != nrow(datanugget$`Data Nuggets`)){

    stop("The length of cluster assignments should be the same as the number of data nuggets. ")

  }

  if(!is.data.frame(newx) & !inherits(newx,"datanugget")){

    stop("newdata should be either a data frame or a datanugget object.")

  }

  #if newx is a datanugget object, get the data nugget centers from it
  if(inherits(newx,"datanugget")){
    newx = newx$`Data Nuggets`[,2:(ncol(datanugget$`Data Nuggets`)-2)]
  }

  #Get original data nuggets centers from the raw datanugget object
  x = datanugget$`Data Nuggets`[,2:(ncol(datanugget$`Data Nuggets`)-2)]

  #get data nugget weights from the raw datanugget object
  w = datanugget$`Data Nuggets`[, "Weight"]

  #use function cluster.predict to get cluster prediction
  clnew = cluster.predict(x,w,cl,newx)

  #output the new cluster assginmens for new data points or new data nuggets
  clnew

}
