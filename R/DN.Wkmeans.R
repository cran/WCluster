
# K-means with observation weights for Data Nugget object
# datanugget is a "datanugget" object
# k is the number of desired clusters,
# cl.centers is chosen initial cluster centers by users. If not NULL, must be a dimension of datanugget centers matrix by k
# containing only entries of class numeric.
# max.iterations is the maximum number of iterations attempted for convergence before quitting.
# num.init is the number of initial clusters to attempt.


DN.Wkmeans <- function(datanugget, k, cl.centers = NULL, num.init = 1,
                       max.iterations = 10, seed = 291102){

  # Argument checking/fixing ##

  # make sure datanugget is of class "datanugget"
  if (!inherits(datanugget,"datanugget")){

    stop('datanugget must be of class "datanugget"')

  }

  #Get data nuggets centers from the datanugget object
  dataset = datanugget$`Data Nuggets`[,2:(ncol(datanugget$`Data Nuggets`)-2)]

  #get data nugget weights from the datanugget object
  obs.weight = datanugget$`Data Nuggets`[, "Weight"]

  #use function Wkmeans with input from datanugget object
  output = Wkmeans(dataset, k, cl.centers, obs.weight, num.init, max.iterations, seed)

  names(output)[1] = "Cluster Assignments for data nuggets"

  #also output the cluster assignments for the original large dataset
  output$`Cluster Assignments for original dataset` = as.numeric(sapply(datanugget$`Data Nugget Assignments`,
                                                                        function(t){output$`Cluster Assignments for data nuggets`[t]}))

  return(output)

}