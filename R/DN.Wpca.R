

# Weighted PCA for Data Nugget object
# datanugget is a "datanugget" object
# wcol is column weights.
#corr indicates whether to use correlation matrix or not.


DN.Wpca=function (datanugget, wcol = NULL, corr = FALSE)
{
  # Argument checking/fixing ####

  # make sure datanugget is of class "datanugget"
  if (!inherits(datanugget,"datanugget")){

    stop('datanugget must be of class "datanugget"')

  }

  #Get data nuggets centers from the datanugget object
  dataset = datanugget$`Data Nuggets`[,2:(ncol(datanugget$`Data Nuggets`)-2)]

  #get data nugget weights from the datanugget object
  obs.weight = datanugget$`Data Nuggets`[, "Weight"]


  if(is.null(wcol)){wcol = rep(1, ncol(dataset))}

  #use function Wpca with input from datanugget object
  output = Wpca(as.matrix(dataset), wrow = obs.weight, wcol = wcol, corr = corr)

  return(output)

}

