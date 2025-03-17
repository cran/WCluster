

# hclust with observation weights for Data Nugget object
# datanugget is a "datanugget" object

DN.Whclust <- function(datanugget){

  # Argument checking/fixing ####

  # make sure datanugget is of class "datanugget"
  if (!inherits(datanugget,"datanugget")){

    stop('datanugget must be of class "datanugget"')

  }

  #Get data nuggets centers from the datanugget object
  dataset = datanugget$`Data Nuggets`[,2:(ncol(datanugget$`Data Nuggets`)-2)]

  #get data nugget weights from the datanugget object
  obs.weight = datanugget$`Data Nuggets`[, "Weight"]

  #use function Whclust with input from datanugget object
  output = Whclust(dataset,obs.weight)

  return(output)

}
