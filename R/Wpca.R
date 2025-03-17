
# Function of PCA with row and column weights.
# x the data matrix,  wrow is row weights, wcol is column weights.
# corr indicates whether to use correlation matrix or not.

Wpca=function (x, wrow = rep(1, nrow(x)), wcol = rep(1, ncol(x)), corr = FALSE)
{
  # convert the data set to a data matrix
  x = as.matrix(x)


  # make sure wrow is of class numeric or table or integer
  if (!is.numeric(wrow) &
      !is.table(wrow) &
      !is.integer(wrow)){

    stop('wrow must be of class "numeric" or "table"')

  }

  # make sure wcol is of class numeric or table or integer
  if (!is.numeric(wcol) &
      !is.table(wcol) &
      !is.integer(wcol)){

    stop('wcol must be of class "numeric" or "table"')

  }

  # make sure wrow is length nrow(x)
  if (length(wrow) != nrow(x)){

    stop(stop("The length of row weights should be the same as the number of observations. "))

  }

  # make sure wrow is length ncol(x)
  if (length(wcol) != ncol(x)){

    stop(stop("The length of column weights should be the same as the number of columns of data. "))

  }

  wrow = as.vector(wrow)
  wcol = as.vector(wcol)

  csum <- function (x, n = nrow(x), na.rm = T)
    if (na.rm) {x[is.na(x)] <- 0; c(rep(1,n)%*% x)} else c(rep(1,n)%*% x)
  cwmean <- function(x,w) csum(x*w)/sum(w)
  wcol <- wcol/sum(wcol)*length(wcol) # normalizing wcol so that sum = ncol(x)
  wm_x <- cwmean(x,wrow) # weighted mean of features

  x_0 <- sweep(x = x, 2, wm_x) # centered data matrix
  x_w <- sweep(sweep(x = x_0, 1, sqrt(wrow), FUN = '*'), 2, sqrt(wcol), FUN = '*') # applying weights
  
  x_wcov <- t(x_w)%*%x_w/(sum(wrow)-1) # weighted covariance matrix
  
  if(corr==T) { dd=sqrt(diag(x_wcov));x_wcov= diag(1/dd) %*% x_wcov %*% diag(1/dd)}
  
  # eigen decomposition
  x_e = eigen(x_wcov)

  res = list(sdev = sqrt(x_e$values), rotation = x_e$vectors,
             x = x_0%*%(x_e$vectors), center = wm_x, scale = if (corr) dd else 1)
  
  res$wrow = wrow
  res$wcol = wcol

  #output
  res
}
