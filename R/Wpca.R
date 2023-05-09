
#Function of PCA with row and column weights.
#x the data matrix,  wrow is row weights, wcol is column weights.
#corr indicates whether to use correlation matrix or not.

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

  csum=function (x, n = nrow(x), na.rm = T)
    if (na.rm) {x[is.na(x)] <- 0; c(rep(1,n)%*% x)} else c(rep(1,n)%*% x)
  cwmean= function(x,w) csum(x*w)/sum(w)
  wcol=wcol/sum(wcol)*length(wcol)
  mx=cwmean(x,wrow)

  xc0 = t(t(x)-mx)
  xc = t(t(xc0*sqrt(wrow))*sqrt(wcol))
  xv=t(xc)%*%xc/(sum(wrow)-1)
  if(corr==T) { dd=sqrt(diag(xv));xv= t(xv/dd)/dd}
  xe = eigen(xv)

  res = list(sdev = sqrt(xe$values), rotation = xe$vectors,
             x = xc0%*%(xe$vectors), center = mx, scale = if (corr) dd else 1)
  res$wrow = wrow
  res$wcol = wcol

  #output
  res
}
