\name{wss}
\alias{wss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sums of squares of residuals for observations with weights
}
\description{
This function calculates sums of squares of residuals with respect to mean for observations with weights.
}
\usage{
wss(x,w = rep(1,nrow(x)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{
A data matrix (data frame, data table, matrix, etc.) containing only entries of class numeric.
}
  \item{w}{
Vector of length nrow(x) of weights for each observation in the dataset. Must be of class numeric or integer. If NULL, the default value is a vector of 1 with length nrow(x), i.e., weights equal 1 for all observations.
}

}
\details{
In this function, for a dataset with observational weights, the weighted mean for the dataset is calculated first. Based on it, the weighted sum of squares of residuals with respect to the weighted mean is calculated with observational weights. This could be used to calculate weighted within-cluster sum of squares for one cluster of data with observational weights.
}
\value{
a length-one numeric vector.
}
\author{
Javier Cabrera, Yajie Duan, Ge Cheng
}
\examples{

    require(cluster)
    # The Ruspini data set from the package "cluster""
    x = as.matrix(ruspini)

    # assign random weights to observations
    w = sample(1:10,nrow(x),replace = TRUE)

    wss(x,w)

    }


