\name{wmean}
\alias{wmean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cluster Centers for observations with weights
}
\description{
This function computes the weighted cluster centers for a set of cluster assignments provided to a dataset with observational weights.
}
\usage{
wmean(x,cl,w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{
A data matrix (data frame, data table, matrix, etc.) containing only entries of class numeric.
}
  \item{cl}{
Vector of length nrow(x) of cluster assignments for each observation in the dataset, indicating the cluster to which each observation is allocated. Must be of class integer.
}
  \item{w}{
Vector of length nrow(x) of weights for each observation in the dataset. Must be of class numeric or integer. If NULL, the default value is a vector of 1 with length nrow(x), i.e., weights equal 1 for all observations.
}

}
\details{
In this function, the function \code{weighted.mean} in the \code{stats} package is used to calculate the cluster centers for each cluster with observational weights.
}
\value{
A matrix of cluster centres. Each column is a weighted center for one cluster.
}

\references{
Cherasia, K. E., Cabrera, J., Fernholz, L. T., & Fernholz, R. (2022). Data Nuggets in Supervised Learning. \emph{In Robust and Multivariate Statistical Methods: Festschrift in Honor of David E. Tyler} (pp. 429-449). Cham: Springer International Publishing.

Beavers, T., Cheng, G., Duan, Y., Cabrera, J., Lubomirski, M., Amaratunga, D., Teigler, J. (2023). Data Nuggets: A Method for Reducing Big Data While Preserving Data Structure (Submitted for Publication)
}
\author{
Javier Cabrera, Yajie Duan, Ge Cheng
}
\seealso{
\code{\link{Wkmeans}}
}
\examples{

    require(cluster)
    # The Ruspini data set from the package "cluster""
    x = as.matrix(ruspini)

    # assign random weights to observations
    w = sample(1:10,nrow(x),replace = TRUE)

    # assign random clusters to observations
    cl = sample(1:3,nrow(x),replace = TRUE)

    #output the weighted cluster centers for each cluster under the random cluster assignments
    wmean(x, cl, w)

}
