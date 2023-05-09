\name{wwcss}
\alias{wwcss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted Within Cluster Sum of Squares
}
\description{
This function computes the weighted within cluster sum of squares (WWCSS) for a set of cluster assignments provided to a dataset with observational weights.
}
\usage{
wwcss(x, cl, w = rep(1,length(x)), groupSum = FALSE)
}
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

  \item{groupSum}{
A logical value indicating whether the weighted within-cluster sum of squres (WWCSS) of each cluster should be returned. If \code{TRUE} the total WWCSS and WWCSS for each cluster are returned. If \code{FALSE} (the default) only the total WWCSS is returned.
  }
}
\details{
This function is used to evaluate clustering results for observations with weights, and also used for optimizing the cluster assignments in the Wkmeans function.
}
\value{
A list containing the following components:

\item{WWCSS}{If requested by \code{groupSum}, vector of individual WWCSS's for each cluster}

\item{TotalWWCSS}{Combined sum of all individual WWCSS's.}
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

    #output the total WWCSS and WWCSS for each cluster for the cluster assignments
    wwcss(x, cl, w, groupSum = TRUE)

}
