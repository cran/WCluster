\name{distw}
\alias{distw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distance between clusters based on Ward's method for observations with weights
}
\description{
This function calculates distances between pairs of clusters based on Ward's method for observations with weights. Specifically, for each pair of clusters, it computes the increase of weighted sum of squares after merging them.
}
\usage{
distw(x,cl,w)
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
Based on the Ward method, the distance between two clusters A and B, is the increase of sum of squares after merging them, which is the merging cost of combining two clusters. Specifically, \code{dist(A,B) = SS(A+B) - SS(A) - SS(B)}, where SS(A+B) is sum of squares of residuals with respect to mean considering A and B as one cluster, SS(A) and SS(B) are for the cluster A and B seperately.

Here this function computes the merging costs for each pair of clusters, especially for a data set with observational weights. The sums of squares are calculated with observational weights. The distances of pairs of clusters could be used for agglomerative hierarchical clustering. The pair of clusters with minimal distance could be merged at the next step.
}
\value{
A k by k matrix where k is the number of clusters. The lower triangular part of the matrix contains distances for pairs of clusters based on Ward's method. There are NAs on all the other positions.
}
\references{
Ward Jr, J. H. (1963). Hierarchical grouping to optimize an objective function. \emph{Journal of the American statistical association}, 58(301), 236-244.

Cherasia, K. E., Cabrera, J., Fernholz, L. T., & Fernholz, R. (2022). Data Nuggets in Supervised Learning. \emph{In Robust and Multivariate Statistical Methods: Festschrift in Honor of David E. Tyler} (pp. 429-449). Cham: Springer International Publishing.

Beavers, T., Cheng, G., Duan, Y., Cabrera, J., Lubomirski, M., Amaratunga, D., Teigler, J. (2023). Data Nuggets: A Method for Reducing Big Data While Preserving Data Structure (Submitted for Publication)
}
\author{
Javier Cabrera, Yajie Duan, Ge Cheng
}
\seealso{
\code{\link{Whclust}}
}
\examples{

    library(cluster)
    # The Ruspini data set from the package "cluster""
    x = as.matrix(ruspini)

    # assign random weights to observations
    w = sample(1:10,nrow(x),replace = TRUE)

    # assign random clusters to observations
    cl = sample(1:3,nrow(x),replace = TRUE)

    #output distances between clusters based on Ward's method under the random cluster assignments
    distw(x, cl, w)

}
