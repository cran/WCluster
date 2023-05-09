\name{Whclust}
\alias{Whclust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hierarchical Clustering with observational weights
}
\description{
This function produces the hierarchical tree for observations with weights, by agglomerative hierarchical clustering based on Ward's method considering observational weights.
}
\usage{
Whclust(x,w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{
A data matrix (of class matrix, data.frame, or data.table) containing only entries of class numeric.
}

  \item{w}{
Vector of length nrow(x) of weights for each observation in the dataset. Must be of class numeric or integer. If NULL, the default value is a vector of 1 with length nrow(x), i.e., weights equal 1 for all observations.
}

}
\details{
Agglomerative hierarchical clustering based on Ward's method considering observational weights are used to generate the hierarchical tree. Based on the Ward method, the distance between two clusters is the increase of sum of squares after merging them, which is the merging cost of combining two clusters. This function computes the merging costs for each pair of clusters for a data set with observational weights. During the process of agglomerative hierarchical clustering, the sums of squares are calculated with observational weights, and the pair of clusters with minimal distance is merged at each step.
}
\value{
An object of class \emph{hclust} which describes the tree produced by the clustering process. It's the same class of object as outputs from function \code{hclust} in the package \code{stats}. See details in \code{\link{hclust}}. There are \code{\link{print}}, \code{\link{plot}}, and \code{\link{cutree}} methods for \code{hclust} objects.
}
\references{
Cherasia, K. E., Cabrera, J., Fernholz, L. T., & Fernholz, R. (2022). Data Nuggets in Supervised Learning. \emph{In Robust and Multivariate Statistical Methods: Festschrift in Honor of David E. Tyler} (pp. 429-449). Cham: Springer International Publishing.

Beavers, T., Cheng, G., Duan, Y., Cabrera, J., Lubomirski, M., Amaratunga, D., Teigler, J. (2023). Data Nuggets: A Method for Reducing Big Data While Preserving Data Structure (Submitted for Publication)
}
\author{
Javier Cabrera, Yajie Duan, Ge Cheng
}
\seealso{
\code{\link{hclust}}, \code{\link{distw}}
}
\examples{

    require(cluster)
      t1 = Sys.time()

    # The Ruspini data set from the package "cluster""
    x = as.matrix(ruspini)

    # assign random weights to observations
    w = sample(1:20,nrow(x),replace = TRUE)

    # hierarchical clustering with observational weights
    h = Whclust(x,w)

    #print the hclust object
    print(h)

    #plot the hierarchical tree
    plot(h)

    #cut the hierarchical tree to get 4 clusters
    k4 = cutree(h,4)
    table(k4)

    #plot the clustering result
    plot(x,cex = log(w),pch = 16,col = k4)
      t2 = Sys.time()

}
