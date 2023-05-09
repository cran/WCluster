\name{Wkmeans}
\alias{Wkmeans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
K-means Clustering with observational weights
}
\description{
This function clusters data with observational weights using K-means.
}
\usage{
Wkmeans(dataset,
        k,
        cl.centers = NULL,
        obs.weights = rep(1, nrow(dataset)),
        num.init = 1,
        max.iterations = 10,
        seed = 291102)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{dataset}{
A data matrix (data frame, data table, matrix, etc) containing only entries of class numeric.
}

  \item{k}{
Number of desired clusters. Must be of class numeric or integer.
}

  \item{cl.centers}{
Chosen cluster centers. If NULL (default), random partition initialization with observational weights would be used. If not NULL, must be a k by ncol(dataset) matrix containing only entries of class numeric.
}

  \item{obs.weights}{
Vector of length nrow(dataset) of weights for each observation in the dataset. Must be of class numeric or integer or table. If NULL, the default value is a vector of 1 with length nrow(dataset), i.e., weights equal 1 for all observations.
}

  \item{num.init}{
Number of initial clusters to attempt. Ignored if cl.centers is not NULL. Must be of class numeric or integer.
}

  \item{max.iterations}{
Maximum number of iterations attempted for convergence before quitting. Must be of class numeric or integer.
}

  \item{seed}{
Random seed for replication. Must be of class numeric or integer.
}

}
\details{
K-means clustering with observational weights can be used as an unsupervised learning technique to cluster observations contained in datasets that also have a measure of importance (e.g. weight) associated with them. The objective of the algorithm which performs this method of clustering is to minimize the total weighted within cluster sum of squares (WWCSS) considering observational weights.

In this function, if no chosen initial cluster centers, random partition initialization with observational weights is used. Each point in the data is first randomly assigned to a random cluster ID, and then the weighted cluster centers are calculated considering observational weights. The initial cluster assignments are obtained by choosing the clusters with minimal weighted sum of squares of residuals with respect to the weighted centers.
}

\value{
A list containing the following components:

\item{Cluster Assignments}{Vector of length nrow(dataset) containing the cluster assignment for each observation.}

\item{Cluster Centers}{k by ncol(dataset) matrix containing the weighted cluster centers for each cluster.}

\item{Weighted WCSS}{List containing the individual WWCSS for each cluster and the combined sum of all individual WWCSS's.}

}
\references{
Cherasia, K. E., Cabrera, J., Fernholz, L. T., & Fernholz, R. (2022). Data Nuggets in Supervised Learning. \emph{In Robust and Multivariate Statistical Methods: Festschrift in Honor of David E. Tyler} (pp. 429-449). Cham: Springer International Publishing.

Beavers, T., Cheng, G., Duan, Y., Cabrera, J., Lubomirski, M., Amaratunga, D., Teigler, J. (2023). Data Nuggets: A Method for Reducing Big Data While Preserving Data Structure (Submitted for Publication)
}
\author{
Javier Cabrera, Yajie Duan, Ge Cheng
}
\seealso{
\code{\link{wss}}, \code{\link{wwcss}}, \code{\link{wmean}}
}
\examples{

    require(graphics)

    x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
    colnames(x) <- c("x", "y")

    # assign random weights to observations
    w = sample(1:20,nrow(x),replace = TRUE)

    #k-means with observational weights
    cl = Wkmeans(dataset = x, k = 2, obs.weights = w, num.init = 2)

    plot(x,cex = log(w),pch = 16,col = cl$`Cluster Assignments`)
    points(cl$`Cluster Centers`, col = 1:2, pch = 8, cex = 5)

    #individual WWCSS for each cluster and the combined sum of all individual WWCSS's
    cl$`Weighted WCSS`


    require(cluster)

    # The Ruspini data set from the package "cluster""
    x = as.matrix(ruspini)

    # assign random weights to observations
    w = sample(1:20,nrow(x),replace = TRUE)

    #k-means with observational weights
    cl = Wkmeans(dataset = x, k = 4, obs.weights = w, num.init = 3)

    plot(x,cex = log(w),pch = 16,col = cl$`Cluster Assignments`)
    points(cl$`Cluster Centers`, col = 1:4, pch = 8, cex = 5)

    #individual WWCSS for each cluster and the combined sum of all individual WWCSS's
    cl$`Weighted WCSS`

}
