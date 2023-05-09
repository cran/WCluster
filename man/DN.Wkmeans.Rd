\name{DN.Wkmeans}
\alias{DN.Wkmeans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
K-means Clustering for data nuggets
}
\description{
This function clusters data nuggets for an object of class datanugget, using K-means considering data nugget centers and weights.
}
\usage{
DN.Wkmeans(datanugget,
        k,
        cl.centers = NULL,
        num.init = 1,
        max.iterations = 10,
        seed = 291102)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{datanugget}{
An object of class datanugget, i.e., the output of functions \code{create.DN} or \code{refine.DN} in the package \code{datanugget}.
}

  \item{k}{
Number of desired clusters. Must be of class numeric or integer.
}

  \item{cl.centers}{
Chosen cluster centers. If NULL (default), random partition initialization would be used. If not NULL, must be a matrix containing only entries of class numeric with dimensions k by the dimension of data nugget centers.
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
Data nuggets are a representative sample meant to summarize Big Data by reducing a large dataset to a much smaller dataset by eliminating redundant points while also preserving the peripheries of the dataset. Each data nugget is defined by a center (location), weight (importance), and scale (internal variability). Data nuggets for a large dataset could be created and refined by functions \code{create.DN} or \code{refine.DN} in the package \code{datanugget}.

K-means clustering with observation weigths can be used as an unsupervised learning technique to cluster observations contained in datasets that also have a measure of importance (e.g. weight) associated with them. In the case of data nuggets, this is the weight parameter associated with the data nuggets, so the centers of data nuggets are clustered using their weight parameters. The objective of the algorithm which performs this method of clustering is to minimize the weighted within cluster sum of squares (WWCSS) considering data nugget weights.

In this function, if no chosen initial cluster centers for data nuggets, random partition initialization with nugget weights is used. Each data nugget is first randomly assigned to a random cluster ID, and then the weighted cluster centers are calculated considering nugget weights. The initial cluster assignments are obtained by choosing the clusters with minimal weighted sum of squares of residuals with respect to the weighted centers.
}
\value{
A list containing the following components:

\item{Cluster Assignments for data nuggets}{Vector of length nrow(datanugget$`Data Nuggets`), i.e., the number of data nuggets. It contains the cluster assignments for each data nugget.}

\item{Cluster Centers}{k by dimension of data nuggets matrix containing the weighted cluster centers for each cluster.}

\item{Weighted WCSS}{List containing the individual WWCSS for each cluster and the combined sum of all individual WWCSS's.}

\item{Cluster Assignments for original dataset}{Vector of length(datanugget$`Data Nugget Assignments`), i.e., number of observations in the original large dataset. It contains the cluster assignments for each observation in the original large dataset.}

}
\references{
Cherasia, K. E., Cabrera, J., Fernholz, L. T., & Fernholz, R. (2022). Data Nuggets in Supervised Learning. \emph{In Robust and Multivariate Statistical Methods: Festschrift in Honor of David E. Tyler} (pp. 429-449). Cham: Springer International Publishing.

Beavers, T., Cheng, G., Duan, Y., Cabrera, J., Lubomirski, M., Amaratunga, D., Teigler, J. (2023). Data Nuggets: A Method for Reducing Big Data While Preserving Data Structure (Submitted for Publication)
}
\author{
Yajie Duan, Javier Cabrera, Ge Cheng
}
\seealso{
\code{\link[datanugget]{datanugget-package}}, \code{\link[datanugget]{create.DN}}, \code{\link[datanugget]{refine.DN}}, \code{\link{Wkmeans}}, \code{\link{wwcss}}, \code{\link{wss}}
}

\examples{
      require(datanugget)

      #2-d small example with visualization
      X = rbind.data.frame(matrix(rnorm(10^4, sd = 0.3), ncol = 2),
                matrix(rnorm(10^4, mean = 1, sd = 0.3), ncol = 2))


      #create data nuggets
      my.DN = create.DN(x = X,
                        R = 500,
                        delete.percent = .1,
                        DN.num1 = 500,
                        DN.num2 = 250,
                        no.cores = 0,
                        make.pbs = FALSE)


      #refine data nuggets
      my.DN2 = refine.DN(x = X,
                         DN = my.DN,
                         EV.tol = .9,
                         min.nugget.size = 2,
                         max.splits = 5,
                         no.cores = 0,
                         make.pbs = FALSE)

      #plot raw large dataset
      plot(X)


      #transform weights to get colors for plot
      w_trans = my.DN2$`Data Nuggets`[, "Weight"]/sum(my.DN2$`Data Nuggets`[, "Weight"])
      w_trans = w_trans/quantile(w_trans,0.8)
      col = sapply(w_trans, function(t){rgb(0,min(t,1),0)})

      #plot refined data nugget centers with weights
      #lighter green means more weights
      plot(my.DN2$`Data Nuggets`[, c("Center1",
                                     "Center2")],col=col,lty = 2,pch=16, cex=0.5)



      #K-means Clustering for data nuggets
      DN.clus = DN.Wkmeans(datanugget = my.DN2,
                  k = 2,
                  num.init = 1,
                  max.iterations = 5)


      DN.clus$`Cluster Centers`
      DN.clus$`Weighted WCSS`


      #plot the clustering result for data nuggets
      plot(my.DN2$`Data Nuggets`[, c("Center1",
                                     "Center2")],
          col = DN.clus$`Cluster Assignments for data nuggets`, lty = 2,pch=16, cex=0.5)
      points(DN.clus$`Cluster Centers`, col = 1:2, pch = 8, cex = 5)

      #plot the clustering result for raw large dataset
      plot(X, col = DN.clus$`Cluster Assignments for original dataset`)


}


