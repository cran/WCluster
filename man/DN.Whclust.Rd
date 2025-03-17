\name{DN.Whclust}
\alias{DN.Whclust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hierarchical Clustering for data nuggets
}
\description{
This function produces the hierarchical tree of data nuggets for an object of class datanugget, by agglomerative hierarchical clustering based on Ward's method considering data nugget centers and weights.
}
\usage{
DN.Whclust(datanugget)
}
\arguments{

  \item{datanugget}{
An object of class datanugget, i.e., the output of functions \code{create.DN} or \code{refine.DN} in the package \code{datanugget}.
}

}
\details{
Data nuggets are a representative sample meant to summarize Big Data by reducing a large dataset to a much smaller dataset by eliminating redundant points while also preserving the peripheries of the dataset. Each data nugget is defined by a center (location), weight (importance), and scale (internal variability). Data nuggets for a large dataset could be created and refined by functions \code{create.DN} or \code{refine.DN} in the package \code{datanugget}.

Based on data nugget centers and weights, this function produces the hierarchical tree of data nuggets for an object of class datanugget, by agglomerative hierarchical clustering for data nugget centers with nugget weights as observational weights. Ward's method is used by computing the merging costs for each pair of clusters, i.e., the increase of sum of squares after merging two clusters. During the process of agglomerative hierarchical clustering, the sums of squares are calculated with data nugget weights, and the pair of clusters with minimal distance is merged at each step.
}
\value{
An object of class \emph{hclust} which describes the tree produced by the clustering process for data nuggets. It's the same class of object as outputs from function \code{hclust} in the package \code{stats}. See details in \code{\link{hclust}}. There are \code{\link{print}}, \code{\link{plot}}, and \code{\link{cutree}} methods for \code{hclust} objects.
}
\references{
Ward Jr, J. H. (1963). Hierarchical grouping to optimize an objective function. \emph{Journal of the American statistical association}, 58(301), 236-244.

Cherasia, K. E., Cabrera, J., Fernholz, L. T., & Fernholz, R. (2022). Data Nuggets in Supervised Learning. \emph{In Robust and Multivariate Statistical Methods: Festschrift in Honor of David E. Tyler} (pp. 429-449). Cham: Springer International Publishing.

Beavers, T., Cheng, G., Duan, Y., Cabrera, J., Lubomirski, M., Amaratunga, D., Teigler, J. (2023). Data Nuggets: A Method for Reducing Big Data While Preserving Data Structure (Submitted for Publication)
}
\author{
Yajie Duan, Javier Cabrera, Ge Cheng
}
\seealso{
\code{\link[datanugget]{datanugget-package}}, \code{\link[datanugget]{create.DN}}, \code{\link[datanugget]{refine.DN}}, \code{\link{Whclust}}, \code{\link{hclust}}, \code{\link{distw}}
}
\examples{

      require(datanugget)

      #2-d small example with visualization
      X = rbind.data.frame(matrix(rnorm(10^3, sd = 0.3), ncol = 2),
                matrix(rnorm(10^3, mean = 1, sd = 0.3), ncol = 2))


      #create data nuggets
      my.DN = create.DN(x = X,
                        R = 100,
                        delete.percent = .1,
                        DN.num1 = 100,
                        DN.num2 = 30,
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

      #plot raw dataset
      plot(X)


      #transform weights to get colors for plot
      w_trans = my.DN2$`Data Nuggets`[, "Weight"]/sum(my.DN2$`Data Nuggets`[, "Weight"])
      w_trans = w_trans/quantile(w_trans,0.8)
      col = sapply(w_trans, function(t){rgb(0,min(t,1),0)})

      #plot refined data nugget centers with weights
      #lighter green means more weights
      plot(my.DN2$`Data Nuggets`[, c("Center1",
                                     "Center2")],col=col,lty = 2,pch=16, cex=0.5)


      #Hierarchical Clustering for data nuggets
      DN.h = DN.Whclust(my.DN2)

      #print the hclust object
      print(DN.h)

      #plot the hierarchical tree
      plot(DN.h)

      #cut the hierarchical tree to get 2 clusters
      k2 = cutree(DN.h,2)
      table(k2)

      #plot the clustering result for data nuggets
      plot(my.DN2$`Data Nuggets`[, c("Center1",
                                     "Center2")], col = k2, lty = 2,pch=16, cex=0.5)

}
