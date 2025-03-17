\name{DNcluster.predict}
\alias{DNcluster.predict}
\title{
Predict the closest clusters for a new dataset based on clusters of data nuggets.
}
\description{
Given an object of class datanugget and cluster assignments for data nuggets, this function returns the cluster assignments for new observations or new data nuggets by choosing the closest clusters.
}
\usage{
DNcluster.predict(datanugget, cl, newx)
}
\arguments{

  \item{datanugget}{
An object of class datanugget, i.e., the output of functions \code{create.DN} or \code{refine.DN} in the package \code{datanugget}.
}

  \item{cl}{
Vector of length nrow(datanugget$`Data Nuggets`), i.e., the number of data nuggets, containing cluster assignments for each data nugget. Must be of class integer.
}

  \item{newx}{A new dataset (a data.frame) with same variables as the original large dataset, or a new object of class datanugget with same variables of data nuggets as the learning datanugget object.}

}
\details{
To obtain the cluster assignments for new observations or new data nuggets, the weighted cluster centers are calculated firstly based on data nugget centers and weights with their known cluster assginments. Then, the cluster with the minimal Euclidean distance between new observation or new data nugget center and weighted cluster center is chosen as the closest cluster. In this way, the cluster assignments for all the new observations or new data nuggets are returned. The weights of new data nuggets are not considered here when predicting the cluster assignments.
}
\value{
Vector of length nrow(newx) or number of new data nuggets, containing the cluster assignments for each new observation or new data nugget.
}
\references{
Cherasia, K. E., Cabrera, J., Fernholz, L. T., & Fernholz, R. (2022). Data Nuggets in Supervised Learning. \emph{In Robust and Multivariate Statistical Methods: Festschrift in Honor of David E. Tyler} (pp. 429-449). Cham: Springer International Publishing.

Beavers, T., Cheng, G., Duan, Y., Cabrera, J., Lubomirski, M., Amaratunga, D., Teigler, J. (2023). Data Nuggets: A Method for Reducing Big Data While Preserving Data Structure (Submitted for Publication)
}
\author{
Yajie Duan, Javier Cabrera, Ge Cheng
}
\seealso{
\code{\link{DN.Whclust}}, \code{\link{DN.Wkmeans}}, \code{\link{cluster.predict}}
}
\examples{

      require(datanugget)

      #2-d small example
      X = rbind.data.frame(matrix(rnorm(5*10^3, sd = 0.3), ncol = 2),
                matrix(rnorm(5*10^3, mean = 1, sd = 0.3), ncol = 2))


      #create data nuggets
      my.DN = create.DN(x = X,
                        R = 300,
                        delete.percent = .1,
                        DN.num1 = 300,
                        DN.num2 = 150,
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


      #K-means Clustering for data nuggets
      DN.clus = DN.Wkmeans(datanugget = my.DN2,
                  k = 2,
                  num.init = 1,
                  max.iterations = 5)


      #new observations to predict cluster assignments
      newdata = matrix(rnorm(10^2, mean = 0.5, sd = 0.3), ncol = 2)

      #predict the cluster assignments for the new observations
      DNcluster.predict(my.DN2,
                        cl = DN.clus$`Cluster Assignments for data nuggets`,
                        newx = as.data.frame(newdata))



      #predict cluster assignments for new data nuggets from a new large dataset
      newdata = rbind.data.frame(matrix(rnorm(5*10^3, sd = 0.5), ncol = 2),
                matrix(rnorm(5*10^3, mean = 1, sd = 0.5), ncol = 2))

      #create data nuggets
      my.DN_new = create.DN(x = newdata,
                        R = 300,
                        delete.percent = .1,
                        DN.num1 = 300,
                        DN.num2 = 150,
                        no.cores = 0,
                        make.pbs = FALSE)


      #refine data nuggets
      my.DN2_new = refine.DN(x = newdata,
                         DN = my.DN_new,
                         EV.tol = .9,
                         min.nugget.size = 2,
                         max.splits = 5,
                         no.cores = 0,
                         make.pbs = FALSE)

      #predict the cluster assignments for the new data nuggets
      DNcluster.predict(my.DN2,
                        cl = DN.clus$`Cluster Assignments for data nuggets`,
                        newx = my.DN2_new)

}

