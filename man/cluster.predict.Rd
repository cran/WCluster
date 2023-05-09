\name{cluster.predict}
\alias{cluster.predict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict the closest clusters for a new dataset.
}
\description{
Given observations with weights and cluster assignments, this function returns the cluster assignments for a new dataset by choosing the closest clusters.
}
\usage{
cluster.predict(x,w = rep(1,nrow(x)),cl,newx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{
A data matrix (data frame, data table, matrix, etc.) containing only entries of class numeric.
}
  \item{w}{
Vector of length nrow(x) of weights for each observation in the dataset. Must be of class numeric or integer. If NULL, the default value is a vector of 1 with length nrow(x), i.e., weights equal 1 for all observations.
}
  \item{cl}{
Vector of length nrow(x) of cluster assignments for each observation in the dataset, indicating the cluster to which each observation is allocated. Must be of class integer.
}
  \item{newx}{A new dataset (a data.frame), with the same variables as the learning dataset. Must be of class data.frame.}

}
\details{
To obtain the cluster assignments for a new dataset, the weighted cluster centers are calculated firstly based on observations with weights and known cluster assginments. Then, the cluster with the minimal Euclidean distance between new observation and weighted cluster center is chosen as the closest cluster. In this way, the cluster assignments for all the new observations are returned.
}
\value{
Vector of length nrow(newx) containing the cluster assignments for each observation in the new dataset.
}
\references{
Cherasia, K. E., Cabrera, J., Fernholz, L. T., & Fernholz, R. (2022). Data Nuggets in Supervised Learning. \emph{In Robust and Multivariate Statistical Methods: Festschrift in Honor of David E. Tyler} (pp. 429-449). Cham: Springer International Publishing.

Beavers, T., Cheng, G., Duan, Y., Cabrera, J., Lubomirski, M., Amaratunga, D., Teigler, J. (2023). Data Nuggets: A Method for Reducing Big Data While Preserving Data Structure (Submitted for Publication)
}
\author{
Yajie Duan, Javier Cabrera, Ge Cheng
}
\seealso{
\code{\link{Wkmeans}}
}
\examples{

    require(cluster)

    # The Ruspini data set from the package "cluster""
    data = as.matrix(ruspini)

    #take the first 70 observations for clustering,
    #and the last 5 observations for prediction
    x = data[1:70,]
    test.x = data[71:75,]

    # assign random weights to observations
    w = sample(1:20,nrow(x),replace = TRUE)

    #k-means clustering with observational weights
    cl = Wkmeans(dataset = x, k = 4, obs.weights = w, num.init = 3)

    #predict the cluster assignments for the test data
    cluster.predict(x,w, cl = cl$`Cluster Assignments`,newx = as.data.frame(test.x))


}

