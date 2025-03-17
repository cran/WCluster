\name{Wpca}
\alias{Wpca}
\title{
Weighted PCA
}
\description{
This function performs PCA on the given data matrix, with row and column weights.
}
\usage{
Wpca(x, wrow = rep(1, nrow(x)), wcol = rep(1, ncol(x)), corr = FALSE)
}
\arguments{
\item{x}{A data matrix (data frame, data table, matrix, etc) containing only entries of class numeric.}
\item{wrow}{Row Weights: vector of length nrow(x) of weights for each observation in the dataset. Must be of class numeric or integer or table. If NULL, the default value is a vector of 1 with length nrow(x), i.e., weights equal 1 for all observations.
}
\item{wcol}{Column Weights: Vector of length ncol(x) of weights for each variable in the dataset. Must be of class numeric or integer or table. If NULL, the default value is a vector of 1 with length ncol(x), i.e., weights equal 1 for all columns.
}
\item{corr}{A logical value indicating whether to use correlation matrix. This is recommended when the column weights are not equal. The default value is FALSE.
}
}

\details{
PCA with row and column weights is conducted by eigen method.
}
\value{
A list containing the following components:

\item{sdev}{the standard deviations of the weighted principal components (i.e., the square roots of the eigenvalues of the weighted covariance/correlation matrix).
}

\item{rotation}{The matrix of the loading vectors for each of the weighted prinicipal components.}

\item{x}{The weighted principal components.}

\item{center, scale}{the weighted centering and scaling used.}

\item{wrow, wcol}{row weights and column weights used.}
}

\references{
Amaratunga, D., & Cabrera, J. (2009). Exploration and analysis of DNA microarray and protein array data. \emph{John Wiley & Sons} (Vol. 605).

Cherasia, K. E., Cabrera, J., Fernholz, L. T., & Fernholz, R. (2022). Data Nuggets in Supervised Learning. \emph{In Robust and Multivariate Statistical Methods: Festschrift in Honor of David E. Tyler} (pp. 429-449). Cham: Springer International Publishing.

Beavers, T., Cheng, G., Duan, Y., Cabrera, J., Lubomirski, M., Amaratunga, D., Teigler, J. (2023). Data Nuggets: A Method for Reducing Big Data While Preserving Data Structure (Submitted for Publication)
}
\author{
Javier Cabrera, Yajie Duan, Ge Cheng
}
\examples{

    require(cluster)

    # The Ruspini data set from the package "cluster""
    x = as.matrix(ruspini)

    # assign random weights to observations
    w = sample(1:20,nrow(x),replace = TRUE)

    #PCA with observational weights
    res = Wpca(x, wrow = w)

    #weighted principal components
    pc = res$x
    pc

    #loading vectors
    loadings = res$rotation
    loadings
}
