\name{DN.Wpca}
\alias{DN.Wpca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted PCA for data nuggets
}
\description{
This function conducts weighted PCA on data nuggets, considering data nugget centers and weights.
}
\usage{
DN.Wpca(datanugget,wcol = NULL, corr = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{datanugget}{An object of class datanugget, i.e., the output of functions \code{create.DN} or \code{refine.DN} in the package \code{datanugget}.
}
  \item{wcol}{Column Weights: Vector of weights for each variable of data nuggets. Must be of class numeric or integer or table. If NULL, column weights are not considered, i.e., weights equal 1 for all columns.
}
  \item{corr}{A logical value indicating whether to use correlation matrix. This is recommended when the column weights are not equal. The default value is FALSE.
}

}
\details{
Data nuggets are a representative sample meant to summarize Big Data by reducing a large dataset to a much smaller dataset by eliminating redundant points while also preserving the peripheries of the dataset. Each data nugget is defined by a center (location), weight (importance), and scale (internal variability). Data nuggets for a large dataset could be created and refined by functions \code{create.DN} or \code{refine.DN} in the package \code{datanugget}. Based on data nugget centers and weights, this function conducts weighted PCA by eigen method for data nugget centers with nugget weights as observational weights. Variable weights could also be included and considered in this function. Correlation matrix is recommended to use when the column weights are not equal.
}
\value{
A list containing the following components:

\item{sdev}{the standard deviations of the weighted principal components (i.e., the square roots of the eigenvalues of the weighted covariance/correlation matrix).
}

\item{rotation}{The matrix of the loading vectors for each of the weighted prinicipal components.}

\item{x}{The weighted prinicipal components.}

\item{center, scale}{the weighted centering and scaling used.}

\item{wrow, wcol}{row weights and column weights used.}

}
\references{
Amaratunga, D., & Cabrera, J. (2009). Exploration and analysis of DNA microarray and protein array data. \emph{John Wiley & Sons} (Vol. 605).

Cherasia, K. E., Cabrera, J., Fernholz, L. T., & Fernholz, R. (2022). Data Nuggets in Supervised Learning. \emph{In Robust and Multivariate Statistical Methods: Festschrift in Honor of David E. Tyler} (pp. 429-449). Cham: Springer International Publishing.

Beavers, T., Cheng, G., Duan, Y., Cabrera, J., Lubomirski, M., Amaratunga, D., Teigler, J. (2023). Data Nuggets: A Method for Reducing Big Data While Preserving Data Structure (Submitted for Publication)
}
\author{
Yajie Duan, Javier Cabrera, Ge Cheng
}
\seealso{
\code{\link[datanugget]{datanugget-package}}, \code{\link[datanugget]{create.DN}}, \code{\link[datanugget]{refine.DN}}, \code{\link{Wpca}}
}

\examples{

      require(datanugget)

      ## small example
      X = cbind.data.frame(rnorm(10^3),
                           rnorm(10^3),
                           rnorm(10^3))

      suppressMessages({

        my.DN = create.DN(x = X,
                          R = 500,
                          delete.percent = .1,
                          DN.num1 = 500,
                          DN.num2 = 250,
                          no.cores = 0,
                          make.pbs = FALSE)

        my.DN.PCA.info = DN.Wpca(my.DN)

      })

      my.DN.PCA.info$sdev
      my.DN.PCA.info$rotation
      my.DN.PCA.info$x

}


