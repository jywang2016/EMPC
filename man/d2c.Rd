% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d2c.R
\name{d2c}
\alias{d2c}
\title{convert model from continuous to discrete time}
\usage{
d2c(A, B, Ts)
}
\arguments{
\item{A}{State matrix of a discreted ssmodel}

\item{B}{Input matrix of a discreted ssmodel}

\item{Ts}{Timestep}
}
\value{
A continuous state-space model
}
\description{
Takes in state-space model and makes it continuous
}
\examples{
# X(k+1) = A\\*X(k) + B\\*U(k) discreted ssModel
A <- matrix(c(0.51,0.22,0.47,0.78),nrow = 2, byrow = T)
B <- matrix(c(0.61,0.83,0.25,0.39),nrow = 2, byrow = T)
csys <- d2c(A,B,600)
c2d(csys$A,csys$B,600)
c2d(csys$A,csys$B,1200)

}
