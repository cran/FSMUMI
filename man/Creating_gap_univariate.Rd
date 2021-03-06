% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Creating_gap_univariate.R
\name{Creating_gap_univariate}
\alias{Creating_gap_univariate}
\title{Creating a gap in a univariate series}
\usage{
Creating_gap_univariate(X, rate, begin = NULL)
}
\arguments{
\item{X}{input vector}

\item{rate}{size of desired gap, as a percentage of input vector size}

\item{begin}{the begining position of the gap (random by default)}
}
\value{
This function  returns a series with a gap of defined size.
}
\description{
This function creates a gap (successive missing values) within a univariate signal.
The size of gap is defined as a percentage of input vector length.
By default, the starting position of the gap is chosen randomly.
}
\examples{
data(dataFSMUMI)
X <- dataFSMUMI[1:5000, 1] #reduction for demo
rate <- 0.1
incompleted_signal <- Creating_gap_univariate(X, rate)
}
\author{
Thi-Thu-Hong Phan, Andre Bigand, Emilie Poisson-Caillault
}
