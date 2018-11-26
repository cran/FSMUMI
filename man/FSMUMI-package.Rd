\name{FSMUMI-package}
\alias{FSMUMI-package}
\alias{FSMUMI}
\docType{package}
\title{
\packageTitle{FSMUMI}
}
\description{
\packageDescription{FSMUMI}
}
\details{
%\packageDESCRIPTION{FSMUMI}
\packageIndices{FSMUMI}
}
\author{
\packageAuthor{FSMUMI}

Maintainer: \packageMaintainer{FSMUMI}
}
\references{
Thi-Thu-Hong Phan, Andre Bigand, and Emilie Poisson Caillault, A New Fuzzy Logic-Based Similarity Measure Applied to Large Gap Imputation for Uncorrelated Multivariate Time Series, Applied Computational Intelligence and Soft Computing, vol. 2018, Article ID 9095683, 15 pages, 2018, <DOI:10.1155/2018/9095683>.}
\keyword{package}
\keyword{imputation}
\keyword{missing data}
\keyword{times series}
\keyword{similarity measure}
\keyword{fuzzy inference system}

\examples{
# Load package dataset
data(dataFSMUMI)
X <- dataFSMUMI[1:3000,]


# Create gaps in multivariate time series
rate=0.01 # Percentage of missing values on a signal
ngaps=1 # Number of gaps on each signal
data <- Creating_gaps(X, rate,ngaps)

#Indexes of gaps
ind=Indexes_size_missing(data);

#Imputation parameters tuning
large_gap_threshold= 30
step_threshold=30
step_finding=10

# Fill gaps using FSMUMI algorithm
results_FSMUMI <- FSMUMImputation(data, large_gap_threshold, step_threshold, step_finding)

# Plot true values (black) and imputation values on the first signal 
Position_of_gap=max(1,ind[[1]][1,1]):min(ind[[1]][1,1]+ind[[1]][1,2])
plot(Position_of_gap,X[Position_of_gap,1], type = "l",ylab="Value")
lines(Position_of_gap,results_FSMUMI[Position_of_gap,1], col = "red", lty = "dashed")

# Compute the similarity between the first imputed signal and the first real signal
compute.sim(X[,1], results_FSMUMI[,1])

# Compute the RMSE  between the first imputed signal and the first real signal
compute.rmse(X[,1], results_FSMUMI[,1])

# Compute the FA2  between the first imputed signal and the first real signal
compute.fa2(X[,1], results_FSMUMI[,1])

# Compute the FSD  between the first imputed signal and the first real signal
compute.fsd(X[,1], results_FSMUMI[,1])

# Compute the FB  between the first imputed signal and the first real signal
compute.fb(X[,1], results_FSMUMI[,1])
}
