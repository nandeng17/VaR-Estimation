#Portfolio loss under stressed scenario
data1<-read.csv(file.choose(new=FALSE),header = TRUE, sep=",",fill=TRUE,quote = "\"", skipNul = TRUE) #choose file MSFT_AAPL_Log_Returns.csv
data<-as.data.frame(data1[9:nrow(data1),1:ncol(data1)])
#clean data and only save what we want
AAPLReturns <- as.vector(data[,7])
MSFTReturns <- as.vector(data[,6])
AAPLReturns <- as.numeric(AAPLReturns)
MSFTReturns <- as.numeric(MSFTReturns)
wAAPL<-0.562546911
wMSFT<-0.437453089
Vt<-1000000
standard_Means <- c(0,0)
standard_CovMatrix <- matrix(rep(0,times = 4),nrow = 2, ncol = 2)
standard_Means[1] <- mean(AAPLReturns[1:499])  
standard_Means[2] <- mean(MSFTReturns[1:499]) 
Log_Returns <- as.data.frame(cbind(AAPLReturns[1:499],MSFTReturns[1:499]))
standard_CovMatrix <- cov(Log_Returns)
lambda <- 0.97
EWMA_AAPL_mean <- c(0)
EWMA_MSFT_mean <- c(0)
## Notice we update our estimate from 11/9/1, which is the first data point in Return vector 
EWMA_AAPL_mean[1] <- lambda*standard_Means[1] + (1-lambda)*AAPLReturns[500]  #501
EWMA_MSFT_mean[1] <- lambda*standard_Means[2] + (1-lambda)*MSFTReturns[500]  #501
for (i in 2:(length(AAPLReturns)-500+2)) 
{
  EWMA_AAPL_mean[i] <- lambda*EWMA_AAPL_mean[i-1]+(1-lambda)*AAPLReturns[500+i-2] #start from 501 
  EWMA_MSFT_mean[i] <- lambda*EWMA_MSFT_mean[i-1]+(1-lambda)*MSFTReturns[500+i-2] #start from 501
}
EWMA_means <- cbind(EWMA_AAPL_mean,EWMA_MSFT_mean)

# Since it is hard to save matrix time series, we decompose covariance matrix as variances and covariances
EWMA_AAPL_var <- c(0)
EWMA_MSFT_var <- c(0)
EWMA_cov <- c(0)
EWMA_AAPL_var[1] <- lambda*standard_CovMatrix[1,1] + (1-lambda)*(AAPLReturns[499]-standard_Means[1])^2
EWMA_MSFT_var[1] <- lambda*standard_CovMatrix[2,2] + (1-lambda)*(MSFTReturns[499]-standard_Means[2])^2
EWMA_cov[1] <- lambda*standard_CovMatrix[1,2] + (1-lambda)*(AAPLReturns[499]-standard_Means[1])*(MSFTReturns[499]-standard_Means[2])
for (i in 2:(length(AAPLReturns)-500+2)) 
{
  EWMA_AAPL_var[i] <- lambda*EWMA_AAPL_var[i-1] + (1-lambda)*(AAPLReturns[500+i-2]-EWMA_AAPL_mean[i-1])^2
  EWMA_MSFT_var[i] <- lambda*EWMA_MSFT_var[i-1] + (1-lambda)*(MSFTReturns[500+i-2]-EWMA_MSFT_mean[i-1])^2
  EWMA_cov[i] <- lambda*EWMA_cov[i-1] + (1-lambda)*(AAPLReturns[500+i-2]-EWMA_AAPL_mean[i-1])*(MSFTReturns[500+i-2]-EWMA_MSFT_mean[i-1])
}


EWMA_means <- c(EWMA_AAPL_mean[761],EWMA_MSFT_mean[761])
EWMA_Covariance_Matrix <- matrix(data = c(EWMA_AAPL_var[761],EWMA_cov[761],EWMA_cov[761],EWMA_MSFT_var[761]),nrow = 2,ncol = 2)

library(MASS)
EWMA_Simulation_Returns <- mvrnorm(50000,mu = EWMA_means,Sigma = EWMA_Covariance_Matrix)
Simulation_Loss <- -Vt * (wAAPL * (exp(EWMA_Simulation_Returns[,1])-1) + wMSFT * (exp(EWMA_Simulation_Returns[,2])-1))

VaR<- sort(Simulation_Loss)[ceiling(length(Simulation_Loss)*0.95)]
VaR
#Analytical VaR result

K<-20
sqrtVaR<-sqrt(K)*VaR
sqrtVaR
#squre of root VaR result

regulatoryVaR<-3*sqrt(10)*VaR
regulatoryVaR
#regulatory VaR, 3* squreroot of 10 *VaR

#stress testing
#sample from updated matrix
Stress_Loss<-c(0)
for( j in 1:50000)
{
Samplereturn<-matrix(rep(0,times = 40),nrow = 20, ncol = 2)
Samplereturn[1,]<- mvrnorm(1,mu = EWMA_means,Sigma = EWMA_Covariance_Matrix)
#replace the second return with extramely bad performance
sigma2<-sqrt(EWMA_Covariance_Matrix[2,2])

Stress_AAPL_mean <- c(0)
Stress_MSFT_mean <- c(0)
Stress_means<-matrix(rep(0,times = 40),nrow = 20, ncol = 2)

Stress_AAPL_var <- c(0)
Stress_MSFT_var <- c(0)
Stress_cov <- c(0)

Stress_AAPL_mean[1] <- Samplereturn[2]-5*sigma2
Stress_MSFT_mean[1] <- Samplereturn[1]  
Stress_means[1,]<-cbind(Stress_AAPL_mean[1],Stress_MSFT_mean[1])
Stress_AAPL_var[1] <- EWMA_AAPL_var[761]
Stress_MSFT_var[1] <- EWMA_cov[761]
Stress_cov[1] <- EWMA_MSFT_var[761]
for (i in 2:20) 
{
  Stress_AAPL_mean[i] <- lambda*Stress_AAPL_mean[i-1]+(1-lambda)*Samplereturn[i-1,2]
  Stress_MSFT_mean[i] <- lambda*Stress_MSFT_mean[i-1]+(1-lambda)*Samplereturn[i-1,1]  
  Stress_means[i,] <- cbind(Stress_AAPL_mean[i],Stress_MSFT_mean[i])
  Stress_AAPL_var[i] <- lambda*Stress_AAPL_var[i-1] + (1-lambda)*(Samplereturn[i-1,2]-Stress_AAPL_mean[i-1])^2
  Stress_MSFT_var[i] <- lambda*Stress_MSFT_var[i-1] + (1-lambda)*(Samplereturn[i-1,1]-Stress_MSFT_mean[i-1])^2
  Stress_cov[i] <- lambda*Stress_cov[i-1] + (1-lambda)*(Samplereturn[i-1,2]-Stress_AAPL_mean[i-1])*(Samplereturn[i-1,1]-Stress_MSFT_mean[i-1])
  Stress_Covariance_Matrix <- matrix(data = c(Stress_AAPL_var[i],Stress_cov[i],Stress_cov[i],Stress_MSFT_var[i]),nrow = 2,ncol = 2);library("corpcor");Stress_Covariance_Matrix <- make.positive.definite(Stress_Covariance_Matrix, tol=1e-3)
Samplereturn[i,]<- mvrnorm(1,mu = Stress_means[i,],Sigma = Stress_Covariance_Matrix)
}

Stress_Loss[j]<- -Vt * (wAAPL * (exp(sum(Samplereturn[,1]))-1) + wMSFT * (exp(sum(Samplereturn[,2]))-1))
}
Average_K_day_loss<-mean(Stress_Loss)
Average_K_day_loss
StressVaR<- sort(Stress_Loss)[ceiling(length(Stress_Loss)*0.95)]
StressVaR

SqrtVaR<-sqrt(20)*StressVaR

for (j in 1:50000 )
{
	if (SqrtVaR>=StressLoss[j]) {p<-p+0}
	else {p<-p+1}
}

Exceedance_of_Sqrt_VaR<-p

Capital_VaR<-3*sqrt(10)*StressVaR
for (j in 1:50000 )
{
	if (Capital_VaR>=StressLoss[j]) {q<-q+0}
	else {q<-q+1}
}

Exceedance_of_Capital_VaR<-q


