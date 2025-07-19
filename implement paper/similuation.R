#install.packages('rhoR')
library(MASS)
library(rhoR)

simulation <- function(n, p=100){
  sigma1 <- diag(p)
  for(i in 1:p){
    for(j in 1:p){
      if(i < j){
        sigma1[i, j] <- (0.5)^(2 * abs(i - j))
        sigma1[j, i] <- sigma1[i, j]
      }
    }
  }
  x <- mvrnorm(n, mu=rep(0, p), Sigma=sigma1)
  beta <- c(1, -1, 1, -1, 1, -1, rep(0, p - 6))
  
  u <- runif(n)
  
  y <- c()
  
  for(i in 1:n){
    if(u[i] < (1/(exp(-t(x[i, ]) %*% beta)))){
      y[i] <- 1
    }else{
      y[i] <- 0
    }
  }
  data <- cbind(x, y)
  data <- as.data.frame(data)
}
  


set.seed(12345)
data <- simulation(n=900, p=1500)

dim(data)

feature_beni <- data[data$y == 0, -1501]
feature_mali <- data[data$y == 1, -1501]



