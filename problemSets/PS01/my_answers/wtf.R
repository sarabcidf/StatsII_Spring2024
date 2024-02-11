
rm(list=ls())

set.seed (123) # Setting my seed 
data <- data.frame(x = runif(200, 1, 10)) # Creating new data
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5) # Modeling relationship 

linear.lik <- function(theta, y, X)
  { n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k]
  sigma2 <- theta[k+1]^2 
  e <- y - X%*%beta
  logl <- -.5*n*log(2*pi) -.5*n*log(sigma2) - ( (t(e) %*% e)/ (2*sigma2) )
  return ( - logl ) }

linear.MLE <- optim(fn=linear.lik , par=c(1 ,1 ,1) , hessian= TRUE, y=data$y, X=cbind(1, data$x), method = "BFGS" )

linear.MLE$par
