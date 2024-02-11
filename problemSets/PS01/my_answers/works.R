rm(list=ls())

set.seed (123) # Setting my seed 
data <- data.frame(x = runif(200, 1, 10)) # Creating new data
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5) # Modeling relationship 

log_likelihood <- function(params, data) {
  alpha <- params[1]
  beta1 <- params[2]
  sig2 <- abs(params[3])  # Ensure variance is positive
  
  n <- nrow(data)
  residuals <- data$y - (alpha + beta1 * data$x)
  loglike <- -(n/2) * log(2 * pi * sig2) - sum(residuals^2) / (2 * sig2)
  
  return(loglike)
}

initial_params <- c(alpha = 0, beta1 = 1, sig2 = 1)

mle <- optim(par = initial_params, fn = log_likelihood, data = data, method = "BFGS", control = list(fnscale = -1))

mle$par

# Doesn't work: 

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


# Maybe: 

rm(list=ls())

set.seed (123) # Setting my seed 
data <- data.frame(x = runif(200, 1, 10)) # Creating new data
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5) # Modeling relationship 

linear.lik <- function(theta, y, X) {
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k]
  sigma2 <- theta[k + 1]^2
  e <- y - X %*% beta
  logl <- -0.5 * n * log(2 * pi) - 0.5 * n * log(sigma2) - ((t(e) %*% e) / (2 * sigma2))
  return(-logl)
}

linear.MLE <- optim(fn=linear.lik , par=c(1 ,1 ,1) , hessian= TRUE, y=data$y, X=cbind(1, data$x), method = "BFGS" )

linear.MLE$par

# Kaley: 

set.seed(123)
data2 <- data.frame(x = runif(200, 1, 10))
data2$y <- 0 + 2.75*data2$x + rnorm(200, 0, 1.5)

#creating the linear_likelihood function 

linear_lik <- function(theta, y, X) {
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k]
  sigma2 <- theta[k + 1]^2
  e <- y - X %*% beta
  logl <- -0.5 * n * log(2 * pi) - 0.5 * n * log(sigma2) - (t(e) %*% e) / (2 * sigma2)
  return(-logl)
}


#using the optima function to create a linear_MLE function
#that uses the linear_lik function

linear_MLE <- optim(
  fn = linear_lik,
  par = c(1, 1, 1),
  hessian = TRUE,
  y = data2$y,
  X = cbind(1, data2$x),
  method = "BFGS"
)

linear_MLE$par

