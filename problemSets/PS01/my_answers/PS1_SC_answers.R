#### Libraries, wd, clearing #### 

# Removing objects
rm(list=ls())

# Detaching all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Loading libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse"),  pkgTest)

# Setting wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Question 1 #### 

set.seed(123) # Setting my seed 
data1 <- rcauchy(1000) # Creating my random variables

# Creating empirical distribution of observed data
ECDF <- ecdf(data1) # Creating my function 
empiricalCDF <- ECDF(data1) # Using function to calculate cumulative probs for data

# Generating test statistic
D <- max(abs(empiricalCDF - pnorm(data1)))
print(D)

# Creating function to calculate p-value:
# I follow the given formula.
# I use sum from k = 1 to 1000, instead of infinity.

kol_smirn_p <- function(D) {
  # Creating constants:
  sqrtpi = sqrt(2 * pi)
  
  # Initializing empty variables:
  sum_serie = 0
  
  # Summing from k = 1 to 1000, using a for loop: 
  for (k in 1:1000) {
    term = exp(-(2*k - 1)^2 * pi^2 / (8 * D^2))
    sum_serie = sum_serie + term
  }
  
  p_val = (sqrtpi / D) * sum_serie
  return(p_val)
}

# Substituting our D test statistic value: 
pvalue <- kol_smirn_p(D)
print(pvalue)

# Comparing to ks.test results: 
ks_comp <- ks.test(data1, pnorm)
print(ks_comp)

#### Question 2 ####

set.seed (123) # Setting my seed 
data2 <- data.frame(x = runif(200, 1, 10)) # Creating new data
data2$y <- 0 + 2.75*data2$x + rnorm(200, 0, 1.5) # Modeling relationship 

# Defining my log-likelihood function that I will maximize (or minimze its negative).
# My parameters for OLS are the intercept, the slope and the variance of the error term (sigma squared).
# So I take the formula of the log likelihood for a normal distribution. 
# This is based on the slides for class: 

linear.lik <- function(theta, y, X) {
  n <- nrow(X) # No. obs 
  k <- ncol(X) # No. param
  beta <- theta[1:k] # Regression coefs
  sigma2 <- theta[k + 1]^2 # Squared variance 
  
  e <- y - X %*% beta # Calculating residuals 
  logl <- -0.5 * n * log(2 * pi) - 0.5 * n * log(sigma2) - ((t(e) %*% e) / (2 * sigma2))
  return(-logl) # Returning negative log-likelihood for minimization
}

# Maximizing the log-likelihood (or minimizing negative log likelihood) using BFGS method. 
# This code is also taken from slides: 
linear.MLE <- optim(fn = linear.lik, par = c(1, 1, 1), hessian = TRUE, y = data2$y, X = cbind(1, data2$x), method = "BFGS")

# Seeing results: 
linear.MLE$par

# Comparing with lm: 
lm_mod <- lm(y ~ x, data = data2)
summary(lm_mod)





