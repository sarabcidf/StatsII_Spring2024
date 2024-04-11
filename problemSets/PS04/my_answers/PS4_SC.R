#### Libraries and options ####

# Removing objects
rm(list=ls())

# Setting wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)

#### Data and survival object ####

data(child)
child_surv <- with(child, Surv(enter, exit, event))

#### Kaplan-Meier and Survival Plots (don't think this is necessary) ####

# Kaplan-Meier estimation
km <- survfit(child_surv ~ 1, data = child)
summary(km, times = seq(0, 15, 1))
plot(km, main = "Kaplan-Meier Plot", xlab = "Years", ylim = c(0.7, 1))
autoplot(km)

# Kaplan-Meier estimation by socio-economic branch
km_socBranch <- survfit(child_surv ~ socBranch, data = child)
summary(km_socBranch)
autoplot(km_socBranch)

#### Cox Proportional Hazard Model ####

# Fitting the Cox PH model: 
cox <- coxph(child_surv ~ sex + m.age, data = child)
summary(cox)
drop1(cox, test = "Chisq")
stargazer(cox, type = "latex")

# Exponantiating Coefs for Interpretation
exp(cox$coefficients) 
stargazer(exp(cox$coefficients))

# Plotting Cox PH model
cox_fit <- survfit(cox)
autoplot(cox_fit)
ggsave("autoplot.png")

# Hazard ratio: 
# The hazard ratio of female babies is 0.92 that of male babies, i.e. female babies are less
  # likely to die (92 female babies die for every 100 male babies; female deaths are 8% lower, etc.)
# For every one unit increase in mother's age, the hazard ratio increases by 1.01,
  # i.e. babies with younger mothers are more likely to die 
  #(101 babies with older mothers for for every 100 babies with younger mothers

