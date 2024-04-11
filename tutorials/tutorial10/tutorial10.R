###################################
# Tutorial 10: Survival Analysis #
###################################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)

#### Survival Analysis

# The `child` dataset from the `eha` package is a dataset of 26,855 children born in 
# Skellefte?, Sweden, 1850-1884. Children are followed for fifteen years or until death or 
# outmigration.
# The response variable is `exit`
# Explanatory variables include:
# - id: An identification number.
# - m.id: Mother's id.
# - sex: Sex.
# - socBranch: Working branch of family (father).
# - birthdate: Birthdate.
# - enter: Start age of follow-up, always zero.
# - exit: Age of departure, either by death or emigration.
# - event: Type of departure, death = 1, right censoring = 0.
# - illeg: Born out of marriage ("illegitimate")?
# - m.age: Mother's age.

## a) Using the Surv() function, build a survival object out of the `child` data.frame. 
##    Using survfit() and R's plotting functions, produce a Kaplan-Meier plot of the data,
##    firstly for overall survival, and secondly comparing categories of socBranch. How do
##    you interpret the second plot?

## b) Run a Cox Proportional Hazard regression on the data, using an additive model with 
##    `socBranch` and `sex` as explanatory variables. Run a test to assess the quality of the
##    model. How can we interpret the coefficients? Plot the model.

#### Complete: 

##################################
# Tutorial 10: Survival Analysis #
##################################

data(child)

#### Completed
# a)
child_surv <- with(child, Surv(enter, exit, event))

km <- survfit(child_surv ~ 1, data = child)
summary(km, times = seq(0, 15, 1))
plot(km, main = "Kaplan-Meier Plot", xlab = "Years", ylim = c(0.7, 1))
autoplot(km)

km_socBranch <- survfit(child_surv ~ socBranch, data = child)
summary (km_socBranch)
autoplot(km_socBranch)

# b)
cox <- coxph(child_surv ~ sex + socBranch, data = child)
summary(cox)
drop1(cox, test = "Chisq")
stargazer(cox, type = "text")

# There is a 0.08 decrease in the expected log of the hazard for female babies compared to 
# male, holding socBranch constant. There is a 0.34 increase in the expected log of the hazard
# for babies of businessmen compared to officials, holding sex constant.

# exponentiate parameter estimates to obtain hazard ratios
exp(-0.083546)
# The hazard ratio of female babies is 0.92 that of male babies, i.e. female babies are less
# likely to die (92 female babies die for every 100 male babies; female deaths are 8% lower, etc.)

cox_fit <- survfit(cox)
autoplot(cox_fit)

newdat <- with(child, 
               data.frame(
                 sex = c("male", "female"), socBranch="official"
               )
)

plot(survfit(cox, newdata = newdat), xscale = 12,
     conf.int = T,
     ylim = c(0.6, 1),
     col = c("red", "blue"),
     xlab = "Time",
     ylab = "Survival proportion",
     main = "")
legend("bottomleft",
       legend=c("Male", "Female"),
       lty = 1, 
       col = c("red", "blue"),
       text.col = c("red", "blue"))
# Note: the confidence intervals on this plot are for the prediction, not
# the standard error of the terms in the model (the effect of sex in the 
# cox ph model was significant, here the CIs overlap. Always check your
# results and interpretation!)


# Adding an interaction
cox.int <- coxph(child_surv ~ sex * socBranch, data = child)
summary(cox.int)
drop1(cox.int, test = "Chisq")
stargazer(cox.int, type = "text")
