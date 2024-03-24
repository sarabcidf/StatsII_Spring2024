#### Removing objects, detaching and loading libraries, setting WD #### 

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

lapply(c("stargazer", "tidyverse", "nnet", "MASS"),  pkgTest)

# Setting wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Problem 1 #### 

# Data
gdp <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

# Creating three-level categorical outcome: 
gdp <- gdp %>%
  mutate(GDPWdiff_cat = ifelse(GDPWdiff < 0, "Decrease",
                               ifelse(GDPWdiff == 0, "No_change",
                                      "Increase")),
         GDPWdiff_cat = factor(GDPWdiff_cat, levels = c("No_change", "Increase", "Decrease")))

count(gdp, GDPWdiff_cat)

## Unordered version: 
unordered_logit <- multinom(GDPWdiff_cat ~ REG + OIL, data = gdp, ref = "No_change")
summary(unordered_logit)

# Turning to OR: 
odds_ratios <- exp(coef(unordered_logit))
print(round(odds_ratios, 2))

# Reporting: 
stargazer(unordered_logit, type = "latex")

odds_ratios_df <- as.data.frame(round(odds_ratios, 2))
odds_ratios_df$Outcome <- rownames(odds_ratios_df)
stargazer(odds_ratios_df, type = "latex", title = "Odds Ratios from Model 1",
          summary = FALSE,  
          rownames = FALSE)  

## Ordered version:
gdp <- gdp %>%
  mutate(GDPWdiff_cat = factor(GDPWdiff_cat, levels = c
                               ("Decrease", "No_change", "Increase"), ordered = TRUE))

count(gdp, GDPWdiff_cat)

# Running
ordered_logit <- polr(GDPWdiff_cat ~ REG + OIL, data = gdp, Hess=TRUE)
summary(ordered_logit)

# Turning to OR: 
odds_ratios <- exp(coef(ordered_logit))
print(round(odds_ratios, 2))

# Reporting: 
stargazer(ordered_logit, type = "latex")
odds_ratios_df <- as.data.frame(round(odds_ratios, 2))
odds_ratios_df$Outcome <- rownames(odds_ratios_df)
stargazer(odds_ratios_df, type = "latex", title = "Odds Ratios from Model 1",
          summary = FALSE,  
          rownames = FALSE)  

#### Problem 2 #### 

# Data
mex <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, 
                     data = mex, family = "poisson")
summary(poisson)

# Reporting
stargazer(poisson, type = "latex")

# To get the visits: 
log_count_est <- coef(poisson)[1] + 
  coef(poisson)["competitive.district"]*1 + 
  coef(poisson)["marginality.06"]*0 + 
  coef(poisson)["PAN.governor.06"]*1

# Converting log to count: 
expected_visits <- exp(log_count_est)
print(expected_visits)




