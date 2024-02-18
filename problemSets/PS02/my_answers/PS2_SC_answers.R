#### Rm, Libraries, Wd #### 

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

lapply(c("stargazer","stargazer"),  pkgTest)

# Setting wd for current folder

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Loading & inspecting data ####

# Loading data

load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))

# Quick exploration

head(climateSupport)
summary(climateSupport)

#### Q1 ####

# Re-leveling independent variables so that "none" is the reference category for 
# sanctions, and 20/192 is the reference category for countries:

# First we must make into unordered factors: 

climateSupport$sanctions <- factor(climateSupport$sanctions, ordered = FALSE)
climateSupport$countries <- factor(climateSupport$countries, ordered = FALSE)

# Then we relevel:

climateSupport$sanctions <- relevel(climateSupport$sanctions, ref = "None")
climateSupport$countries <- relevel(climateSupport$countries, ref = "20 of 192")

# Fitting the model (taking code from slides): 

climate_logit <- glm(choice ~ countries + sanctions, data = climateSupport, 
                     family = binomial(link = logit))

# Looking at the results: 

summary(climate_logit)

# Reporting: 

stargazer(climate_logit, type = "text", title = "Model Summary", header = FALSE,
          model.names = FALSE, significance.levels = c(0.05, 0.01, 0.001), 
          add.lines = list(c("Number of Fisher Scoring iterations", "4")))

stargazer(climate_logit, type = "latex", title = "Model Summary", header = FALSE,
          model.names = FALSE, significance.levels = c(0.05, 0.01, 0.001),
          add.lines = list(c("Number of Fisher Scoring iterations", "4")))

# Testing global null hypothesis (code from slides 49 and 50, W4): 

null_logit <- glm(choice ~ 1, data = climateSupport, family = binomial(link = "logit"))
summary(null_logit)

anova_test <- anova(null_logit, climate_logit, test = "LRT")
anova_test

# Reporting: 

stargazer(null_logit, type = "latex", 
          title = "Summary of Null Logistic Regression Model")

anova_table <- as.data.frame(anova_test)

stargazer(anova_table, type = "latex",  
          title = "LRT Results")


# For transformation of results: 

b_countries_80_of_192 <- 0.336
b_countries_160_of_192 <- 0.648
b_sanctions_5_percent <- 0.192
b_sanctions_15_percent <- -0.133
b_sanctions_20_percent <- -0.304

# Transforming from "effect on log odds" to "effect on odds": 

odds_80_countries <- exp(b_countries_80_of_192)
odds_160_countries <- exp(b_countries_160_of_192)
odds_sanctions_5_percent <- exp(b_sanctions_5_percent)
odds_sanctions_15_percent <- exp(b_sanctions_15_percent)
odds_sanctions_20_percent <- exp(b_sanctions_20_percent)

# Transforming from "effect on odds" to "effect on probability": 

prob_80_countries <- odds_80_countries / (1 + odds_80_countries)
prob_160_countries <- odds_160_countries / (1 + odds_160_countries)
prob_sanctions_5_percent <- odds_sanctions_5_percent / (1 + odds_sanctions_5_percent)
prob_sanctions_15_percent <- odds_sanctions_15_percent / (1 + odds_sanctions_15_percent)
prob_sanctions_20_percent <- odds_sanctions_20_percent / (1 + odds_sanctions_20_percent)

# Creating table to report: 

transf_results <- data.frame(
  Variable = c("80 of 192 countries", "160 of 192 countries", "Sanctions 5%", "Sanctions 15%", "Sanctions 20%"),
  Odds = c(odds_80_countries, odds_160_countries, odds_sanctions_5_percent, odds_sanctions_15_percent, odds_sanctions_20_percent),
  Probability = c(prob_80_countries, prob_160_countries, prob_sanctions_5_percent, prob_sanctions_15_percent, prob_sanctions_20_percent)
)

stargazer(transf_results, type = "latex", 
          title = "Effects of Countries Participation and Sanctions on Policy Support", 
          header = FALSE, summary = FALSE)

#### Q2 #### 

## PART A ##

# Changing the reference category to 5% sanctions to be able to interpret directly: 

climateSupport$sanctions2 <- relevel(climateSupport$sanctions, ref = "5%")

# Fitting again: 

climate_logit2 <- glm(choice ~ countries + sanctions2, data = climateSupport, 
                     family = binomial(link = logit))

# Looking at the results: 

summary(climate_logit2)

# Reporting: 

stargazer(climate_logit2, type = "text", title = "Model Summary", header = FALSE,
          model.names = FALSE, significance.levels = c(0.05, 0.01, 0.001),
          report=('vc*p'), 
          add.lines = list(c("Number of Fisher Scoring iterations", "4")))

stargazer(climate_logit2, type = "latex", title = "Model Summary", header = FALSE,
          model.names = FALSE, significance.levels = c(0.05, 0.01, 0.001),
          report=('vc*p'), 
          add.lines = list(c("Number of Fisher Scoring iterations", "4")))

# Transforming for odds and probability: 

coef5to15 <- -0.32510
odds <- exp(coef5to15)
prob <- odds / (1 + odds)

odds
prob

# PART B ##

# Sanctions = None, 
# Countries = 80 out of 192... 

filtered_data <- climateSupport %>%
  filter(countries == "80 of 192", sanctions == "None")

# Creating function based on formula in slide 40, W4: 

probability <- function(b0, b1, xi) {
  1 / (1 + exp(-(b0 + b1 * xi)))
}

coefficients <- coef(climate_logit)
b0 <- coefficients['(Intercept)']
b1 <- coefficients['countries80 of 192'] # This is dependent to this case

prob_support <- probability(b0, b1, xi = 1)
prob_support

# Double-checking with predict():

predicted_prob2 <- predict(climate_logit, newdata = filtered_data, type = "response")
summary(predicted_prob2)

# PART C ##

# Fitting interactive model: 

climate_logit_int <- glm(choice ~ countries + sanctions + countries*sanctions,
                      data = climateSupport, 
                      family = binomial(link = logit))

# LRT: 

anova_test2 <- anova(climate_logit, climate_logit_int, test = "LRT")
anova_test2

anova_table <- as.data.frame(anova_test2)

# Reporting: 

stargazer(anova_test2, type = "latex",  
          title = "LRT Results")

stargazer(climate_logit_int, type = "latex", 
          title = "Summary of Null Logistic Regression Model")

# Reporting together with additive: 

# Apparently, stargazer does not like long model names 
# https://www.reddit.com/r/rstats/comments/ucmtdn/issue_with_stargazer_package_after_update_to_r_420/
# So I change them: 

cl <- climate_logit
cli <- climate_logit_int

stargazer(cl, cli, type = "text", title = "Model Summary", header = FALSE,
          model.names = FALSE, significance.levels = c(0.05, 0.01, 0.001),
          report=('vc*p'), 
          add.lines = list(c("Number of Fisher Scoring iterations", "4")))

stargazer(cl, cli, type = "latex", title = "Model Summary", header = FALSE,
          model.names = FALSE, significance.levels = c(0.05, 0.01, 0.001),
          report=('vc*p'), 
          add.lines = list(c("Number of Fisher Scoring iterations", "4")))





