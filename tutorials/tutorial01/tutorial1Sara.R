##################
#### Stats II ####
##################

###############################
#### Tutorial 1: Refresher ####
###############################

# Today's tutorial is a refesher of the R skills we learnt in the first semester.
#     1. Importing data
#     2. Wrangling data
#     3. Analysing data
#     4. Communicating

# We will attempt to do all of this in 50 minutes!

#### Case study
# A fictional think-tank, the Alliance of Wealthy People who Dislike Tax, has asked
# you to study the relationship between tax, regulation and GDP per capita. They believe
# that countries with low tax and light regulation are wealthier, and they want you to 
# prove it using statistics!

#### Creating the dataset
# For this task, we need data. The World Bank has the information we need.

# 1. Go to https://databank.worldbank.org/source/world-development-indicators#
# 2. In the "add country" screen, select the region "Europe and Central Asia", then 
#    click "select all". Close the pop-up.
# 3. In the "add series" screen, select "Ease of Doing Business Rank", "GDP per capita 
#    (current US$)" and "tax revenue (% of GDP)". Tip: you can use the letters to skip
#    to the right section. Close the pop-up.
# 4. In the "add time" screen, select the year 2019.
# 5. Click "apply changes".
# 6. On the left side of the screen, click the "layout" option.
# 7. Change "country" to "row". Click "apply changes".
# 8. Change "series" to "column". Click "apply changes".
# 9. Change "time" to "page". Click "apply changes".
# 10. In "download options" in the top right, choose "CSV". Save to desktop.
# 11. Open the folder on the desktop, click "extract all". Extract to desktop.

# NOTA: Lo guardé como "actual_data" en el folder del tutorial 

# Well done, you've got your dataset!

#### Importing the data
# Your csv file should now be in the desktop folder. Before opening it, we're going to
# load in our libraries.

library(tidyverse)
library(stargazer)

## loading the data
# We're going to use some built-in functionality of RStudio to help us. In the "files"
# window in the bottom right, click the "..." box and open the folder with the data. Then
# open the data file we just imported using "import dataset...". In the window that 
# opens, you'll notice we can manually change the type of data for each column...

data <- read.csv("/Users/sarabcidf/Desktop/ASDS/Stats II/StatsII_Spring2024/tutorials/tutorial01/actual_data.csv")

#### Wrangling the data
# We should now have a dataset where our variables are at least of the correct type.
# However, we need to do a bit of tidying to get the data into a more user-friendly
# format. 
  
# 1. First, let's have a look at our data object. Use the functions we learned from last
#    term. 

summary(data)
str(data)
summary(data)
ls(data)
ls.str(data)
head(data)


# 2. Let's drop the rows and columns we don't need.
# We only have one year, so the two cols related to year can be dropped; also, we only
# really need one col for country name, so let's drop country code too.

data <- data %>%
  select(-(starts_with("Time")), -Country.Code)
  
# 3. Let's also get rid of the variable code in square brackets

names(data) <- sub(" \\[.*", "", names(data)) # hint: try using the function sub() with the regexp " \\[.*"
# That didn't really work

names(data) <- c("c_name", "ease_business","gdp_cap","tax_rev_gdp")

# The data also has issues in that NAs are coded as ".."
data <- data %>%
  mutate(across(everything(), ~na_if(., "..")))

data$ease_business <- as.numeric(data$ease_business)
data$gdp_cap <- as.numeric(data$gdp_cap)
data$tax_rev_gdp <- as.numeric(data$tax_rev_gdp)
  
#### Analysing the data
# Now that we have a dataset in the desired format, we can proceed to the analysis.

# 1. Let's perform some preliminary descriptive analysis using our visualisation skills.
#    Try using ggplot to create a plot of scatter showing GDP p/c vs Tax revenue. Add a
#    simple linear regression line.


plot1 <- ggplot(data, aes(x = gdp_cap, y = tax_rev_gdp)) +
                  geom_point() +
                  geom_smooth(method = "lm", se = FALSE) + 
                  theme_minimal() +                    
                  labs(
                    title = "Scatterplot with Regression Line",
                    x = "gdp_cap",
                    y = "tax_rev"
                  )
plot1
  
# 2. Now let's try the same using GDP p/c vs Ease of Doing Business.

plot2 <- ggplot(data, aes(x = gdp_cap, y = ease_business)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() +                    
  labs(
    title = "Scatterplot with Regression Line",
    x = "gdp_cap",
    y = "business"
  )
plot2

# 3. And, for the sake of argument, let's see what the relationship is between Tax and
#    Ease of Doing Business.

plot3 <- ggplot(data, aes(x = tax_rev_gdp, y = ease_business)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() +                    
  labs(
    title = "Scatterplot with Regression Line",
    x = "tax",
    y = "business"
  )
plot3

# 4. Let's think for a minute before we perform the multivariate regression: what kind
#    of interaction are we seeing with these three plots?

# 5. Now let's run a regression!

reg <- lm(gdp_cap ~ tax_rev_gdp + ease_business, data = data)
summary(reg)

# How do we interpret these results?

#### Communicating
# The final task is to communicate our results. We're going to do this in pdf format 
# using latex, and then upload our results to github, just as we would with a problem
# set!

# 1. Visualisation
# We want a good visualisation of our results, including a title. We've seen that Ease 
# of Doing Business doesn't seem to have a very significant effect (statistically or
# substantively), so let's plot GDP vs Tax, and include Ease of Doing Business as
# either a size or alpha variable to our scatter points. Use the "export" option in the
# plots window to create a pdf of the plot below. Save it in the same folder as your 
# latex template.

data %>%
  ggplot(aes(tax_rev_gdp, 
             gdp_cap, 
             alpha = ease_business)) +
  geom_point() +
  geom_smooth(method = "lm", show.legend = FALSE) +
  ylim(0, 150000) +
  labs(title = "",
       subtitle = "",
       alpha = "") +
  theme(legend.position = c(.85, .75),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.5, "cm"))

# 2. Regression table
# We'll use stargazer to create the latex code for our regression table. Clear your 
# console, then run the code below.

stargazer(reg, type = "latex")

# Now all we need is to update the latex template and upload the pdf to github!