# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

### Chapter 11 Inference for Regression ###

# load file
load(file = 'moderndive.RData')

# packages
library(tidyverse)
library(moderndive)
library(infer)
library(patchwork)  # plot arrangement


######## Regression refresher: score ~ bty_avg ###########
# Sampling scenario around regression case; n = 463 is a sample
# 463 teacher(s) assumed to be representative sample of greater population (e.g., all instructors at UT Austin)
# (always need more knowledge of sampling methodology)
# "estimate" in the regression table is a point estimate of population slope    (0.067)
# "estimate" in the regression table is a point estimate of population intercept (3.880)
# the "best fitting line" is an ESTIMATE of some true (and unknowable) POPULATION LINE
# Hence, Scenario for sampling inference (population regression slope & fitted regression slope)

# proceed to the rest of the Regression Table

# STD_ERROR: if we collect from another 463 sample (score + bty_avg), another 1000 times, 
# then put the data on histogram, we would have sampling distribution, standard deviation of sampling dist = std_error

# statistic: (test statistic, 4.09) corresponds to standardized t-test statisic (theory-based, not computer-based)
# note: get_regression_table is theory-based. SKIP FOR NOW

# p-value: probability of obtaining a test statistic as extreme or more than observed test statistic (sample test statistic = 4.09)
# assuming Null Hypothesis is true

# lower_ci
# upper_ci

#### CONDITIONS FOR INFERENCE FOR REGRESSION #####
## Do the following assumptions hold....

# LINE
# L - linearity of relationship between variables
# I - independence of residuals
# N - normality of residuals
# E - equality of variance of residuals

##### REFRESHER ON REGRESSION (moderndive package)
evals # comes with moderndive package, dataset score, bty_avg (n = 463)

evals_ch5 <- evals %>% select(ID, score, bty_avg, age)

# Best Fitting Line through scatter plot
ggplot(evals_ch5, aes(x = bty_avg, y = score)) 
    + geom_point() 
    + labs(x = 'Beauty Score', 
           y = 'Teaching Score', 
           title = 'Relationship between teaching and beauty scores') 
    + geom_smooth(method = "lm", se = FALSE)

# Fit Regression Model
score_model <- lm(score ~ bty_avg, data = evals_ch5)

# Get Regression Table
get_regression_table(score_model)


###### REFRESHER ON RESIDUALS

# Get Residual Points from score_model
regression_points <- get_regression_points(score_model)

# Linearity of Relationships

# Independence of Residuals
# violated because professors teach more than once class
evals %>% select(ID, prof_ID, score, bty_avg) %>% view()

# Normality of Residuals
# center should be a 0, should be normal shaped - check histogram
ggplot(regression_points, aes(x = residual)) 
    + geom_histogram(binwidth = 0.25, color = "white") 
    + labs(x = 'Residual')

# Equality of Variance
ggplot(regression_points, aes(x = bty_avg, y = residual)) 
    + geom_point() + labs(x = 'Beauty Score', y = "Residual") 
    + geom_hline(yintercept = 0, col = 'blue', size = 1)

###### SIMULTION-BASED INFERENCE FOR REGRESSION (instead of theory-based) ########
