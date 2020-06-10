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

#### ---CONFIDENCE INTERVAL--- ####

## Bootstrap Distribution for the fitted slope (0.067)
bootstrap_distn_slope <- evals_ch5 %>%
    specify(formula = score ~ bty_avg) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "slope")

visualize(bootstrap_distn_slope)

## Percentile-method 
percentile_ci_ch11 <- bootstrap_distn_slope %>%
    get_confidence_interval(type = 'percentile', level = 0.95)

## Standard error method (should be near 0.067)

# find sample statistic - slope
observed_slope <- evals %>%
    specify(score ~ bty_avg) %>%
    calculate(stat = 'slope')

# create 95% confidence interval
se_ci_ch11 <- bootstrap_distn_slope %>%
    get_ci(level = 0.95, 
            type = 'se', 
            point_estimate = observed_slope)

## Visualize
## Compare all 3 confidence interval approaches
# 1 percentile
# 2 standard error
# 3 theory-based

# Three Confidence Interval and Zero-Line (added)

visualise(bootstrap_distn_slope) 
    + shade_confidence_interval(endpoints = percentile_ci_ch11, fill = NULL, linetype = "solid", color = "#1b9e77") 
    + shade_confidence_interval(endpoints = se_ci_ch11, fill = NULL, linetype = 'dashed', color = '#d95f02') 
    + shade_confidence_interval(endpoints = c(0.035, 0.099), fill = NULL, linetype = 'dotted', color = '#7570b3') 
    # add zero line
    # another cool feature how Infer Package is Tidy-friendly
    + geom_vline(xintercept = 0, color = 'red')


#### ---HYPOTHESIS TESTING--- ####

null_distn_slope <- evals %>%
    specify(score ~ bty_avg) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 1000, type = 'permute') %>%
    calculate(stat = 'slope')

# null distribution of re-sampled slope
# notice center o distribution at 0 because in a 
# NULL Universe there is NO RELATIONSHIP between score and bty_avg
# NO RELATIONSHIP = slope of zero

visualise(null_distn_slope)

# see null distribution in relation to sample statistic slope (0.067)
visualise(null_distn_slope) 
    + geom_vline(xintercept = 0.067, color = 'red', size = 3)

# get exact p-value
null_distn_slope %>% 
    get_p_value(obs_stat = observed_slope, direction = 'both')