# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

#### Chapter 9 Bootstrapping and Confidence Intervals ####

## Review Sampling (ch8)
# Building off sampling distributions and effects of sampling variation (ch8)
# Quantified variation using standard error

# Sampling-related notation
# Study population : population parameter (unknown)
# Study sample : sample statistic (infer population parameter)
# sample, ideally, unbiased and representative of population (generalizable)

# IRL there is no virtual sampling (1000 samples of size n),
# but take SINGLE representative sample

## Question: 
# How do you quantify sampling variation when you only have a single sample to work with?
# Ans: Bootstrapping Re-sampling 
# What if, instead of single estimate of (unknowable) population parameter, we want range of plausible values?
# Ans: Confidence Interval

# Packages
library(tidyverse)
library(moderndive)
library(infer)

# What is the average year on US pennies in 2019?

pennies_sample

# EDA of pennies_sample df
ggplot(data = pennies_sample, mapping = aes(x = year)) 
+ geom_histogram(binwidth = 10, color = 'green')

# find average year
options(digits = 6) # set global option to allow 1995.44 (6 digits)
pennies_sample %>% summarize(mean_year = mean(year))



