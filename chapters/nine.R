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
# Ans: Bootstrapping Re-sampling (with replacement)
# What if, instead of single estimate of (unknowable) population parameter, we want range of plausible values?
# Ans: Confidence Interval

# Taking multiple samples (not do in practice)
# BOOTSTRAPPING distributions taking muliple RE-sample from a SINGLE sample
# BOOTSTRAPPING: study effects of sampling variation on esitmates from the 'effort' of a SINGLE sample

# Packages
library(tidyverse)
library(moderndive)
library(infer)
library(patchwork)  # plot arrangement

# load file
load(file = 'moderndive.RData')

###### What is the average year on US pennies in 2019?

pennies_sample

# EDA of pennies_sample df
ggplot(data = pennies_sample, mapping = aes(x = year)) 
+ geom_histogram(binwidth = 10, color = 'green')

# find average year
options(digits = 6) # set global option to allow 1995.44 (6 digits)
pennies_sample %>% summarize(mean_year = mean(year))

# To demonstrate re-sampling with replacement, create pennies_resample tibble 
# of 50 resample values

pennies_resample <- tibble(
  year = c(1976, 1962, 1976, 1983, 2017, 2015, 2015, 1962, 2016, 1976, 
           2006, 1997, 1988, 2015, 2015, 1988, 2016, 1978, 1979, 1997, 
           1974, 2013, 1978, 2015, 2008, 1982, 1986, 1979, 1981, 2004, 
           2000, 1995, 1999, 2006, 1979, 2015, 1979, 1998, 1981, 2015, 
           2000, 1999, 1988, 2017, 1992, 1997, 1990, 1988, 2006, 2000)
)

# compare distribution of Original Sample 50 pennies vs Re-sample of 50 pennies
library(patchwork)

resample_plot <- ggplot(data = pennies_resample, mapping = aes(x = year)) 
    + geom_histogram(binwidth = 10, color = 'red') 
    + labs(title = "Re-sample of 50 pennies")

sample_plot <- ggplot(data = pennies_sample, mapping = aes(x = year)) 
    + geom_histogram(binwidth = 10, color = 'green') 
    + labs(title = "Original Sample of 50 pennies")

resample_plot + sample_plot

# compare means of pennies_sample vs pennies_resample
pennies_resample %>% summarize(mean_year = mean(year)) %>% pull(mean_year) # [1] 1994.82
pennies_sample %>% summarize(mean_year = mean(year)) %>% pull(mean_year) # [1] 1995.44

####### Resampling 35 times
### with help of 35 friends

pennies_resamples   # note pennies_resample(s), in moderndive pkg

# What was the mean year of pennies for each of the 35 friends?

resampled_means <- pennies_resamples %>% 
                    group_by(name) %>% 
                    summarize(mean_year = mean(year))

# Visualize the distribution of mean years for each of 35 friends
# Histogram
ggplot(data = resampled_means, mapping = aes(x = mean_year)) 
    + geom_histogram(binwidth = 1, color = 'white', boundary = 1990) 
    + labs(x = 'Sampled mean year')

## Histogram of distribution of mean_years for 35 friends is called BOOTSTRAP DISTRIBUTION
## Is APPROXIMATES the sampling distribution of sample mean (ch8)

######## Computer Simulation of REsampling


## Virtual resampling ONCE

pennies_sample # original sample of 50 pennies

# size = 50 matches size of original sample
# resample with replacement = TRUE
# default reps = 1
virtual_resample <- pennies_sample %>%
+ rep_sample_n(size = 50, replace = TRUE)

virtual_resample %>% summarize(resample_mean = mean(year)) %>% pull(resample_mean) # [1] 1995.66

## NOTE: When you virtually resample ONCE, there is a SINGLE sample statistic (1995.66) and 
## so there is no sample distribution to visualize

## Virtual resampling 35 times
virtual_resamples <- pennies_sample %>% 
                rep_sample_n(size = 50, replace = TRUE, reps = 35)

virtual_resampled_means <- virtual_resamples %>% 
                group_by(replicate) %>% 
                summarize(mean_year = mean(year))

# plot distribution of resampled means on histogram
ggplot(data = virtual_resampled_means, mapping = aes(x = mean_year)) 
    + geom_histogram(binwidth = 1, color = 'orange', boundary = 1990) 
    + labs(x = "REsample mean year (35 re-samples)")
    + geom_vline(xintercept = 1995.679, color = 'dodgerblue')

### Adding mean_of_means and visualizing at geom_vline

# [1] 1995.679
virtual_resampled_means %>% 
    summarize(mean_of_means = mean(mean_year)) %>% 
    pull(mean_of_means)

resampled_means_35 <- ggplot(data = virtual_resampled_means, mapping = aes(x = mean_year)) 
    + geom_histogram(binwidth = 1, color = 'orange', boundary = 1990) 
    + labs(x = "REsample mean year (35 re-samples)") 
    + geom_vline(xintercept = 1995.679, color = 'dodgerblue')





## Virtual resampling 1000 times

# re-sampling 1000 times
virtual_resamples_1k <- pennies_sample %>% 
            rep_sample_n(size = 50, replace = TRUE, reps = 1000)

# compute 1000 sample means
virtual_resampled_means_1k <- virtual_resamples_1k %>%
    + group_by(replicate) %>%
    + summarize(mean_year = mean(year))

# histogram distribution of 1000 sample means (bell shape more clear)
ggplot(data = virtual_resampled_means_1k, aes(x = mean_year)) 
    + geom_histogram(binwidth = 1, color = 'dodgerblue', boundary = 1990) 
    + labs(x = 'sample mean (1000 re-samples)')

# mean of 1000 re-sampled means (1995.42)

virtual_resampled_means_1k %>% 
    summarize(mean_of_means = mean(mean_year)) %>% 
    pull(mean_of_means)

# add geom_vline representing mean of 1000 re-sampled means
resampled_means_1k <- ggplot(data = virtual_resampled_means_1k, aes(x = mean_year)) 
    + geom_histogram(binwidth = 1, color = 'dodgerblue', boundary = 1990) 
    + labs(x = 'sample mean (1000 re-samples)') 
    + geom_vline(xintercept = 1995.42, color = 'red')

# compare distribution shape of Virtual Resampling (35x) vs Virtual Resampling (1000x)
library(patchwork)
resampled_means_35 + resampled_means_1k

######## Understanding Confidence Intervals ##########
## Analogy: Point Estimate (spear fishing) vs Confidence Interval (fishing with a net)

## Confidence Interval construction: 
## 2 Methods: Percentile Method and Standard Error Method

## Percentile method: compute 2.5th and 97.5th percentiles
## Standard Error method: 
# - compute Standard Deviation from bootstrap distribution (aka Standard Error or SE)
# - calc 95% CI: Re-Sampled Mean +/- (1.96 * SE)

#### CONSTRUCTING CONFIDENCE INTERVAL ####

### compare process of getting means_of_means between DPLYR vs INFER packages


### DPLYR work flow ###

# re-sampling 1000 times
virtual_resamples_1k <- pennies_sample %>% 
            rep_sample_n(size = 50, replace = TRUE, reps = 1000)

# compute 1000 sample means
virtual_resampled_means_1k <- virtual_resamples_1k %>%
    + group_by(replicate) %>%
    + summarize(mean_year = mean(year))

# mean of 1000 re-sampled means (1995.42)
virtual_resampled_means_1k %>% 
    summarize(mean_of_means = mean(mean_year)) %>% 
    pull(mean_of_means)

# tying all dplyr pipes into ONE workflow
# 1995.484 (vs 1995.419 originally)
pennies_sample %>% 
    rep_sample_n(size = 50, replace = TRUE, reps = 1000) %>% 
    group_by(replicate) %>% 
    summarize(mean_year = mean(year)) %>% 
    summarize(mean_of_means = mean(mean_year)) %>% 
    pull(mean_of_means)

# NOTE: Authors do not provide VISUAL of SHADED Confidence interval in GGPLOT 
## Confidence Interval in Shaded Yellow between 1991 and 2000
# So here it is:

virtual_resampled_means_1k %>% 
    summarize(mean_of_means = mean(mean_year), 
        quantile1 = quantile(mean_year, probs = 0.025), 
        quantile2 = quantile(mean_year, probs = 0.975))

## NOTE: note exact decimals for quantile1 and quantile2 
ggplot(data = virtual_resampled_means_1k, aes(x = mean_year)) 
    + geom_histogram(binwidth = 1, color = 'dodgerblue', boundary = 1990) 
    + annotate("rect", xmin = 1991, xmax = 2000, ymin = 0, ymax = Inf, fill = 'yellow', alpha = .5)




### INFER work flow ###
## main advantage: 
# - `infer` verb names better align with overall resampling framework needed to construct confidence intervals
# - can jump back and forth seamlessly between CI and hypothesis testing w minimal code changes
# - simpler for conducing inference when you have more than one variable (two groups or regression > than two var)

bootstrap_distribution <- pennies_sample %>% 
    specify(response = year) %>% 
    generate(reps = 1000) %>% 
    calculate(stat = 'mean')

visualise(bootstrap_distribution)

# comparison of visual plot workflow

visualise(bootstrap_distribution)

# OR

ggplot(data = bootstrap_distribution, mapping = aes(x = stat)) 
    + geom_histogram(binwidth = 1, color = 'white')


####### CONFIDENCE INTERVAL with INFER ######

### Percentile method w Infer
percentile_ci <- bootstrap_distribution %>% 
    get_confidence_interval(level = 0.95, type = 'percentile')

percentile_ci

# Visually Plot Confidence Interval
visualise(bootstrap_distribution) 
    + shade_confidence_interval(endpoints = percentile_ci)

# OR
visualise(bootstrap_distribution) 
    + shade_ci(endpoints = percentile_ci, color = 'hotpink', fill = 'khaki')

### Standard error method w Infer ###
x_bar <- pennies_sample %>% 
    summarize(mean_year = mean(year)) %>% 
    pull(mean_year)

standard_error_ci <- bootstrap_distribution %>% 
    get_confidence_interval(type = "se", point_estimate = x_bar)

standard_error_ci

# easy visualization
visualise(bootstrap_distribution) 
    + shade_confidence_interval(endpoints = standard_error_ci)





