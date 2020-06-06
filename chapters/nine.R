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

# Packages
library(tidyverse)
library(moderndive)
library(infer)
library(patchwork)  # plot arrangement

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

### Computer Simulation of REsampling


## Virtual resampling ONCE

pennies_sample # original sample of 50 pennies

# size = 50 matches size of original sample
# resample with replacement = TRUE
# default reps = 1
virtual_resample <- pennies_sample %>%
+ rep_sample_n(size = 50, replace = TRUE)

virtual_resample %>% summarize(resample_mean = mean(year)) %>% pull(resample_mean) # [1] 1995.66

## Virtual resampling 35 times
virtual_resamples <- pennies_sample %>% 
                rep_sample_n(size = 50, replace = TRUE, reps = 35)

virtual_resampled_means <- virtual_resamples %>% 
                group_by(replicate) %>% 
                summarize(mean_year = mean(year))

# plot distribution of resampled means on histogram
ggplot(data = virtual_resampled_means, mapping = aes(x = mean_year)) 
    + geom_histogram(binwidth = 1, color = 'orange', boundary = 1990) 
    + labs(x = "REsample mean year")



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
ggplot(data = virtual_resampled_means_1k, aes(x = mean_year)) 
    + geom_histogram(binwidth = 1, color = 'dodgerblue', boundary = 1990) 
    + labs(x = 'sample mean (1000 re-samples)') 
    + geom_vline(xintercept = 1995.42, color = 'red')


### Understanding Confidence Intervals