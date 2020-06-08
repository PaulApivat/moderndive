# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

#### Chapter 10 Hypothesis Testing ####
## Computer-based methods using randomization, simulation and bootstrapping > Theory-based methods ###
## Computational Thinking & Statistical Intuition

# Packages
library(tidyverse)
library(moderndive)
library(infer)
library(nycflights13)
library(ggplot2movies)
library(patchwork)  # plot arrangement

# load file
load(file = 'moderndive.RData')

##### Promotions Activity 
## Data: Journal of Applied Psych (1974)
## Does gender affect promotion decisions?

# dataset: 3 columns, 48 rows
promotions 

# Randomly pick 6 rows; arranged by id
promotions %>% 
    sample_n(size = 6) %>% 
    arrange(id)

# Exploratory Data Analysis using Stacked Barchart
# n = 24 resumes for each gender

ggplot(data = promotions, mapping = aes(x = gender, fill = decision)) 
    + geom_bar() 
    + labs(x = "Gender of name on resume")





