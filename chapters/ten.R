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

## See proportion of gender w promotion decision
promotions %>%
    + group_by(gender, decision) %>%
    + tally(sort = TRUE)

# A tibble: 4 x 3
# Groups:   gender [2]
#  gender decision     n
#  <fct>  <fct>    <int>
#1 male   promoted    21      87.5% Male Promoted
#2 female promoted    14      58.3% Female Promoted
#3 female not         10
#4 male   not          3

### Resumes with male names had 29.2% more promotions than female names



### Does this provide CONCLUSIVE evidence of advantage for males?
## Alternatively can 29.2% difference in *this* sample occur by chance? 



## Shuffling ONCE - Hypothesis Testing is about hypothetical universe(s)
# NULL Hypothesis = one universe
# Alternative Hypothesis = another universe



