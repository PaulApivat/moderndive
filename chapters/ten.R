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



## Shuffling ONCE - 
# Hypothesis Testing is about hypothetical universe(s)
# RANDOM SHUFFLING of gender only makes sense in a universe with NO GENDER discrimination


# NULL Hypothesis = one universe
# Alternative Hypothesis = another universe

promotions_shuffled

# Exploratory Data Analysis of SHUFFLED resumes
# proportions more similar
ggplot(promotions_shuffled, aes(x = gender, fill = decision)) 
    + geom_bar() 
    + labs(x = 'Gender of resume name (shuffled)')

# find proportions in shuffled universe
promotions_shuffled %>% 
    group_by(gender, decision) %>% 
    tally(sort = TRUE)

# A tibble: 4 x 3
# Groups:   gender [2]
#  gender decision     n
#  <fct>  <fct>    <int>
#1 male   promoted    18        75% males promoted
#2 female promoted    17        70.8% females promoted
#3 female not          7
#4 male   not          6

### Resumes with male names had 4.2% more promotions than female names

##### SAMPLING VARIATION: First, IRL sample there was a 29.2% difference, in shuffled sample 4.2% difference
## could there be sampling variation? What if we repeat shuffling multiple times?

#### SHUFFLING 16 TIMES
## Shuffling = Permutations = Re-sampling WITHOUT Replacement

# recruit 16 people to repeat shuffling exercise
# display distribution of difference in Male vs Female promotion, 16 times
# These 16 shuffles equal HYPOTHESIZED UNIVERSE of NO GENDER DISCRIMINATION
# We expect most difference values to center at '0' (no gender discrimination)

# Sampling Variation means there's a distribution around Zero; -0.292 to 0.208
# Turn attention BACK to IRL: is the 29.2% difference

# Ask: In a HYPOTHESIZED UNIVERSE of no gender discrimination (Null Hypothesis), 
# HOW LIKELY would it be that we observe this (29.2%) difference?





