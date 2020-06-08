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
# HOW LIKELY would it be that we observe this (29.2%) difference? Or is it just sampling variation?

####### HYPOTHESIS TESTING USING PERMUTATIONS ########

# population parameter: The "true" difference in population proportion; between Male - Female Promotion Rates
# sample statistic: 29.2% difference in sample proportion; Male - Female Promotion Rates
# Is this sampling variation?
# Construct two universe - NULL (no gender discrimination) vs ALT (yes gender discrimination)
# In the NULL universe, how likely would be observe a 29.2% difference?
# A 29.2% difference in favor of male is greater than 0, but is it "meaningfully greater than 0?"

# NULL - different due to sampling variation;
# ALT - true differences

# 1. Population Parameter : A HYPOTHESIS is a statement about the value of an unknown population parameter.
# 2. Hypothesis: Null & Alternative; 1-sided (Male > Female) or 2-sided (Male != Female)
# 3. Observed Test Statistic: (single sample) Point Esimate observed in real life (ie. 29.2% difference)
# 4. Test Statistic: point estimate or sample statistic to test hypothesis
# 5. Null Distribution: Sampling distribution of test statistic ASSUMING the NULL hypothesis is true.
# Ask: Assuming "no gender discrimination", how does the test statistic vary due to sampling variation?
# Ask: Will the difference in sample proportions (male - female) vary due to sampling under the Null Hypothesis universe
# 6. P-Value: probability of obtaining a test statistic as extreme or more extreme than the OBSERVED TEST STATISTIC ASSUMING
# NULL hypothesis is true (surprise element)

# P-Value In a universe of no gender discrimination, how surprise would we be to see 29.2% in favor of men for promotion.
# Significance level: set before hand, if p-value falls below alpha, we reject Null hypothesis
# Significance level: if p-value does not fall below alpha, "FAIL TO REJECT" (not same as 'accept')











