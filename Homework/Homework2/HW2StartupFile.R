# Homework 2 Problems

### 1.a 
# Develop a function that has the following formal arguements: 
# 1. a multiple linear regression model object from lm()
# 2. a number of bootstrap iterations (200 by default)
# 3. a confidence level (95% by default)
# Your function must return a set of bootstrap confidence intervals for the beta coefficients in that model

# note: Your function may not call another function to conduct the bootstrapping or interval construction

### 1.B Use your function on each of the 9 regression datasets used in class in Lesson 6 (10/4)



# --------------------------------------------------------------------- #
### French Fries experiment - data from reshape2 package

# install.packages("reshape2")
## Load French Fries taste test data
ff <- reshape2::french_fries
# only first and last week
ff_before_after <- ff[ff$time %in% c(1,10), ]
str(ff_before_after)

### the following mixed-effects model will test for interactions between 
# the oil types (treatment) and number of weeks the oil had been used (time=1 to 10),
# while modeling the random variability of replicates between the participants 
library(lme4)
mixed_mod <- lme4::lmer(potato ~ (-1) + time:treatment + (1|subject), data=ff_before_after)
mixed_mod
mixed_mod@beta
# In the main effects you will find that coefficients represent the 2-by-3 structure
# where we have {start, end} by {oil1, oil2, oil3} represented. 

#-----------
# 2.a. Conduct a permutation test for pairwise differences between 
# the estimated starting scores (time=1) for the potato flavors for two oil types.

# Note: you need to be careful to permute the treatment labels WITHIN each experimental block
# for subject and time for this test. 



#-----------
# 2.b. Create permutation-based 95% confidence intervals for the change in flavor before and after
# for each of the three oils. In other words, create permutation-based confidence intervals for beta2, beta4 and beta6. 



