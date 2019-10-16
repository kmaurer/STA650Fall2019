# Homework 2 Problem Development

library(tidyverse)

# Develop a function that will take in ONLY a multiple linear regression model 
# object, a number of bootstrap iterations (200 by default), and a confidence level (95% by default)
# that returns a set of bootstrap confidence intervals for the beta coefficients in that model

# Only prebuilt functions you are allowed to use are from Base R: 
  # Basic mathematical operators (+, -, /, *, ^, ...)
  # Basic summary statistics (quantile, mean, median, sd, var, cor, min, max, ... ) 
  # Basis data object functions (c, data.frame, list, array, length, dim, nrow, ... )
  # Apply family functions (apply, sapply, lapply, mapply)
  # Function building functions (function, assignment arrow, equals operator)
  # Model building functions (lm)


# bootstrapping helper function
boot_samp <- function(df){
  return(df[sample(1:nrow(df), replace=T), ])
} 

# bootstrap interval generator
boot_beta_intervals <- function(object, B=200, alpha=.05){
  # gather bootstrap sample statistics (betas) from B bootstrap samples
  boot_betas <- sapply(1:B, function(i){
    lm(formula(object), boot_samp(object$model))$coef 
  })
  # find the central 100*(1-alpha) CI for each beta
  results <- data.frame(estimate=apply(boot_betas, 1, quantile, probs=c(0.5)),        
                        lower=apply(boot_betas, 1, quantile, probs=c(alpha/2)),
                        upper=apply(boot_betas, 1, quantile, probs=c(1-alpha/2)))
  names(results)[2:3] <- paste0(c("lower:","upper:"), c(alpha/2, 1-alpha/2))
  return(results)
}

my_mod <- lm(price ~ carat + x + y + z + cut, data=diamonds)
boot_beta_intervals(my_mod)

my_mod2 <- lm(Petal.Length ~ ., data=iris)
boot_beta_intervals(my_mod2, B=1000,alpha=.1)

### 1.B Use your function on each of the 9 regression datasets used in class in Lesson 6 (10/4)

# From lesson 6 Code =-=-=-=-=-=-=-=-=-=-=
all_reg_sets <- c("wpbc","wankara","laser","treasury",
                  "skillcraft","puma","air_quality","ccpp","casp")

data_list <- list(NULL)
all_reg_sizes <- NA
for (i in 1:length(all_reg_sets)){
  load(paste0("C:/Users/maurerkt/Documents/Github/STA650Fall2019/Lessons/data/",all_reg_sets[i],"_cleaned.Rdata"))
  data_list[[i]] <- data
  all_reg_sizes[i] <- nrow(data)
  print(paste("vars from data",all_reg_sets[i]))
  print(names(data))
}
names(data_list) <- all_reg_sets
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

beta_intervals_list <- lapply(data_list, function(df){
  mod <- lm(y ~ . , data=df)
  boot_beta_intervals(mod)
})
beta_intervals_list


# --------------------------------------------------------------------- #
### French Fries experiment

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

### In the main effects you will find that coefficients represent the 2-by-3 structure
# where we have {start, end} by {oil1, oil2, oil3} represented. 

# 2.a. Conduct a permutation test for pairwise differences between 
# the estimated starting scores (time=1) for the potato flavors for two oil types.

# Function to permute the data WITHIN blocks for subject and time. 
# Thus permuting the 6 treatments labels for the rows from a specific a week and person. 
perm_fries <- function(ff_ba){
  for(person in unique(ff_ba$subject)){
    for(week in unique(ff_ba$time)){
      block_idx <- which(ff_ba$subject==person & ff_ba$time==week)
      ff_ba$treatment[block_idx]  <- sample(ff_ba$treatment[block_idx])
    }
  }
  return(ff_ba)
}
head(perm_fries(ff_before_after))

# gather a set of M permuatation based betas from the specified model that can be used for testing
M=1000
perm_betas <- sapply(1:M, function(i) {
    tmp_df <- perm_fries(ff_before_after)  
    mod <- lme4::lmer(potato ~ (-1) + time:treatment + (1|subject), data=tmp_df)
    mod@beta
  }) 

# Want to compare beta1-beta3, beta1-beta5, beta3-beta5
beta_pairs <- list(c(1,3), c(1,5),c(3,5))

sapply(beta_pairs, function(pair){
  real_diff <- diff(mixed_mod@beta[pair])
  perm_diffs <- apply(perm_betas[pair, ], 2, diff)
  pval <- mean(abs(perm_diffs) >= abs(real_diff))
  return(pval)
})

# It appears that none of the p-values suggest differences in starting Potato flavor across the three oils



# 2.b. Create permutation-based 95% confidence intervals for the change in flavor before and after
# for each of the three oils. In other words, create permutation-based confidence intervals for beta2, beta4 and beta6. 
sapply(c(2,4,6), function(beta){
  perm_diffs <- quantile(perm_betas[beta, ], c(0.025, 0.975))
})



# --------------------------------------------------------------------- #

