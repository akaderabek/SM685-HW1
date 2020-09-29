library(faraway)
library(ggplot2)
library(tidyverse)
library(janitor)


# 1. Use teengamb in Faraway Chapter 2. Exercise 1. [10pt]
data("teengamb", package = "faraway")
df <- teengamb

# 1.1 Estimate the mean of expenditure on gambling by sex and the standard errors. [4pt]

  #subset males
dfm <- subset(df, df$sex == 0)
  #mean expenditure on gambling for males
mean(dfm$gamble, na.rm=T)
  #Standard deviation for male expenditure on gambling 
sd(dfm$gamble, na.rm=T)
  #Variance for male expenditure (sig squared)
var(dfm$gamble, na.rm=T)

  #subset females
dff <- subset(df, df$sex == 1)
  #mean expenditure on gambling for females
mean(dff$gamble)
  #Standard deviation for female expenditure on gambling 
sd(dff$gamble, na.rm=T)
  #Variance for female expenditure (sig squared)
var(dff$gamble)

# 1.2 Test the difference in expenditure on gambling between two sexes. What do you conclude? [6pt]
  # Ho == Ha "Gambling expenditures for males are equal to gambling expenditures of females"
  # Ho != Ha "Gambling expenditures for males are not equal to gambling expenditures of females"
  # We can reject the null hypothesis because we see the estimates of variance are markedly different.

# 2. Faraway Chapter 2. 
# Exercise 1. [28pt]
  # Fit a regression model with the expenditure on gambling as the response and sex, status, 
  # income and verbal score as predictors. Present the output.

# Answer:
GamReg <- lm(gamble~sex+status+income+verbal, df)
summary(GamReg)
plot(GamReg)

# 2.1 Interpret each of the regression coefficient estimates. [5pt]
  
#Answer:
# The model intercept is 22.55565, this would be the predicted value of the model if the value of all other variables was 0.
  
# The regression coefficient for sex is -22.11833 and is significant at the .05 level. This indicates for individuals who 
  # are female (x=1) there will be a negative association, or we could say we predict the expenditures for males to be 22.11833
  # higher than females with all other predictors equal.

# The regression coefficient for status is 0.05223 but is insignificant.

# The regression coefficient for income is 4.96198 and is significant at the .001 level. This indicates a positive association
  # and we would predict for every one unit increase in income we will see a 4.96198 increase in gambling expenditure.
  
# The regression coefficient for verbal is -2.95949 but is insignificant.


# 2.2 What do you observe in #2.(1) in relations to #1.(2)? [5pt]

# 2.3 (b) Which observation has the largest positive residual? Give the case number. [2pt]
  # Answer: Case 24 has the highest residual

# 2.4 (c) Compute the mean and median of the residuals...& Describe your observation. [2pt]
Resid_GamReg <- resid(GamReg)
Resid_GamReg
sum(Resid_GamReg)/47

# 2.5 (d) Compute the correlation of the residuals with the fitted values...& Describe your observation. [2pt]

# 2.6 (e) Compute the correlation of the residuals with income...& Describe your observation. [2pt]

# 2.7 What is the total sum of squares in the response variable corrected for its mean? How about residual sum of squares? [5pt]

# 2.8 Compute R2 by “hand”. How does this compare to R2 in the lm output? [5pt]

# 3. Faraway Chapter 2.
  # Exercise 7. [17pt]
  # An experiment was conducted to determine the effect of four factors on the resistivity of a semiconductor wafer.
  # The data is found in wafer where each of the four factors is coded as - or + depending on whether the low or high setting
  # for that factor was used. Fit the linear model resist ~ x1 + x2 + x3 + x4.


# 3.1 (a) Extract the X matrix using the model.matrix function. 
  # Examine this to determine how the low and high levels have been coded in the model [2pt]

# 3.2 (b) Compute the correlation in the X matrix. 
  # Why are there some missing values in the matrix? [5pt]


# 3.3 (c) What difference in resistance is expected when moving from the low to the high level of x1?...& Describe your observation. [5pt]

# 3.4 (d) Refit the model without x4 and examine the regression coefficients and standard errors?
  # What stayed the same as the original fit and what changed?...& Describe your observation. [5pt]

