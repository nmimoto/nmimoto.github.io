###
###
### Boston Data (MASS) - Multiple Regression
###                         with 5-fold Cross Validation
###   ver 0.0.2
###
##################################################


###-------------------------------------------------
###--- 0. Preliminary

# Using Boston3 from https://nmimoto.github.io/R/boston0.txt
library(MASS)                # install.packages("MASS")
library(tidyverse)           # install.packages("tidyverse")
Boston <- as_tibble(Boston)
Boston

# Rename "medv" column as "resp" to streamline analysis.
Boston2 <- Boston %>% rename(resp=medv)
Boston2

# move "resp" columnm to 1st
Boston2 <- Boston2 %>% relocate(resp)
Boston2

# turn "chas" column into 0/1 factor
Boston3 <- Boston2 %>% mutate( chas=as.factor(chas) )
Boston3

# Importance List by Chisq test

  # top list:
  # 3  4  6 10
  # "zn" "indus" "nox"   "rad"

  # bottom list
  # c(7, 8, 9, 13)
  # "rm"    "age"   "dis"   "black"

  #1  resp    (dbl)   <- used to be "medv". Response variable (Y).
  #2  crim    (dbl)
  #3  zn      (dbl)
  #4  indus   (dbl)
  #5  chas    (factor)
  #6  nox     (dbl)
  #7  rm      (dbl)
  #8  age     (dbl)
  #9  dis     (dbl)
  #10 rad     (dbl)
  #11 tax     (dbl)
  #12 ptratio (dbl)
  #13 black   (dbl)
  #14 lstat   (dbl)




###-------------------------------------------------
###--- 1. Multiple Linear Regression


#- Regression with all variables
Reg01 <- lm(medv ~ . , data=Boston)
summary(Reg01)

plot(Reg01)



#- Backward stepwise regression
Reg02 <- lm(medv ~ . -age, data=Boston)
summary(Reg02)


Reg03 <- lm(medv ~ . -age-indus, data=Boston)
summary(Reg03)

plot(Reg03)


Reg04 <- lm(medv ~ crim + zn + indus, data=Boston)
summary(Reg04)



#--- Looking at the result
str(Reg03)  # click on environment tab in Rstudio

Reg03$residuals

plot(Reg03$residuals)
