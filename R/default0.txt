###
###
###  Default - Preliminary Analysis
###                      ver 0.0.4
###
####################################################



#-------------------------------------------------
###--- 0. Preliminary
library(ISLR)    # install.packages('ISLR', repos='https://cran.case.edu/')
head(Default)    # see the data
dim(Default)

?Default  # see explanation for variables

#  A data frame with 10000 observations on the following 4 variables.
#
# 1 default (factor)  A factor with levels No and Yes indicating whether the customer defaulted on their debt
# 2 student (factor)  A factor with levels No and Yes indicating whether the customer is a student
# 3 balance (numeric) The average balance that the customer has remaining on their credit card after making their monthly payment
# 4 income  (numeric) Income of customer
#
# column "default" is the respoinse variable.

dim(Default)
head(Default)    # see only first few rows
names(Default)   # list of column names

#--- Turn the data into tibble ---
library(tidyverse)            # install.packages("tidyverse")
Default <- as_tibble(Default)
Default

class(Default)
print(Default, n=100)      # if you want to see more rows

# table of columns
table(Default$default)     # Note that this is No/Yes instead of YES/no
table(Default$student)


Default2 <- Default %>%
              rename(resp=default) %>%     # Rename "default" column as "resp"
              relocate(resp)               # move "resp" columnm to 1st
Default2


#-------------------------------------------------
###--- 0.5 Preliminary Plots
Default
student
Default$student

attach(Default)
# Back to Default data (not Default2)
plot(balance, default)
plot(default, balance, xlab="Default", ylab="Balance")

plot(income, default)
plot(default, income, xlab="Default", ylab="Income")

plot(student, default, ylab="Default", xlab="Student")
plot(default, student, xlab="Default", ylab="Student")


plot(balance, income, col=ifelse((default=="Yes"), "red", "blue"),
                      pch=ifelse((default=="Yes"), 4, 3))
lines(balance[default=="Yes"], income[default=="Yes"], col="red",
      pch=4, type="p")









#-------------------------------------------------
###--- 1. Routine Exploratory Analysis (class of resp should be "dbl")
Orig <- Default2
resp.col.name <- "resp"

  #- Check for N/A in data. Remove if there's any.
  summary(Orig)
  sum(is.na(Orig))
  # If there is na in the data, run below
  dim(Orig)
  Orig <- Orig %>% na.omit()
  dim(Orig)


  ##----------
  ##- Correlation Check

  # Pick columns that are numeric
  library(corrplot)    # install.packages("corrplot")
    Orig_num <- Orig %>% select_if(is.numeric)
    cor(Orig_num)
    corrplot::corrplot(cor(Orig_num))
    corrplot::corrplot(cor(Orig_num), method="number")


  ##----------
  ##- Visualization

  # pairs(Orig) equivalent
  library(GGally)      # install.packages("GGally")
    GGally::ggpairs(Orig[, c(2:4, 1)], aes(color=resp, alpha=1))


  ###----------
  ###- Chi-sq test of association for each column

  # Pick columns that has more than 1 unique value
  list_cols <- Orig %>% summarise_all(function(x) length(unique(x))) %>%
        gather() %>% filter(value>1) %>% pull(key)

  # Apply chisq.test to those columns
  ChiSq.pval <- Orig %>% select(list_cols) %>%
        summarise_all(funs(chisq.test(., Orig$resp)$p.value))


  ChiSq.pval <- Orig %>% summarise_all(funs(chisq.test(., Orig$resp)$p.value))
  ChiSq.pval
  barplot(t(t(as.matrix(ChiSq.pval))), las=2, cex.names=1); abline(h=.05, col='red')

  which(ChiSq.pval < .05)             # Col num of variables w <.05 p-value
  list_cols[which(ChiSq.pval < .05)]  # Top list of 'important' variables

  which(ChiSq.pval > .6)              # Col num of variables w >.6 p-value
  list_cols[which(ChiSq.pval > .6)]   # Top list of 'unimportatnt' variables

### End of Routine Exploratory Analysis.
