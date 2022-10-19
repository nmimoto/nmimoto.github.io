###
###
###  Iris - Preliminary Analysis
###                      ver 0.0.4
###
####################################################





#-------------------------------------------------
###--- 0. Preliminary
library(datasets)     # install.packages('datasets', repos='https://cran.case.edu/')
data(iris)
summary(iris)
?iris


#--- Turn the data into tibble ---
library(tidyverse)            # install.packages("tidyverse")
Iris <- as_tibble(iris)
Iris


# table of columns
table(Iris$Species)     # Note that this is No/Yes instead of YES/no

# setosa  versicolor  virginica
#     50          50         50


# Turn "Species" column into Binary response
Iris1 <- Iris %>%
    mutate(Species=ifelse(Species=="setosa", "Yes", "No")) %>%
    mutate(Species=as.factor(Species)) %>%
    rename(resp=Species) %>%     # Rename "Species" column as "resp"
    relocate(resp)               # move "resp" columnm to 1st
Iris1


Iris2 <- Iris %>%
    mutate(Species=ifelse(Species=="versicolor", "Yes", "No")) %>%
    mutate(Species=as.factor(Species)) %>%
    rename(resp=Species) %>%     # Rename "Species" column as "resp"
    relocate(resp)               # move "resp" columnm to 1st
Iris2


Iris3 <- Iris %>%
    mutate(Species=ifelse(Species=="virginica", "Yes", "No")) %>%
    mutate(Species=as.factor(Species)) %>%
    rename(resp=Species) %>%     # Rename "Species" column as "resp"
    relocate(resp)               # move "resp" columnm to 1st
Iris3






#-------------------------------------------------
###--- 1. Routine Exploratory Analysis (class of resp should be "dbl")
Orig <- Iris2

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
