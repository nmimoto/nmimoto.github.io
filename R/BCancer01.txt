###
###
###  Breast Cancer Data - Logistic
###
###
####################################################


#-------------------------------------------------
###--- 0. Preliminary
library(tidyverse)
# load data from author's website
BCancer <- read_csv(file="https://nmimoto.github.io/datasets/BCancer.csv")
BCancer

# Show data class of each column
cbind(1:ncol(BCancer), as.matrix(map(BCancer, class)))


# Remove "id" and change class "diagnosis" to factor, rename "diagnosis" to "resp"
BCancer2 <- BCancer %>% select(-"id") %>%
    mutate( diagnosis=as.factor(ifelse(diagnosis=="M", "Yes", "No") )) %>%
    rename(resp=diagnosis)
BCancer2  # resp variable is factor


dim(BCancer2)
# [1] 569  31




####----------
###--- Routine Exploratory Analysis (resp should be factor)
Orig <- BCancer2
resp.col.name <- "resp"

    #- Check for N/A in data. Remove if there's any.
    summary(Orig)
    sum(is.na(Orig))
    # Orig <- Orig %>% na.omit()
    # dim(Orig)

    table(Orig[, resp.col.name])
    #  No Yes
    # 357 212

    ##----------
    ##- Correlation Check
    # Turn the response to numeric
    Orig2 <- Orig %>% mutate(resp=as.numeric(resp=="Yes"))

    # Pick columns that are numeric
    Orig2_num <- Orig2 %>% select_if(is.numeric)
    # cor(Orig2_num)
    # corrplot::corrplot(cor(Orig2_num), method="number")
    corrplot::corrplot(cor(Orig2_num))

    ##----------
    ##- Visualization

    # pairs(Orig) equivalent
    library(GGally)      # install.packages("GGally")
      GGally::ggpairs(Orig[, c(1,  2:6)],  aes(colour=factor(resp), alpha=1))
      GGally::ggpairs(Orig[, c(1,  7:11)], aes(colour=factor(resp), alpha=1))
      GGally::ggpairs(Orig[, c(1, 12:16)], aes(colour=factor(resp), alpha=1))
      GGally::ggpairs(Orig[, c(1, 17:21)], aes(colour=factor(resp), alpha=1))
      GGally::ggpairs(Orig[, c(1, 22:26)], aes(colour=factor(resp), alpha=1))
      GGally::ggpairs(Orig[, c(1, 27:31)], aes(colour=factor(resp), alpha=1))


    ###----------
    ###- Chi-sq test of association for each column

    # Pick columns that has more than 1 unique value
    list_cols <- Orig %>% summarise_all(function(x) length(unique(x))) %>%
        gather() %>% filter(value > 1) %>% pull(key)

    # Apply chisq.test to those columns
    ChiSq.pval <- Orig %>% select(list_cols) %>%
        summarise_all(funs(chisq.test(., Orig$resp)$p.value))

    ChiSq.pval
    which(ChiSq.pval < .05)             # Col num of variables w <.05 p-value
    list_cols[which(ChiSq.pval < .05)]  # Top list of 'important' variables

    which(ChiSq.pval > .6)              # Col num of variables w >.6 p-value
    list_cols[which(ChiSq.pval > .6)]   # Top list of 'unimportatnt' variables

### End of Routine Exploratory Analysis


##- Rename cols for convenience
BCancer3 <- BCancer2
names(BCancer3) <- c("resp",
      "V01", "V02", "V03", "V04", "V05", "V06", "V07", "V08", "V09", "V10",
      "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20",
      "V21", "V22", "V23", "V24", "V25", "V26", "V27", "V28", "V29", "V30")
BCancer3

cbind(1:ncol(BCancer2), as.matrix(map(BCancer2, class)))
cbind(1:ncol(BCancer3), as.matrix(map(BCancer3, class)))

# Conversion List         [,1] [,2]
# resp                    resp 1    "factor"
# mean_radius             V01** 2    "numeric"
# mean_texture            V02  3    "numeric"
# mean_perimeter          V03  4    "numeric"
# mean_area               V04  5    "numeric"
# mean_smoothness         V05  6    "numeric"
# mean_compactness        V06  7    "numeric"
# mean_concavity          V07  8    "numeric"
# mean_concave_points     V08  9    "numeric"
# mean_symmetry           V09  10   "numeric"
# mean_fractal dimension  V10  11   "numeric"
# radius_error            V11  12   "numeric"
# texture_error           V12  13   "numeric"
# perimeter_error         V13  14   "numeric"
# area_error              V14  15   "numeric"
# smoothness_error        V15  16   "numeric"
# compactness_error       V16  17   "numeric"
# concavity_error         V17  18   "numeric"
# concave_points_error    V18  19   "numeric"
# symmetry_error          V19  20   "numeric"
# fractal_dimension_error V20  21   "numeric"
# worst_radius            V21** 22   "numeric"
# worst_texture           V22  23   "numeric"
# worst_perimeter         V23  24   "numeric"
# worst_area              V24  25   "numeric"
# worst_smoothness        V25  26   "numeric"
# worst_compactness       V26  27   "numeric"
# worst_concavity         V27  28   "numeric"
# worst_concave_points    V28** 29   "numeric"
# worst_symmetry          V29  30   "numeric"
# worst_fractal_dimension V30  31   "numeric"
