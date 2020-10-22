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


# Remove "id" and change class "diagnosis" to factor, rename "diagnosis" to "resp"
BCancer2 <- BCancer %>% select(-"id") %>%
    mutate( diagnosis=as.factor(ifelse(diagnosis=="M", "Yes", "No") )) %>%
    rename(resp=diagnosis)
BCancer2  # resp variable is factor

dim(BCancer2)
# [1] 569  31


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
# mean_radius             V01  2    "numeric"**
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
# worst_radius            V21  22   "numeric"**
# worst_texture           V22  23   "numeric"
# worst_perimeter         V23  24   "numeric"
# worst_area              V24  25   "numeric"
# worst_smoothness        V25  26   "numeric"
# worst_compactness       V26  27   "numeric"
# worst_concavity         V27  28   "numeric"
# worst_concave_points    V28  29   "numeric"**
# worst_symmetry          V29  30   "numeric"
# worst_fractal_dimension V30  31   "numeric"





###--------------------------------------------------------------------
###--- 0b. Divide Dataset to Training and Testing and Set up k fold CV
Orig <- BCancer3             # Entire Data set (have to be data.frame)
resp.col.name <- "resp"      # name of response column
train.size <- 500            # num of rows for training set
test.size <-  96             # num of rows for testing set
num.folds <- 5               # k for k-fold CV
my.seed <- 55622             # give a seed

    #---
    set.seed(my.seed)
    ix = sample(1:nrow(Orig))
    Orig2 = Orig[ix, ]
    Train.set  = Orig2[1:train.size, ]
    Train.resp = Orig2[1:train.size, resp.col.name]
    Test.set   = Orig2[(train.size+1):(train.size+test.size), ]
    Test.resp  = Orig2[(train.size+1):(train.size+test.size), resp.col.name]

    # K-fold Cross Validation
    library(cvTools)     # install.packages("cvTools")
    set.seed(my.seed)
    folds = cvFolds(  nrow(Train.set),  K=num.folds  )  # k-fold CV (random assignment)

    CV.train      = list(Train.set[ folds$which!=1, ])
    CV.train.resp = list(Train.resp[folds$which!=1,1])
    CV.valid      = list(Train.set[ folds$which==1, ])
    CV.valid.resp = list(Train.resp[folds$which==1,1])

    for (k in 2:num.folds) {
      CV.train[[k]]      = Train.set[ folds$which!=k, ]
      CV.train.resp[[k]] = Train.resp[folds$which!=k,1]
      CV.valid[[k]]      = Train.set[ folds$which==k, ]
      CV.valid.resp[[k]] = Train.resp[folds$which==k,1]
    }
# Output (all data.frame):
#   Train.set      /  Train.resp
#   Test.set       /  Test.resp
#   CV.train[[k]]  /  CV.train.resp[[k]]
#   CV.valid[[k]]  /  CV.valid.resp[[k]]









###-------------------------------------------------
###--- 1. Logistic Regression on Training Set


##--- All features
Fit01 <- glm(resp~ V01 + V28, family=binomial, data=Train.set )
#Fit01 <- glm(resp~ V21 + V28, family=binomial, data=Train.set )
summary(Fit01)

coef(Fit01)


#- Extract responses (training and testing)
Train.prob =predict(Fit01, type ="response")  # fitted responses
Test.prob = predict(Fit01, newdata=Test.set, type="response")



#- Pick a threshold
threshold = .1

#- Check the training set accuracy
Train.pred = ifelse(Train.prob > threshold, "Yes", "No")  # Turn the fitted values to Up/Down using threshold of .5
Test.pred  = ifelse(Test.prob  > threshold, "Yes", "No")
CM.train <- caret::confusionMatrix(factor(Train.pred), factor(as.matrix(Train.resp)), positive="Yes")
CM.test  <- caret::confusionMatrix(factor(Test.pred),  factor(as.matrix(Test.resp)), positive="Yes")
CM.train
CM.test


layout(matrix(1:2, 1, 2))
#- Training Set
pROC::plot.roc(factor(as.matrix(Train.resp)),  Train.prob, levels=c("No", "Yes"))
  auc.train = pROC::auc(factor(as.matrix(Train.resp)), Train.prob, levels=c("No", "Yes"))
  text(.2, .2, paste("AUC=",round(auc.train, 3)))

#- Test Set
pROC::plot.roc(factor(as.matrix(Test.resp)),  Test.prob, levels=c("No", "Yes"))
  auc.test = pROC::auc(factor(as.matrix(Test.resp)), Test.prob, levels=c("No", "Yes"))
  text(.2, .2, paste("AUC=",round(auc.test, 3)))
layout(1)
c(auc.train, auc.test)








###-----------------------------------------------------------
###--- Final Model Fit to Entire Training Set
Fit03 <- glm(resp~ V01 + V28, family=binomial, data=Train.set )
summary(Fit03)

#- Predict on Test set
Test.prob = predict(Fit01, newdata=Test.set, type="response")





###-----------------------------------------------------------
###--- Threshold Picker


###--- Threshold Picker

#cost.list = c(1,1,1,1)/4           # order of (TP, TN, FP, FN)
cost.list = c(0,0,3,1)/4           # order of (TP, TN, FP, FN)
#cost.list = c(0,0,1,1)/2           # order of (TP, TN, FP, FN)
#cost.list = c(0,0,1,2)/3           # order of (TP, TN, FP, FN)
#cost.list = c(0,0,1,3)/4           # order of (TP, TN, FP, FN)




threshold.list = seq(0.01,.99,.01)    # grid for threshold
cost=0
library(caret)      # for confusionMatrix
for (i in 1:length(threshold.list)){

    threshold = threshold.list[i]

    #- Check the training set accuracy
    Test.pred  = ifelse(Test.prob  > threshold, "Yes", "No")
    CM.test  <- confusionMatrix(factor(Test.pred),
                                factor(as.matrix(Test.resp)),
                                positive="Yes")
    TP = CM.test$table[2,2]   # True  Pos
    TN = CM.test$table[1,1]   # True  Neg
    FP = CM.test$table[2,1]   # False Pos
    FN = CM.test$table[1,2]   # False Neg

    cost[i] = sum(c(TP, TN, FP, FN) * cost.list)
}
plot(threshold.list, cost, xlab="threshold")

cost.list
which.min(cost)
min(cost)
threshold.list[which.min(cost)]
