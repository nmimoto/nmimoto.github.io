####
###
###  Default Data (ISLR) - Logistic Regession
###                                      ver 0.0.4
###
####################################################


###-------------------------------------------------------
###--- 0. Preliminary
library(ISLR)    # install.packages('ISLR', repos='https://cran.case.edu/')
head(Default)    # see the data
dim(Default)

#  ?Default  # see explanation for variables

#  A data frame with 10000 observations on the following 4 variables.
#
# 1 default (factor)  A factor with levels No and Yes indicating whether the customer defaulted on their debt
# 2 student (factor)  A factor with levels No and Yes indicating whether the customer is a student
# 3 balance (numeric) The average balance that the customer has remaining on their credit card after making their monthly payment
# 4 income  (numeric) Income of customer
#
# column "default" is the respoinse variable.


#--- Turn the data into tibble ---
library(tidyverse)            # install.packages("tidyverse")
Default <- as_tibble(Default)
Default

Default2 <- Default %>%
              rename(resp=default) %>%     # Rename "default" column as "resp"
              relocate(resp)               # move "resp" columnm to 1st
Default2






###-------------------------------------------------------
###--- 1. Data Separation (Copied from Rtut-CV)

### --- Divide Dataset to Training and Testing and Set up k fold CV
Orig <- Default2             # Entire Data set (have to be data.frame)
train.size <- 8500           # num of rows for training set
test.size <- 1500            # num of rows for testing set
my.seed <- 8346              # give a seed

    #---AAAA---
    resp.col.name <- "resp"   # name of response column
    num.folds <- 5               # k for k-fold CV

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
    #---BBBB---

###  Line below will replace above chunk (#---AAAA to #---BBBB)
###  source('https://nmimoto.github.io/R/ML-00.txt')
###

    # Output (all data.frame):
    #   Train.set
    #   Train.resp
    #   Test.set
    #   Test.resp
    #   CV.train[[k]]
    #   CV.train.resp[[k]]
    #   CV.valid[[k]]
    #   CV.valid.resp[[k]]





###-------------------------------------------------------
###--- 3 Logistic Regression on Training Set


###----------------------------
###--- 3.1 Model1 Just balance
Fit01 <- glm(resp ~ balance,  family=binomial, data=Train.set)
summary(Fit01)

    #--- Plot the result for visualization
    plot(Train.set$balance, Train.set$resp=="Yes", ylab="Resp", xlab="Balance")
    lines(Train.set$balance, Fit01$fitted, lwd=2, col="red", type="p")


###----------------------------
###--- 3.2 Model2 All three
Fit02 <- glm(resp ~ .,  family=binomial, data=Train.set)
summary(Fit02)

coef(Fit02)
summary(Fit02)$coef        # extract estimated parameters
summary(Fit02)$coef[,4]   # extract 4th column (P-value) of above

    #--- Plot the result for visualization
    plot(Train.set$balance, Train.set$resp=="Yes", ylab="Resp", xlab="Balance")
    lines(Train.set$balance, Fit02$fitted, lwd=2, col="red", type="p")


###----------------------------
###--- 3.3 Model3 Remove income
Fit03 <- glm(resp ~ balance + student,  family=binomial, data=Train.set)
summary(Fit03)

    #--- Plot the result for visualization
    plot(Train.set$balance, Train.set$resp=="Yes", ylab="Resp", xlab="Balance")
    lines(Train.set$balance, Fit03$fitted, lwd=2, col="red", type="p")
                              # two red lines are for Student= Yes/No
    ix = (Train.set$student=="Yes")
    lines(Train.set$balance[ix], Fit03$fitted[ix], lwd=2, col="blue", type="p")

    #- Extract fitted response (training)
    Train.prob =predict(Fit03, type ="response")
    head(Train.prob)

    #- Predict in Test Set
    Test.prob = predict(Fit03, newdata=Test.set, type="response")
    head(Test.prob)


    ###----------------------------
    #- 3.3a Output result for given threshold value
    threshold = .9   # pick a threshold

    #- Check the training set accuracy
    library(caret)
    Train.pred = ifelse(Train.prob > threshold, "Yes", "No")  # Turn the fitted values to Up/Down using threshold of .5
    Test.pred  = ifelse(Test.prob  > threshold, "Yes", "No")
    CM.train <- confusionMatrix(factor(Train.pred), factor(as.matrix(Train.resp)), positive="Yes")
    CM.test  <- confusionMatrix(factor(Test.pred),  factor(as.matrix(Test.resp)),  positive="Yes")

    CM.train            # Training set result
    CM.train$table      # output just the table

    CM.train[["byClass"]][["Sensitivity"]]
    CM.train[["byClass"]][["Specificity"]]

    CM.test             # Testing set
    CM.test$table      # output just the table

    # Test set result
    #               Reference
    # Prediction      No Yes
    #        No     1437  62         [Specificity][  ]  = TrueNeg / sum of col
    #        Yes       0   1         [  ][Sensitivity]  = TruePos / sum of col

    colSums(CM.test$table) / sum(colSums(CM.test$table))    # % of Actual Yes/No
    rowSums(CM.test$table) / sum(rowSums(CM.test$table))    # % of predicted Yes/No


    ###----------------------------
    #- 3.4b Output ROC curve and AUC for all threshold
    library(pROC)
    #- Training Set
    plot.roc(factor(as.matrix(Train.resp)),  Train.prob, levels=c("No", "Yes"))
    # point corresponding to CM.train
    abline(h=CM.train[["byClass"]][["Sensitivity"]], v=CM.train[["byClass"]][["Specificity"]], col="red")
    auc.train = auc(factor(as.matrix(Train.resp)), Train.prob, levels=c("No", "Yes"))
    text(.2, .2, paste("Train AUC=",round(auc.train, 3)))








###-------------------------------------------------------
###--- 4 Logistic Regression with CV

AUCs <- MSE.valid <- matrix(0, 5, 2)
colnames(AUCs) = c("Train AUC", "Valid AUC")
for (k in 1:5) {

    Fit00 <- glm(resp ~. , family=binomial, data=CV.train[[k]])     # <----- Change model here

    #- Extract fitted response (training)
    Train.prob =predict(Fit00, type ="response")  # fitted responses
    #- Predict in Validation Set
    Valid.prob = predict(Fit00, newdata=CV.valid[[k]], type="response")


    #- Check the training set accuracy
    library(caret)
    Train.pred = ifelse(Train.prob > threshold, "Yes", "No")  # Turn the fitted values to Up/Down using threshold of .5
    Valid.pred = ifelse(Valid.prob > threshold, "Yes", "No")
    #CM.train <- confusionMatrix(factor(Train.pred),  factor(as.matrix(CV.train.resp[[k]])), positive="Yes")
    #CM.valid  <- confusionMatrix(factor(Valid.pred), factor(as.matrix(CV.valid.resp[[k]])), positive="Yes")

    AUCs[k,] <- round(c(auc(factor(as.matrix(CV.train.resp[[k]])), Train.prob, levels=c("No", "Yes")),
                        auc(factor(as.matrix(CV.valid.resp[[k]])), Valid.prob, levels=c("No", "Yes"))), 4)

}
AUCs

Av.AUCs = apply(AUCs, 2, mean)
Av.AUCs


###
###   Make decision about best model based on Av Valid AUC
###





###----------------------------
###--- 5 Final Training/Test fit using best model

### Best model
Fit03 <- glm(resp ~ balance + student,  family=binomial, data=Train.set)
summary(Fit03)


###---
Fit00 = Fit03

#- Extract fitted response (training)
Train.prob =predict(Fit00, type ="response")
head(Train.prob)

#- Predict in Test Set
Test.prob = predict(Fit00, newdata=Test.set, type="response")
head(Test.prob)


###----------------------------
#- 5.1a Output result for given threshold value
threshold = .9   # pick a threshold

    #- Check the training set accuracy
    library(caret)
    Train.pred = ifelse(Train.prob > threshold, "Yes", "No")  # Turn the fitted values to Up/Down using threshold of .5
    Test.pred  = ifelse(Test.prob  > threshold, "Yes", "No")
    CM.train <- confusionMatrix(factor(Train.pred), factor(as.matrix(Train.resp)), positive="Yes")
    CM.test  <- confusionMatrix(factor(Test.pred),  factor(as.matrix(Test.resp)),  positive="Yes")

    CM.train            # Training set result
    CM.train$table      # output just the table

    CM.train[["byClass"]][["Sensitivity"]]
    CM.train[["byClass"]][["Specificity"]]

    CM.test             # Testing set
    CM.test$table      # output just the table

    # Test set result
    #               Reference
    # Prediction      No Yes
    #        No     1437  62         [Specificity][  ]  = TrueNeg / sum of col
    #        Yes       0   1         [  ][Sensitivity]  = TruePos / sum of col

    colSums(CM.test$table) / sum(colSums(CM.test$table))    # % of Actual Yes/No
    rowSums(CM.test$table) / sum(rowSums(CM.test$table))    # % of predicted Yes/No


###----------------------------
#- 5.1b Output ROC curve and AUC for all threshold
library(pROC)
    #- Training Set
    plot.roc(factor(as.matrix(Train.resp)),  Train.prob, levels=c("No", "Yes"))
    # point corresponding to CM.train
    abline(h=CM.train[["byClass"]][["Sensitivity"]], v=CM.train[["byClass"]][["Specificity"]], col="red")
    auc.train = auc(factor(as.matrix(Train.resp)), Train.prob, levels=c("No", "Yes"))
    text(.2, .2, paste("Train AUC=",round(auc.train, 3)))

    #- Test Set
    plot.roc(factor(as.matrix(Test.resp)),  Test.prob, levels=c("No", "Yes"))
    # point corresponding to CM.test
    abline(h=CM.test[["byClass"]][["Sensitivity"]], v=CM.test[["byClass"]][["Specificity"]], col="red")
    auc.test = auc(factor(as.matrix(Test.resp)), Test.prob, levels=c("No", "Yes"))
    text(.2, .2, paste("Test AUC=",round(auc.test, 3)))







###-------------------------------------------------------
###--- 6 Determine the Best Threshold

#--- Decide on Threshould penalty
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
