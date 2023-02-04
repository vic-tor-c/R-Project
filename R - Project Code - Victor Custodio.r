> #Read data sets.
> 
> testData <- read.csv("testset.csv")
> trainData <- read.csv("trainset.csv")
> 
> #Converting data format to factor.
> 
> testData[] <- lapply(testData, factor)
> trainData[] <- lapply(trainData, factor)
> 
> View(testData)
> View(trainData)
> 
> #--------------------------------------------------------------------
>
> #Removing "unknown" values and calculating % of data loss.
> #Clean and remove any rows where column value is "unknown".
> 
> # Counting original number of rows before cleaning data.
> nrows_test <- nrow(testData)
> nrows_train <- nrow(trainData)
> 
> #Removing rows with "unknown" value in any column.
>
> testData <- subset(testData, job != "unknown" & marital != "unknown" & education != "unknown" & housing != "unknown" & loan != "unknown")
> trainData <- subset(trainData, job != "unknown" & marital != "unknown" & education != "unknown" & housing != "unknown" & loan != "unknown")
> 
> #Counting the number of rows in each data set after cleaning.
> 
> nrows_test_clean <- nrow(testData)
> nrows_train_clean <- nrow(trainData)
> 
> #Calculating the % of data that is lost after cleaning.
>
> nrows_test_clean/nrows_test
[1] 0.9286733
> nrows_train_clean/nrows_train
[1] 0.9284958
>
> #About 7-8% of data is lost in each data set.
>
> #--------------------------------------------------------------------
>
> #Looking at the information gain of each attribute.
>
> install.packages("FSelector")
> library(FSelector)
>
> formula_all <- Subscribed ~ .
> 
> weights <-information.gain(formula_all, data = trainData)
> 
> weights
>
> #--------------------------------------------------------------------
>
> #Decision Tree Model
>
> install.packages("partykit")
> library(partykit)
>
> install.packages("RWeka")
> library(RWeka)
>
> # Using 3 attributes: job, poutcome, month
>
> actualPredict <- testData$Subscribed
> formula_1 <- Subscribed ~ job + poutcome + month
> tree_1 <- ctree(formula_1, data = trainData)
> plot(tree_1)
> testPredict <- predict(tree_1, newdata = testData)
> table (testPredict, actualPredict)
>
> #Calculating accuracy
>
> mean(testPredict == testData$Subscribed)
>
> #Confusion Matrix
>
> install.packages("caret")
> install.packages("e1071", dependencies = TRUE)
> library(caret)
> library(e1071)
> confusionMatrix(table(testPredict, actualPredict))
>
> # Precision and Recall
> 
> install.packages("ROCR")
> library(ROCR)
>
> pred <- prediction(as.numeric(testPredict), as.numeric(actualPredict))
> RP.perf <- performance(pred, "prec", "rec")
> plot(RP.perf)
>
> # ROC Curve
>
> ROC.perf <- performance(pred, "tpr", "fpr")
> plot(ROC.perf)
>
> # Area under ROC Curve
>
> ROC.perf <- performance(pred, "tpr", "fpr")
> auc.tmp <- performance(pred, "auc")
> auc <- as.numeric(auc.tmp@y.values)
> View(auc)
>
> #--------------------------------------------------------------------
>
> # Logistic Model
>
> #Read data sets.
>
> testData <- read.csv("testset.csv")
> trainData <- read.csv("trainset.csv")
>
> #Clean and remove any rows where column value is "unknown". 
>
> testData <- subset(testData, job != "unknown" & marital != "unknown" & education != "unknown" & housing != "unknown" & loan != "unknown")
> trainData <- subset(trainData, job != "unknown" & marital != "unknown" & education != "unknown" & housing != "unknown" & loan != "unknown")
> 
> # Convert Subscribed attribute values to 1 - yes, 0 - no.
>
> testData$Subscribed <- ifelse(testData$Subscribed == "yes", 1 ,0)
> trainData$Subscribed <- ifelse(trainData$Subscribed == "yes", 1 ,0)
>
> # Building the Logistic Model
>
> # Using 2 attributes: duration and campaign
>
> logitmod <- lm (Subscribed ~ duration + campaign, data = trainData)
> pred <- predict(logitmod, newdata = testData, type = "response")
>
> y_pred <- ifelse(pred > 0.5, 1, 0)
> View(y_pred)
>
> y_actual <- testData$Subscribed
> View(y_actual)
>
> #Calculating accuracy
>
> mean (y_pred == y_actual)
>
> install.packages ("caret")
> install.packages ("e1071", dependencies=TRUE)
> library(caret)
> library(e1071)
>
> #Confusion Matrix
>
> confusionMatrix(table(y_pred, y_actual))
>
> install.packages ("ROCR")
> library(ROCR)
>
> # Precision and Recall
>
> pred <- prediction(y_pred, y_actual)
> 
> RP.perf <- performance(pred, "prec", "rec")
> plot(RP.perf)
> 
> #ROC Curve
>
> ROC.perf <- performance(pred, "tpr", "fpr")
> plot(ROC.perf)
>
> # Area under ROC Curve
>
> ROC.perf <- performance(pred, "tpr", "fpr")
> auc.tmp <- performance(pred, "auc")
> auc <- as.numeric(auc.tmp@y.values)
> View(auc)
>