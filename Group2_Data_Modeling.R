#Step 1 - load appr. library's
library(readxl)
library(sqldf)
library(eeptools) 
library(rpart) 
library(dplyr)
library(tidyverse)
library(lubridate)
library(neuralnet)
library(arules)
library(e1071)
library(gridExtra)
library(caret)
library(randomForest)
library(kernlab)
library(bindr)
library(kernlab)
library(caTools)

#Step 2 Get Data - build dataframe
dataset <- read_excel(path = "C:/Users/A256138/Documents/R/Applied_DataScience/Project/PremierLeague.xlsx",sheet = 'PremierLeague')

#Add Win/loss column  
dataset$win <- ifelse(dataset$GS>dataset$GA,1,0)

#Add Halftime Goal Diff column
dataset$HTdiff <- dataset$HTGS-dataset$HTGA

#Add Red+Yellow Cards (single variable)
dataset$red_yellow <- dataset$RedCards + dataset$YellowCards
dataset$opp_red_yellow <- dataset$OppRedCards + dataset$OppYellowCards


#Step 3 - Use logistic model to identify which variables we will pull into the model

#We are trying to identify which attributes about a soccer game drive wins and losses
#win = GS > GA, everything else is considered a loss

#Log model - All Variables except Row, Div, Date
logModel <- glm(formula=win ~ 
                  HomeAway
                + FTR
                + HTdiff
                + HTR
                + Referee
                + ShotsTaken
                + ShotsAllowed
                + ShotsOnTarget
                + OppShotsOnTarget
                + FoulsCommitted
                + FoulsReceived
                + CornerKicks
                + OppCornerKicks
                + YellowCards
                + OppYellowCards
                + RedCards
                + OppRedCards
                + WinOdds
                + DrawOdds
                + LossOdds
                + red_yellow
                + opp_red_yellow,
                data=dataset,family=binomial(logit))
summary(logModel)


#Condense down to remove first set of insignificant variables
logModel_condensed <- glm(formula=win ~ 
                            HTdiff
                          + ShotsOnTarget
                          + OppShotsOnTarget
                          + CornerKicks 
                          + OppCornerKicks
                          + RedCards
                          + OppRedCards,
                          data=dataset,family=binomial(logit))
summary(logModel_condensed) 

#Condense one more time as others have become insignificant (Opp corner kicks)

logModel_condensed2 <- glm(formula=win ~ 
                             GS
                           + HTdiff
                           + OppShotsOnTarget
                           + RedCards,
                           data=dataset,family=binomial(logit))
summary(logModel_condensed2) 

#Check R2 in Linear Model

LinearRegression <- lm(formula=win ~ 
                         + GS
                       + HTdiff
                       + OppShotsOnTarget
                       + RedCards,
                       data=dataset)
summary(LinearRegression) 


#step 4 - Build and test a handful of model techniques with significant variables from the logistic regression

#Create train and test data set
randIndex <- sample(1:dim(dataset)[1])

cutPoint2_3 <- floor(2* dim(dataset)[1]/3)  #creates training dataset

trainData <- dataset[randIndex[1:cutPoint2_3],]  #create training data set
testData <- dataset[randIndex[(cutPoint2_3+1):dim(dataset)[1]],]  #create test dataset


#convert win to factor so we can test if we're predicing win/loss
trainData$win <- as.factor(trainData$win)
testData$win <- as.factor(testData$win)


#KSVM Model
ksvmWin <- ksvm(win~ 
                  GS   
                + HTdiff
                + OppShotsOnTarget
                + RedCards,
                data=trainData,
                kernel="rbfdot",  
                kpar="automatic", 
                C=10,             
                cross=10,             
                prob.model=TRUE)

#check the model
ksvmWin

#Test the model
goodPred <- predict(ksvmWin,testData)                             
View(goodPred)


#create a dataframe to pull in actual wins and predicted
compGood1 <- data.frame(trainData[,30], goodPred)
colnames(compGood1) <- c("test", "Pred")

#Compute percent of correct cases
perc_ksvm <- length(which(compGood1$test==compGood1$Pred))/dim(compGood1)[1]
perc_ksvm
compGood1

#Confusion Matrix - ksvm
results <- table(test=compGood1$test, pred=compGood1$Pred)
print(results)


#Plot
#first determine the prediction is "correct" or "wrong" for each case
compGood1$correct <- ifelse(compGood1$test==compGood1$Pred,"correct","wrong")

Plot_ksvm <- data.frame(compGood1$correct, testData$GS, testData$HTdiff
                        ,testData$OppShotsOnTarget,testData$RedCards,compGood1$Pred)                                                       
#change column names
colnames(Plot_ksvm) <- c("correct","GS","HTdiff","OppShotsOnTarget","RedCards","Predict")
Plot.ksvm.good <- ggplot(Plot_ksvm, aes(x=GS, y=HTdiff))+
  geom_point(aes(size=correct, color=OppShotsOnTarget, shape=Predict))+
  ggtitle("ksvm - win/loss prediction")

Plot.ksvm.good










#SVM Model
svmWin <- svm(win~ 
                GS   
              + HTdiff
              + OppShotsOnTarget
              + RedCards,
              data=trainData,
              kernel="radial",
              C=10,cross=10,
              prob.model=TRUE) 

#check the model
svmWin

#Test the model
goodPred2 <- predict(svmWin,testData)                             
View(goodPred2)


#create a dataframe to pull in actual wins and predicted
compGood2 <- data.frame(trainData[,30], goodPred2)
colnames(compGood2) <- c("test", "Pred")

#Compute percent of correct cases
perc_svm <- length(which(compGood2$test==compGood2$Pred))/dim(compGood2)[1]
perc_svm





#Plot
#first determine the prediction is "correct" or "wrong" for each case
compGood2$correct <- ifelse(compGood2$test==compGood2$Pred,"correct","wrong")

Plot_svm <- data.frame(compGood2$correct, testData$GS, testData$HTdiff
                       ,testData$OppShotsOnTarget,testData$RedCards,compGood1$Pred)                                                       
#change column names
colnames(Plot_svm) <- c("correct","GS","HTdiff","OppShotsOnTarget","RedCards","Predict")
Plot.svm.good <- ggplot(Plot_svm, aes(x=GS, y=HTdiff))+
  geom_point(aes(size=correct, color=OppShotsOnTarget, shape=Predict))+
  ggtitle("svm - win/loss prediction")

Plot.svm.good



#naive bayes model           
nbGood <- naiveBayes(win~ 
                       GS   
                     + HTdiff
                     + OppShotsOnTarget
                     + RedCards,
                     data=trainData)

nbGood

goodPred3 <- predict(nbGood,testData)

#create a dataframe to pull in actual wins and predicted
compGood3 <- data.frame(trainData[,30], goodPred3)
colnames(compGood3) <- c("test", "Pred")

#Compute percent of correct cases
perc_nb <- length(which(compGood3$test==compGood3$Pred))/dim(compGood3)[1]
perc_nb


#Confusion Matrix - ksvm
results3 <- table(test=compGood3$test, pred=compGood3$Pred)
print(results3)                  


#Plot
#first determine the prediction is "correct" or "wrong" for each case
compGood3$correct <- ifelse(compGood3$test==compGood3$Pred,"correct","wrong")

Plot_nb <- data.frame(compGood3$correct, testData$GS, testData$HTdiff
                      ,testData$OppShotsOnTarget,testData$RedCards,compGood1$Pred)                                                       
#change column names
colnames(Plot_nb) <- c("correct","GS","HTdiff","OppShotsOnTarget","RedCards","Predict")
Plot.nb.good <- ggplot(Plot_nb, aes(x=GS, y=HTdiff))+
  geom_point(aes(size=correct, color=OppShotsOnTarget, shape=Predict))+
  ggtitle("naive bayes - win/loss prediction")

Plot.nb.good

str(dataset)

#Final Step - Compare 3 Models     

#Graphically - Combine KSVM, svm, NB models
grid.arrange(Plot.svm.good, Plot.ksvm.good, Plot.nb.good, newpage = TRUE)                  



#Confusion Matrix - Across KSVM, SVM, NB Models
ksvm_cm <- table(test=compGood1$test, pred=compGood1$Pred)
svm_cm <- table(test=compGood2$test, pred=compGood2$Pred)
nb_cm <- table(test=compGood3$test, pred=compGood3$Pred)

print(ksvm_cm)
print(svm_cm)
print(nb_cm)

#Decision Tree Model - Using final Variables from model


Treemodel <- rpart(
  win ~                                          
    GS   
  + HTdiff
  + OppShotsOnTarget
  + RedCards,
  data = dataset, 
  control = rpart.control(minsplit = 3))

par(xpd = NA, mar = rep(0.7, 4)) 
plot(Treemodel, compress = TRUE)
text(Treemodel, cex = 0.7, use.n = TRUE, fancy = FALSE, all = TRUE)   