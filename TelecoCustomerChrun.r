churn <- read.csv(file.choose(), na.strings = "")
View(churn)
library(dplyr)
library(tidyverse)

#________________________________Cleaning___________________________________#

#Checking for NA's
summary(churn)
churn <- na.omit(churn)
churn = churn[,c(-1)]
#Changing Data Type
churn$MonthlyCharges <- as.double(churn$MonthlyCharges)
churn$TotalCharges <- as.double(churn$TotalCharges)
View(churn)
#0utliers

outliers <- function(dataframe){
  dataframe %>%
    select_if(is.numeric) %>% 
    map(~ boxplot.stats(.x)$out) 
  
  
}
z <- outliers(churn)
z <- t(as.matrix(z))
z

#Cleaning data
churn$Contract <- gsub("-", "", churn$Contract) 
churn$Contract <- gsub(" ", "", churn$Contract)
churn$Contract <- tolower(churn$Contract)

churn$PaymentMethod <- tolower(churn$PaymentMethod)
churn$PaymentMethod <- gsub(" ", "", churn$PaymentMethod)
churn$PaymentMethod <- gsub("automatic|[[:punct:]]", "", churn$PaymentMethod)

new.function <- function(a, b="No internet service")
{
  c <- gsub(b, "no", a)
  tolower(c)
}

churn$MultipleLines <- new.function(churn$MultipleLines,"No phone service")
churn$OnlineSecurity <- new.function(churn$OnlineSecurity)
churn$OnlineBackup <- new.function(churn$OnlineBackup)
churn$DeviceProtection <- new.function(churn$DeviceProtection)
churn$TechSupport <- new.function(churn$TechSupport)
churn$StreamingTV <- new.function(churn$StreamingTV)
churn$StreamingMovies <- new.function(churn$StreamingMovies)
churn$Partner <- new.function(churn$Partner)
churn$Dependents <- new.function(churn$Dependents)
churn$PhoneService <- new.function(churn$PhoneService)
churn$PaperlessBilling <- new.function(churn$PaperlessBilling)
churn$Churn <- new.function(churn$Churn)

churn$SeniorCitizen <- ifelse(churn$SeniorCitizen == 0, "n", "y")

#Subset
male_cust <- churn[which(churn$gender=='Male'),]
female_cust <- churn[which(churn$gender=='Female'),]
elder_male <- male_cust[which(male_cust$SeniorCitizen=='y'),]
elder_Female <- female_cust[which(female_cust$SeniorCitizen=='y'),]


#_____________________________________Modelling_______________________________________#

#_________________________________Logistic Model_____________________________________#
#Creating Dummy Variables
install.packages("fastDummies")
library(fastDummies)
results <- fastDummies::dummy_cols(churn)
knitr::kable(results)
View(results)
churn_dummy <- results[,c(5,18:56)]
names(churn_dummy)[18] <- "InternetService_Fiber"

#.1 logistic Regression
table(churn_dummy$Churn)
str(churn_dummy)
churn_dummy$Churn <- ifelse(churn$Churn == 'no', 1, 0)
a <- colnames(churn_dummy)
a <- a[c(1,5:40)]
churn_dummy[a] <- sapply(churn_dummy[a], as.numeric)
View(churn_dummy)
#.1.2 Subsetting
churn_yes <- churn_dummy[which(churn_dummy$Churn==0),]
churn_no <- churn_dummy[which(churn_dummy$Churn==1),]

#.1.3 Training sets
set.seed(100)
train_rows_churn_yes <- sample(1:nrow(churn_yes), 0.7*nrow(churn_yes))
train_rows_churn_no <- sample(1:nrow(churn_no), 0.7*nrow(churn_no))
training_churn_yes <- churn_yes[train_rows_churn_yes, ]
training_churn_no <- churn_no[train_rows_churn_no, ]
trainingData <- rbind(training_churn_yes, training_churn_no)
View(trainingData)
#.1.4 Test data
test_churn_yes <- churn_yes[-train_rows_churn_yes, ]
test_churn_no <- churn_no[-train_rows_churn_no, ]
testData <- rbind(test_churn_yes, test_churn_no)

#.1.5 Build model
library(MASS)
model <- glm(Churn ~ ., data = trainingData, family = binomial(link="logit"))
stepAIC(model, direction = 'backward')
model <- glm(Churn ~ tenure + MonthlyCharges + TotalCharges + 
               SeniorCitizen_n + PhoneService_no + MultipleLines_no + InternetService_DSL + 
               InternetService_Fiber + OnlineBackup_no + DeviceProtection_no + 
               StreamingTV_no + StreamingMovies_no + Contract_monthtomonth + 
               Contract_oneyear + PaperlessBilling_no + PaymentMethod_electroniccheck, 
             family = binomial(link = "logit"), data = trainingData)
predicted <- predict(model, testData, type="response")
stepAIC(model, direction = 'backward')

#.1.6 Optimal cut off
install.packages("InformationValue")
library(InformationValue)
optCutOff <- optimalCutoff(testData$Churn, predicted)[1]
summary(model)

#.1.7 Comparison between actual vs predicted
misClassError(testData$Churn, predicted, threshold = optCutOff)

#.1.8 true positive detection using ROC
plotROC(testData$Churn, predicted)

#1.9 Confusion matrix
Concordance(testData$Churn, predicted)
p <- InformationValue::confusionMatrix(testData$Churn, predicted, threshold = optCutOff)

# Accuracy of the Model
accuracy = ((p[1,1] + p[2,2])/sum(p))*100
accuracy

#VIF
library(car)
vif(model) #Used to check multicollinearity

#k-cross validation
# Define training control
install.packages("e1071")
library(e1071)
library(caret)
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(Churn ~ tenure + MonthlyCharges + TotalCharges + 
                 SeniorCitizen_n + PhoneService_no + MultipleLines_no + InternetService_DSL + 
                 InternetService_Fiber + OnlineBackup_no + DeviceProtection_no + 
                 StreamingTV_no + StreamingMovies_no + Contract_monthtomonth + 
                 Contract_oneyear + PaperlessBilling_no + PaymentMethod_electroniccheck, data = trainingData, method = "glm",
               trControl = train.control)
# Summarize the results
print(model)

#________________Forward Selection______________________#
#.1.5 Build model
library(MASS)
model <- glm(Churn ~ ., data = trainingData, family = binomial(link="logit"))
stepAIC(model, direction = 'forward')
model <- glm(Churn ~ tenure + MonthlyCharges + TotalCharges + gender_Female + 
               SeniorCitizen_n + Partner_no + Dependents_no + PhoneService_no + 
               MultipleLines_no + InternetService_DSL + InternetService_Fiber + 
               OnlineSecurity_no + OnlineBackup_no + DeviceProtection_no + 
               TechSupport_no + StreamingMovies_no + Contract_monthtomonth + 
               Contract_oneyear + PaperlessBilling_no + PaymentMethod_banktransfer + 
               PaymentMethod_creditcard + PaymentMethod_electroniccheck, family = binomial(link = "logit"), data = trainingData)
predicted <- plogis(predict(model, testData))
stepAIC(model, direction = 'forward')

#.1.6 Optimal cut off
install.packages("InformationValue")
library(InformationValue)
optCutOff <- optimalCutoff(testData$Churn, predicted)[1]
summary(model)

#.1.7 Comparison between actual vs predicted
misClassError(testData$Churn, predicted, threshold = optCutOff)

#.1.8 true positive detection using ROC
plotROC(testData$Churn, predicted)

#1.9 Confusion matrix
Concordance(testData$Churn, predicted)
p <- InformationValue::confusionMatrix(testData$Churn, predicted, threshold = optCutOff)
# Accuracy of the Model
accuracy = ((p[1,1] + p[2,2])/sum(p))*100
accuracy

#VIF validation
vif(model)

#k-cross validation
# Define training control
install.packages("e1071")
library(e1071)
library(caret)
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(Churn ~ tenure + MonthlyCharges + TotalCharges + gender_Female + 
                 SeniorCitizen_n + Partner_no + Dependents_no + PhoneService_no + 
                 MultipleLines_no + InternetService_DSL + InternetService_Fiber + 
                 OnlineSecurity_no + OnlineBackup_no + DeviceProtection_no + 
                 TechSupport_no + StreamingMovies_no + Contract_monthtomonth + 
                 Contract_oneyear + PaperlessBilling_no + PaymentMethod_banktransfer + 
                 PaymentMethod_creditcard + PaymentMethod_electroniccheck, data = trainingData, method = "glm",
               trControl = train.control)
# Summarize the results
print(model)

#_________________________random Forest___________________________#

library(randomForest)

random_forest <- churn
s <- random_forest[,c(5,8,18,19)]
random_forest <- random_forest[,-c(5,8,18,19)]
View(random_forest)
c <- colnames(random_forest)
random_forest[c] <- lapply(random_forest[c], as.factor)
str(random_forest)
random_forest <- as.data.frame(random_forest)
random_forest <- cbind(s, random_forest)

set.seed(123)
index = sample(1:nrow(random_forest), size=0.7*nrow(random_forest))
train_data = random_forest[index,]
test_data = random_forest[-index,]

#Without tuning
model_r <- randomForest(Churn ~ ., data = train_data,importance = TRUE)

#optimal mtry
tune_r <- tuneRF(train_data[,-20], train_data[,20], stepFactor = 0.5, plot = TRUE, ntreeTry = 500, trace = TRUE, improve = 0.001)

#With Tuning
model_r <- randomForest(Churn ~ ., data = train_data, mtry = 2, importance = TRUE)

#Prediction for trainig set
predTrain <- predict(model_r, train_data, type = "class")
table(predTrain, train_data$Churn)

#Prediction for testin set
predtest <- predict(model_r, test_data, type = "class")
p_r <- table(predtest, test_data$Churn)
accuracy = ((p_r[1,1] + p_r[2,2])/sum(p_r))*100
accuracy

#Importance
importance(model_r)
varImpPlot(model_r)

#Plot
plot(model_r, main = "Forest Model with Tuning" )
legend("topright", colnames(model_r$err.rate), col = 1:3, fill=1:3)



# We will compare model 1 of Random Forest with Decision Tree model

model_dt <- train(Churn ~ ., data = train_data, method = "rpart")
model_dt_1 <- predict(model_dt, data = train_data)
table(model_dt_1, train_data$Churn)
mean(model_dt_1 == train_data$Churn)

model_dt_vs <- predict(model_dt, newdata = test_data)
table(model_dt_vs, test_data$Churn)

mean(model_dt_vs == test_data$Churn)
acc_r <- table(model_dt_vs, test_data$Churn)
accuracy = ((acc_r[1,1] + acc_r[2,2])/sum(acc_r))*100
accuracy


