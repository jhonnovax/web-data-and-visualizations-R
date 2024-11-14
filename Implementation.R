#------------------------
# Get page information
#------------------------
library(pageviews)

banking_in_germany <- article_pageviews(project = "en.wikipedia", "Banking in Germany")
str(banking_in_germany)


#------------------------
# Get linkn information
#------------------------
library(httr)

url <- "https://analyst-2.ai/analysis/kaggle-german-credit-risk-d0f1/97838d73/"
pageview_response <- GET(url)
pageview_data <- content(pageview_response)
str(pageview_data)


#------------------------
# Get factors
#------------------------
library(sm)

data <- read.csv("Special Topics/Assignment 4/german_credit_data.csv")
head(data)

table(data$Housing)
housing_factor <- as.factor(data$Housing)
table(housing_factor)

table(data$Checking.account)
checking_account_factor <- factor(data$Checking.account)
table(checking_account_factor)

#------------------------
# Box and Density Plot
#------------------------
boxplot(Credit.amount ~ Housing * Checking.account,
        data=data,
        varwidth=TRUE,
        col=c('gold','darkgreen','blue'),
        main="Credit Amount vs Housing & Checking Account",
        xlab="Housing Type",
        ylab="Credit Amount")

sm.density.compare(data$Credit.amount, data$Housing)
title(main="Credit Amount by Housing Type")


#------------------------
# Partitioning
#------------------------
library(randomForest) 
library(rpart) 
library(rpart.plot) 
library(caret) 
library(Metrics) 
library(ipred) 
library(caTools)

# Deleting index column
data <- data[,-1] 

# Handling NA values
data <- data.frame(lapply(data, function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
  } else if (is.factor(x) || is.character(x)) {
    x[is.na(x)] <- as.character(stats::na.omit(x)[which.max(table(x))])
    x <- as.factor(x)
  }
  return(x)
}))

# Converting remaining categorical variables to factor
data$Risk <- as.factor(data$Risk)
data$Sex <- as.factor(data$Sex)
data$Saving.accounts <- as.factor(data$Saving.accounts)
data$Purpose <- as.factor(data$Purpose)

head(data) 
set.seed(100) 
sample_split <- sample.split(Y = data$Risk, SplitRatio = 0.7) 
train_set <- subset(x = data, sample_split == TRUE) 
test_set <- subset(x = data, sample_split == FALSE) 

#------------------------
# Decision Tree 
#------------------------
dt_model <- rpart(Risk ~ ., data = train_set, method = "class") 
dt_pred <- predict(dt_model, test_set[,-10], type = "class") 
confusionMatrix(test_set$Risk, dt_pred) 
accuracy(test_set$Risk , dt_pred) 
rpart.plot(dt_model) 

#------------------------
# Random Forest 
#------------------------
rf_model <- randomForest(Risk ~ ., data = train_set, ntree=50, ntry=3, importance=TRUE, na.action=randomForest::na.roughfix, replace=FALSE) 
rf_pred <- predict(rf_model, newdata = test_set[,-10])
confusionMatrix(test_set$Risk, rf_pred)
accuracy(test_set$Risk , rf_pred)
varImpPlot(rf_model, col=3)





