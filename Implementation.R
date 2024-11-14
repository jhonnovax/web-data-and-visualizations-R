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
table(data$Checking.account)

housing_factor <- factor(data$Housing, levels = c(0,1,2), labels = c("rent", "own", "free"))
checking_account_factor <- factor(data$Checking.account, levels = c(0,1,2,3), labels = c('NA', 'little','moderate','rich'))

table(housing_factor)
table(checking_account_factor)

#------------------------
# Box and Density Plot
#------------------------
boxplot(data$Credit.amount ~ housing_factor * checking_account_factor,
        data=data,
        varwidth=TRUE,
        col=c('gold','darkgreen')
)
sm.density.compare(data$Credit.amount, data$Housing)


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

data <- data[,-1] 
head(data) 
set.seed(100) 
sample_split <- sample.split(Y = data$Risk, SplitRatio = 0.7) 
train_set <- subset(x = data, sample_split == TRUE) 
test_set <- subset(x = data, sample_split == FALSE) 

#------------------------
# Decision Tree 
#------------------------
origin_pred = test_set$Risk 
dt_model <- rpart(Risk ~ ., data = train_set, method = "class") 
dt_pred <- predict(dt_model, test_set[,-10], type = "class") 
confusionMatrix(factor(origin_pred), factor(dt_pred)) 
accuracy(origin_pred, dt_pred) 
rpart.plot(dt_model) 

#------------------------
# Random Forest 
#------------------------
origin_pred = train_set$Risk 
head(train_set)

# Convert categorical columns to factors
rf_train_set <- lapply(train_set, function(x) {
  if (is.character(x)) {
    as.factor(x)
  } else {
    x
  }
})
rf_test_set <- lapply(test_set, function(x) {
  if (is.character(x)) {
    as.factor(x)
  } else {
    x
  }
})
head(rf_train_set)
head(rf_test_set)

# Appling model
rf_model <- randomForest(Risk ~ ., data = rf_train_set, ntree=50, ntry=3, importance=TRUE, na.action=na.roughfix) 
rf_pred <- predict(rf_model, rf_test_set) 
rf_pred <- ifelse(rf_pred> 0.5,1,0) 
confusionMatrix(factor(origin_pred), factor(rf_pred)) 
accuracy(origin_pred, rf_pred) 
varImpPlot(rf_model, col=3) 




