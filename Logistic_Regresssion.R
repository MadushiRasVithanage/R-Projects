
library(tidyverse)
library(caret)
library(e1071)
library(ROCR)
## lOad data set


library(readr)
ChurnData <- read_csv("D:/STAT/Sem_3/MAS 5313 - Group Project/Churn_Data/ChurnData.csv")

library(readr)
ChurnData <- read_csv("D:/STAT/Sem_3/MAS 5313 - Group Project/Churn_Data/ChurnData.csv")
View(ChurnData)
summary(ChurnData)


## Distribution Analysis
numerical_cols <-c('tenure', 'MonthlyCharges', 'TotalCharges')
ChurnData %>%
  select(all_of(numerical_cols)) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ key, scales = 'free') +
  theme_minimal()

# Convert TotalCharges to numeric
ChurnData$TotalCharges <- as.numeric(ChurnData$TotalCharges)

# Replace NA values with the median of TotalCharges
ChurnData[is.na(ChurnData$TotalCharges), 'TotalCharges'] <- median(ChurnData$TotalCharges, na.rm = TRUE)
sum(is.na(ChurnData))
##Convert categorical variables to factors
ChurnData <- ChurnData %>%
  mutate_if(is.character, as.factor)
#################################################################################
################# Statistic Tests################################################
# Example: Test association between gender and churn
chisq.test(table(ChurnData$gender, ChurnData$Churn))
# Compare the tenure of customers who churn and those who do not
t.test(tenure ~ Churn, data = ChurnData)
#Compare the MonthlyCharges across different contract types
anova_model <- aov(MonthlyCharges ~ Contract, data = ChurnData)
summary(anova_model)



################################################################################################################################################
##Split data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(ChurnData$Churn, p = 0.8, list = FALSE)
trainData <- ChurnData[trainIndex,]
testData <- ChurnData[-trainIndex,]
##Standardize numerical features (optional but recommended)
numerical_cols <- c('tenure', 'MonthlyCharges', 'TotalCharges')
preProcValues <- preProcess(trainData[, numerical_cols], method = c("center", "scale"))
trainData[, numerical_cols] <- predict(preProcValues, trainData[, numerical_cols])
testData[, numerical_cols] <- predict(preProcValues, testData[, numerical_cols])
##Train a Logistic Regression Model
# Fit logistic regression model
logistic_model <- glm(Churn ~ ., data = trainData, family = binomial)
# Summary of the model
summary(logistic_model)

