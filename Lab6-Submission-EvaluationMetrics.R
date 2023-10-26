if (require("ggplot2")) {
  require("ggplot2")
} else {
  install.packages("ggplot2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## pROC ----
if (require("pROC")) {
  require("pROC")
} else {
  install.packages("pROC", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## dplyr ----
if (require("dplyr")) {
  require("dplyr")
} else {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# 1. Accuracy and Cohen's Kappa ----
## 1.a. Load the dataset ----
library(readr)

machine_readable_business_employment_data_jun_2023_quarter <- read_csv("machine-readable-business-employment-data-jun-2023-quarter.csv")

machine_readable_business_employment_data_jun_2023_quarter_freq <- machine_readable_business_employment_data_jun_2023_quarter$class
cbind(frequency =
        table(machine_readable_business_employment_data_jun_2023_quarter_freq),
      percentage = prop.table(table(machine_readable_business_employment_data_jun_2023_quarter_freq)) * 100)

train_index <- createDataPartition(train_index <- createDataPartition(heart_attack_dataset$class,
                                                                      p = 0.75,
                                                                      list = FALSE)
                                   machine_readable_business_employment_data_jun_2023_quarter_train <- machine_readable_business_employment_data_jun_2023_quarter[train_index, ]
                                   machine_readable_business_employment_data_jun_2023_quarter_test <- machine_readable_business_employment_data_jun_2023_quarter[-train_index, ]
                                   $class,
                                   p = 0.75,
                                   list = FALSE)
machine_readable_business_employment_data_jun_2023_quarter_train <- machine_readable_business_employment_data_jun_2023_quarter[train_index, ]
machine_readable_business_employment_data_jun_2023_quarter_test <- machine_readable_business_employment_data_jun_2023_quarter[-train_index, ]

train_control <- trainControl(method = "cv", number = 5)

set.seed(7)  
class_model_glm <-
  train(class ~ ., data = machine_readable_business_employment_data_jun_2023_quarter_train, method = "glm",
        metric = "Accuracy", trControl = train_control) 

print(class_model_glm)

machine_readable_business_employment_data_jun_2023_quarter_freq <- machine_readable_business_employment_data_jun_2023_quarter$class
cbind(frequency =
        table(machine_readable_business_employment_data_jun_2023_quarter_freq),
      percentage = prop.table(table(machine_readable_business_employment_data_jun_2023_quarter_freq)) * 100)

train_index <- createDataPartition(heart_attack_dataset$class,
                                   p = 0.8,
                                   list = FALSE)
machine_readable_business_employment_data_jun_2023_quarter_train <- machine_readable_business_employment_data_jun_2023_quarter[train_index, ]
machine_readable_business_employment_data_jun_2023_quarter_test <- machine_readable_business_employment_data_jun_2023_quarter[-train_index, ]

train_control <- trainControl(method = "cv", number = 10,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)

set.seed(7)
class_model_knn <-
  train(class ~ ., data = machine_readable_business_employment_data_jun_2023_quarter_train, method = "knn",
        metric = "ROC", trControl = train_control)

print(class_model_knn)

predictions <- predict(class_model_knn, machine_readable_business_employment_data_jun_2023_quarter_test[, 1:8])

print(predictions)
confusion_matrix <-
  caret::confusionMatrix(predictions,
                         machine_readable_business_employment_data_jun_2023_quarter_test[, 1:9]$class)

print(confusion_matrix)

predictions <- predict(class_model_knn, machine_readable_business_employment_data_jun_2023_quarter_test[, 1:8],
                       type = "prob")

print(predictions)

roc_curve <- roc(machine_readable_business_employment_data_jun_2023_quarter_test$class, predictions$neg)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for KNN Model", print.auc = TRUE,
     print.auc.x = 0.6, print.auc.y = 0.6, col = "blue", lwd = 2.5)

train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                              classProbs = TRUE,
                              summaryFunction = mnLogLoss)
set.seed(7)

Employment_model_cart <- train(class ~ ., data = machine_readable_business_employment_data_jun_2023_quarter, method = "rpart",
                          metric = "logLoss", trControl = train_control)

print(Employment_model_cart)

## StatsNZ & Tatauranga, A. (2023).Business Employment Data June Quater . https://www.stats.govt.nz/large-datasets/csv-files-for-download/ # nolint ----


