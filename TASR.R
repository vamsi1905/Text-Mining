# Build a simple logistic regression model
simple_glm <- tasrml %>%
  select(-Respondent) %>%
  glm(STATUS ~ .,
      family = "binomial",
      data = .)

# Print the summary of the model
summary(simple_glm)

# Load caret
library(caret)

tasrml_select <- tasrml %>%
  select(-Respondent)

?createDataPartition
# Split the data into training and testing sets
set.seed(1234)
in_train <- createDataPartition(tasrml_select$STATUS, p = 0.8, list = FALSE)
training <- tasrml_select[in_train,]
testing <- tasrml_select[-in_train,]

# Upsampling
# There are multiple possible approaches to dealing with class imbalance. 
# Here, you will implement upsampling using caret's upSample() function.

# Use the training data set for upsampling, both for x (the predictors) and 
# y (the class memberships).
# The label for the class column goes in yname; remember that it is "Status".

up_train <- upSample(x = select(training, -STATUS),
                     y = training$STATUS,
                     yname = "STATUS") %>%
  as_tibble()

up_train %>%
  count(STATUS)

# Build a logistic regression model
tasrml_glm <- train(STATUS ~ ., method = "glm", family = "binomial",
                   data = training,
                   trControl = trainControl(method = "boot",
                                            sampling = "up"))

# Build a random forest model
tasrml_rf <- train(STATUS ~ ., method = "rf", family = "binomial",
                   data = training,
                   trControl = trainControl(method = "boot",
                                            sampling = "up"))
# Print the model object 
tasrml_glm
tasrml_rf

# Classification model metrics
# The confusionMatrix() function is helpful but often you want to store specific performance estimates for later, 
# perhaps in a dataframe-friendly form. The yardstick package is built to handle such needs. 
# For this kind of classifier model, 
# you might look at the positive or negative predictive value or perhaps overall accuracy.

# Confusion matrix for logistic regression model
confusionMatrix(predict(tasrml_glm, testing),
                testing$STATUS)



# Load yardstick
library(yardstick)

# Predict values
testing_results <- testing %>%
  mutate(`Logistic regression` = predict(tasrml_glm, testing),
         `Random forest` = predict(tasrml_rf, testing))

## Calculate accuracy
accuracy(testing_results, truth = STATUS, estimate = `Logistic regression`)
accuracy(testing_results, truth = STATUS, estimate = `Random forest`)

## Calculate positive predict value
ppv(testing_results, truth = STATUS, estimate = `Logistic regression`)
ppv(testing_results, truth = STATUS, estimate = `Random forest`)