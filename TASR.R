# Build a simple logistic regression model
simple_glm <- stackoverflow %>%
  select(-Respondent) %>%
  glm(Remote ~ .,
      family = "binomial",
      data = .)

# Print the summary of the model
summary(simple_glm)

# Load caret
library(caret)

stack_select <- stackoverflow %>%
  select(-Respondent)

?createDataPartition
# Split the data into training and testing sets
set.seed(1234)
in_train <- createDataPartition(stack_select$Remote, p = 0.8, list = FALSE)
training <- stack_select[in_train,]
testing <- stack_select[-in_train,]

# Upsampling
# There are multiple possible approaches to dealing with class imbalance. 
# Here, you will implement upsampling using caret's upSample() function.

# Use the training data set for upsampling, both for x (the predictors) and 
# y (the class memberships).
# The label for the class column goes in yname; remember that it is "Remote".

up_train <- upSample(x = select(training, -Remote),
                     y = training$Remote,
                     yname = "Remote") %>%
  as_tibble()

up_train %>%
  count(Remote)

# Build a logistic regression model
stack_glm <- train(Remote ~ ., method = "glm", family = "binomial",
                   data = training,
                   trControl = trainControl(method = "boot",
                                            sampling = "up"))

# Build a random forest model
stack_rf <- train(Remote ~ ., method = "rf", family = "binomial",
                   data = training,
                   trControl = trainControl(method = "boot",
                                            sampling = "up"))
# Print the model object 
stack_glm
stack_rf

# Classification model metrics
# The confusionMatrix() function is helpful but often you want to store specific performance estimates for later, 
# perhaps in a dataframe-friendly form. The yardstick package is built to handle such needs. 
# For this kind of classifier model, 
# you might look at the positive or negative predictive value or perhaps overall accuracy.

# Confusion matrix for logistic regression model
confusionMatrix(predict(stack_glm, testing),
                testing$Remote)



# Load yardstick
library(yardstick)

# Predict values
testing_results <- testing %>%
  mutate(`Logistic regression` = predict(stack_glm, testing),
         `Random forest` = predict(stack_rf, testing))

## Calculate accuracy
accuracy(testing_results, truth = Remote, estimate = `Logistic regression`)
accuracy(testing_results, truth = Remote, estimate = `Random forest`)

## Calculate positive predict value
ppv(testing_results, truth = Remote, estimate = `Logistic regression`)
ppv(testing_results, truth = Remote, estimate = `Random forest`)