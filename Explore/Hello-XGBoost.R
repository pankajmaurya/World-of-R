library(xgboost)

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
# fit model
bst <- xgboost(x = train$data, y = factor(train$label),
               max.depth = 2, eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic")
# predict
pred <- predict(bst, test$data)

hist(
  pred,
  breaks = 50,
  col = "steelblue",
  main = "Histogram of Predicted Probabilities",
  xlab = "Predicted probability (P = poisonous)"
)

plot(
  pred,
  test$label,
  pch = 16,
  col = rgb(0, 0, 1, 0.3),
  xlab = "Predicted Probability",
  ylab = "Actual Label (0 = edible, 1 = poisonous)",
  main = "Predictions vs Actual Labels"
)

pred_label <- ifelse(pred > 0.5, 1, 0)

cm <- table(
  Predicted = pred_label,
  Actual = test$label
)

barplot(
  cm,
  beside = TRUE,
  col = c("skyblue", "salmon"),
  legend = TRUE,
  main = "Confusion Matrix"
)
