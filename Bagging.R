# bagging
install.packages('ipred')
library(ipred)

# train data
set.seed(0)

final_dataset$extraversion_score <- ifelse(
  (final_dataset$extraversion_score)==1, 'High', 'Low')

final_dataset$extraversion_score <- as.factor(final_dataset$extraversion_score)

bagging <- bagging(extraversion_score~., 
                   data = final_dataset[train,],
                   coob = TRUE, nbagg = 100)
print(bagging)
test$bagging <- predict(bagging, test)
table(test$extraversion_score, test$bagging)
