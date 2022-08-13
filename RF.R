# random forest

# import the libraries
library(randomForest) # for making the model
library(caret) # for predict function
# to plot graphs
library('ggplot2')
library('cowplot')

# constant variables and functions
# cross validation
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

plot_graph <- function(model){
  oob.error.data <- data.frame(
    Trees=rep(1:nrow(model$err.rate), times=3),
    Type=rep(c("High", "Low"), each=nrow(model$err.rate)),
    Error=c(model$err.rate[,"High"], 
            model$err.rate[,"Low"]))
  
  ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
    geom_line(aes(color=Type))
}
# 
# fit model on the train dataset
# rf <- randomForest(extraversion_score~AU01+AU02+AU05+
#                    AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
#                    AU20+AU23+AU24+AU25+AU26+AU28+AU43,
#                    data=train,
#                    proximity=TRUE)

#------------------------- extraversion --------------------
dqset.seed(101)
rf_extraversion <- train(extraversion_score~AU01+AU02+AU05+	
                    AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                    AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                    data = train,
                    method = "rf",
                    metric = "Accuracy",
                    proximity = TRUE,
                    importance =TRUE,
                    trControl = trControl)

# print the result 
print(rf_extraversion)

# predict on default model
test$e_rf_default <- predict(rf_extraversion, test)
confusionMatrix(test$e_rf_default, test$extraversion_score)

# error rate of defualt model
plot(rf, main = "Random Forest Default model")

# search best mtry
dqset.seed(101)
e_tuneGrid <- expand.grid(.mtry = c(1: 10))
e_rf_mtry <- train(extraversion_score~AU01+AU02+AU05+	
                     AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                     AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                 data = train,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = e_tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 100,
                 ntree = 500)
print(e_rf_mtry)
# best value of mtry
best_e_mtry <- e_rf_mtry$bestTune$mtry
max(e_rf_mtry$results$Accuracy)


# search best maxnodes
store_maxnode <- list()
e_tuneGrid <- expand.grid(.mtry = best_e_mtry)
for (maxnodes in c(100,150,200,175, 250, 300)) {
  dqset.seed(101)
  e_rf_maxnode <- train(extraversion_score~AU01+AU02+AU05+	
                        AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                        AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                      data = train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = e_tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- e_rf_maxnode
}
eresults_mtry <- resamples(store_maxnode)
summary(eresults_mtry)

# search the best ntrees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  e_rf_maxtrees <- randomForest(extraversion_score~AU01+AU02+AU05+	
                         AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                         AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                       data = train,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = e_tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 36,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- e_rf_maxtrees
}
e_results_tree <- resamples(store_maxtrees)
summary(e_results_tree)

# choose the best parameters from above
e_fit_rf <- randomForest(extraversion_score~AU01+AU02+AU05+	
                    AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                    AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                data = train,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = e_tuneGrid,
                trControl = trControl,
                importance = TRUE,
                ntree = 450,
                maxnodes = 36)

# predict on test data
test$e_pred <-predict(rf_extraversion, test)
confusionMatrix(test$e_pred, test$extraversion_score)

# check variable importance
varImp(rf_extraversion)
#------------------------- openness --------------------
rf_openness <- train(openness_score~AU01+AU02+AU05+	
                    AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                    AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                    data = train,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl)

rf_conscientious <- train(conscientious_score~AU01+AU02+AU05+	
                       AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                       AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                     data = train,
                     method = "rf",
                     metric = "Accuracy",
                     trControl = trControl)

rf_agreeableness <- train(agreeableness_score~AU01+AU02+AU05+	
                       AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                       AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                     data = train,
                     method = "rf",
                     metric = "Accuracy",
                     trControl = trControl)

rf_neuroticism <- train(neurotcism_score~AU01+AU02+AU05+	
                     AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                     AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                     data = train,
                     method = "rf",
                     metric = "Accuracy",
                     trControl = trControl)

# Print the results
print(rf_extra)

# y = train$extraversion_score,
# xtest = test,
# ytest = test$extraversion_score,

# make prediction on the train set
p1 <- predict(rf, train)
confusionMatrix(p1, train$extraversion_score)

# make prediction on the test set
test$e_rfpred <- predict(rf, test)
confusionMatrix(test$e_rfpred, test$extraversion_score)

# error rate of random forest
plot(rf)
plot(rf_default)
