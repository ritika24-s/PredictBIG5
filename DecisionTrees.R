# decision trees

# import library tree
install.packages('tree')
install.packages('caTools')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('DAAG')
install.packages('party')
require('tree')
require('caTools')
library('rpart')
library('rpart.plot')
library('DAAG')
library('party')
library('mlbench')
library('caret')
library('pROC')




# view histograms grouped by persons
# library(ggplot2)
# ggplot(final_dataset, aes(x = person, fill = extraversion_score)) + 
#   geom_histogram(alpha = 0.5, position = "identity")

# implement decision tree

# decision_tree <- tree(formula = extraversion_score~AU01+AU02+AU05+	
#                       AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
#                       AU20+AU23+AU24+AU25+AU26+AU28+AU43+anger+
#                       disgust+fear+happiness+sadness+surprise+neutral,
#                       data =final_dataset)
# # check the summary of the tree
# summary(decision_tree)
# plot(decision_tree)
# text(decision_tree, pretty = 0)
# decision_tree
# 
# # prune the tree
# set.seed(101)
# size = 0.8*nrow(final_dataset)
# train = sample(1:nrow(final_dataset), size)
# decision_tree <- tree(formula = extraversion_score~AU01+AU02+AU05+	
#                         AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
#                         AU20+AU23+AU24+AU25+AU26+AU28+AU43+anger+
#                         disgust+fear+happiness+sadness+surprise+neutral,
#                       data =final_dataset,
#                       subset = train)
# # check the summary of the tree
# summary(decision_tree)
# plot(decision_tree)
# text(decision_tree, pretty = 0)
# 
# # predict on test set
# decision_tree$pred <- predict(decision_tree, 
#                               final_dataset[-train],
#                               type = "class")




# apply decision tree on the train dataset
e_dtree <- rpart(formula = extraversion_score~AU01+AU02+AU05+	
                           AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                           AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                       data = train,
                      method = "class",
                      control = rpart.control(maxdepth = 4))
summary(e_dtree)
# plot the decision tree
rpart.plot(e_dtree, box.palette = "RdBu", digits = -3)

# find optimal values of cp
printcp(e_dtree)
plotcp(e_dtree)

# train data again on lower cp value
# apply decision tree on the train dataset
e_dtree <- rpart(formula = extraversion_score~AU01+AU02+AU05+	
                   AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                   AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                 data = train, cp = 0.01)

# predict values at any point
test <- data_rf[-sample,]
test$e_dpred <- predict(e_dtree, test, type = "class")

View(test)

table(test$extraversion_score, test$e_dpred)

# confusion matrix
confusionMatrix(test$e_dpred, test$extraversion_score)

# roc
p1 <- predict(e_dtree, test, type = 'prob')
p1 <- p1[,2]
r <- multiclass.roc(test$extraversion_score, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve for extraversion score')
