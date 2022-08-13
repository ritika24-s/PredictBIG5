# Step1: Begin with a small cp. 
# apply decision tree on the train dataset
e_dtree <- rpart(formula = extraversion_score~AU01+AU02+AU05+	
                   AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                   AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
                 data = train,
                 method = 'class')
rpart.plot(e_dtree, box.palette = "RdBu", digits = -3)

# Step2: Pick the tree size that minimizes misclassification rate (i.e. prediction error).
# Prediction error rate in training data = Root node error * rel error * 100%
# Prediction error rate in cross-validation = Root node error * xerror * 100%
# Hence we want the cp value (with a simpler tree) that minimizes the xerror. 
printcp(e_dtree)
print(e_dtree)
bestcp <- e_dtree$cptable[which.min(e_dtree$cptable[,"xerror"]),"CP"]

# Step3: Prune the tree using the best cp.
e_dtree.pruned <- prune(e_dtree, cp = bestcp)

# view the tree
rpart.plot(e_dtree.pruned, box.palette = "RdBu", digits = -3)


# train and predict the data on train and test set
e_dtree <- rpart(formula = extraversion_score~AU01+AU02+AU05+	
                   AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                   AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                 data = train,
                 method = 'class')

rpart.plot(e_dtree, box.palette = "RdBu", digits = -3, extra= 106)
printcp(e_dtree)
# predict the data
test$e_dpred <- predict(e_dtree,test, type="class")
# confusion matrix (training data)
conf.matrix <- table(test$extraversion_score, test$e_dpred)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)
accuracy_e_dpred <- sum(diag(conf.matrix)) / sum(conf.matrix)

# --------------------------------------------------------------------------
# function to find accuracy score
accuracy_tune <- function(fit, dataset, score) {
  prediction <- predict(fit, dataset, type = 'class')
  conf.matrix <- table(dataset[[score]], prediction)
  accuracy_score <- sum(diag(conf.matrix )) / sum(conf.matrix )
  accuracy_score
}

# hyper parameter tuning
control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 5,
                         cp = 0.01)

tune_fit <- rpart(extraversion_score~AU01+AU02+AU05+	
                  AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                  AU20+AU23+AU24+AU25+AU26+AU28+AU43, 
                  data = train, 
                  method = 'class', 
                  control = control)
printcp(tune_fit)
rpart.plot(tune_fit, box.palette = "RdBu", digits = -3, main = "Extraversion - Decision Tree" )

accuracy_tune(tune_fit, test, "extraversion_score" )
