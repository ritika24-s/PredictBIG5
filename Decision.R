library('rpart')
library('rpart.plot')

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



#####------------------------------openness ------------------

# Step1: Begin with a small cp. 
# apply decision tree on the train dataset
o_dtree <- rpart(formula = openness_score~AU01+AU02+AU05+	
                   AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                   AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
                 data = train,
                 method = 'class')
rpart.plot(o_dtree, box.palette = "RdBu", digits = -3)

# Step2: Pick the tree size that minimizes misclassification rate (i.e. prediction error).
# Prediction error rate in training data = Root node error * rel error * 100%
# Prediction error rate in cross-validation = Root node error * xerror * 100%
# Hence we want the cp value (with a simpler tree) that minimizes the xerror. 
printcp(o_dtree)
print(o_dtree)
bestcp <- o_dtree$cptable[which.min(o_dtree$cptable[,"xerror"]),"CP"]

# Step3: Prune the tree using the best cp.
o_dtree.pruned <- prune(o_dtree, cp = bestcp)

# view the tree
rpart.plot(o_dtree.pruned, box.palette = "RdBu", digits = -3)


# train and predict the data on train and test set
o_dtree <- rpart(formula = openness_score~AU01+AU02+AU05+	
                   AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                   AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                 data = train,
                 method = 'class')

rpart.plot(o_dtree, box.palette = "RdBu", digits = -3, extra= 106)
printcp(o_dtree)
# predict the data
test$o_dpred <- predict(o_dtree,test, type="class")
# confusion matrix (training data)
conf.matrix <- table(test$openness_score, test$o_dpred)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)
accuracy_o_dpred <- sum(diag(conf.matrix)) / sum(conf.matrix)

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

tune_fit <- rpart(openness_score~AU01+AU02+AU05+	
                    AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                    AU20+AU23+AU24+AU25+AU26+AU28+AU43, 
                  data = train, 
                  method = 'class', 
                  control = control)
printcp(tune_fit)
rpart.plot(tune_fit, box.palette = "RdBu", digits = -3, main = "Openness - Decision Tree" )

accuracy_tune(tune_fit, test, "openness_score" )

#####------------------------------ neuroticism ------------------

# Step1: Begin with a small cp. 
# apply decision tree on the train dataset
n_dtree <- rpart(formula = neurotcism_score~AU01+AU02+AU05+	
                   AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                   AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
                 data = train,
                 method = 'class')
rpart.plot(n_dtree, box.palette = "RdBu", digits = -3)

# Step2: Pick the tree size that minimizes misclassification rate (i.e. prediction error).
# Prediction error rate in training data = Root node error * rel error * 100%
# Prediction error rate in cross-validation = Root node error * xerror * 100%
# Hence we want the cp value (with a simpler tree) that minimizes the xerror. 
printcp(n_dtree)
print(n_dtree)
bestcp <- n_dtree$cptable[which.min(n_dtree$cptable[,"xerror"]),"CP"]

# Step3: Prune the tree using the best cp.
n_dtree.pruned <- prune(n_dtree, cp = bestcp)

# view the tree
rpart.plot(n_dtree.pruned, box.palette = "RdBu", digits = -3)


# train and predict the data on train and test set
n_dtree <- rpart(formula = neurotcism_score~AU01+AU02+AU05+	
                   AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                   AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                 data = train,
                 method = 'class')

rpart.plot(n_dtree, box.palette = "RdBu", digits = -3, extra= 106)
printcp(n_dtree)
# predict the data
test$n_dpred <- predict(n_dtree,test, type="class")
# confusion matrix (training data)
conf.matrix <- table(test$neurotcism_score, test$n_dpred)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)
accuracy_n_dpred <- sum(diag(conf.matrix)) / sum(conf.matrix)

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

tune_fit <- rpart(neurotcism_score~AU01+AU02+AU05+	
                    AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                    AU20+AU23+AU24+AU25+AU26+AU28+AU43, 
                  data = train, 
                  method = 'class', 
                  control = control)
printcp(tune_fit)
rpart.plot(tune_fit, box.palette = "RdBu", digits = -3, main = "Neuroticism - Decision Tree" )

accuracy_tune(tune_fit, test, "neurotcism_score" )

#####------------------------------ agreeableness ------------------

# Step1: Begin with a small cp. 
# apply decision tree on the train dataset
a_dtree <- rpart(formula = agreeableness_score~AU01+AU02+AU05+	
                   AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                   AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
                 data = train,
                 method = 'class')
rpart.plot(a_dtree, box.palette = "RdBu", digits = -3)

# Step2: Pick the tree size that minimizes misclassification rate (i.e. prediction error).
# Prediction error rate in training data = Root node error * rel error * 100%
# Prediction error rate in cross-validation = Root node error * xerror * 100%
# Hence we want the cp value (with a simpler tree) that minimizes the xerror. 
printcp(a_dtree)
print(a_dtree)
bestcp <- a_dtree$cptable[which.min(a_dtree$cptable[,"xerror"]),"CP"]

# Step3: Prune the tree using the best cp.
a_dtree.pruned <- prune(a_dtree, cp = bestcp)

# view the tree
rpart.plot(a_dtree.pruned, box.palette = "RdBu", digits = -3)


# train and predict the data on train and test set
a_dtree <- rpart(formula = agreeableness_score~AU01+AU02+AU05+	
                   AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                   AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                 data = train,
                 method = 'class')

rpart.plot(a_dtree, box.palette = "RdBu", digits = -3, extra= 106)
printcp(a_dtree)
# predict the data
test$a_dpred <- predict(a_dtree,test, type="class")
# confusion matrix (training data)
conf.matrix <- table(test$agreeableness_score, test$a_dpred)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)
accuracy_n_dpred <- sum(diag(conf.matrix)) / sum(conf.matrix)

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

tune_fit <- rpart(agreeableness_score~AU01+AU02+AU05+	
                    AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                    AU20+AU23+AU24+AU25+AU26+AU28+AU43, 
                  data = train, 
                  method = 'class', 
                  control = control)
printcp(tune_fit)
rpart.plot(tune_fit, box.palette = "RdBu", digits = -3, main = "Agreeableness - Decision Tree" )

accuracy_tune(tune_fit, test, "agreeableness_score" )

#####------------------------------ conscientiousness ------------------
# Step1: Begin with a small cp. 
# apply decision tree on the train dataset
c_dtree <- rpart(formula = conscientious_score~AU01+AU02+AU05+	
                   AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                   AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
                 data = train,
                 method = 'class')
rpart.plot(c_dtree, box.palette = "RdBu", digits = -3)

# Step2: Pick the tree size that minimizes misclassification rate (i.e. prediction error).
# Prediction error rate in training data = Root node error * rel error * 100%
# Prediction error rate in cross-validation = Root node error * xerror * 100%
# Hence we want the cp value (with a simpler tree) that minimizes the xerror. 
printcp(c_dtree)
print(c_dtree)
bestcp <- c_dtree$cptable[which.min(c_dtree$cptable[,"xerror"]),"CP"]

# Step3: Prune the tree using the best cp.
c_dtree.pruned <- prune(c_dtree, cp = bestcp)

# view the tree
rpart.plot(c_dtree.pruned, box.palette = "RdBu", digits = -3)


# train and predict the data on train and test set
c_dtree <- rpart(formula = conscientious_score~AU01+AU02+AU05+	
                   AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                   AU20+AU23+AU24+AU25+AU26+AU28+AU43,
                 data = train,
                 method = 'class')

rpart.plot(c_dtree, box.palette = "RdBu", digits = -3, extra= 106)
printcp(c_dtree)
# predict the data
test$c_dpred <- predict(c_dtree,test, type="class")
# confusion matrix (training data)
conf.matrix <- table(test$conscientious_score, test$c_dpred)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)
accuracy_n_dpred <- sum(diag(conf.matrix)) / sum(conf.matrix)

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

tune_fit <- rpart(conscientious_score~AU01+AU02+AU05+	
                    AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                    AU20+AU23+AU24+AU25+AU26+AU28+AU43, 
                  data = train, 
                  method = 'class', 
                  control = control)
printcp(tune_fit)
rpart.plot(tune_fit, box.palette = "RdBu", digits = -3, main = "Conscientious - Decision Tree" )

accuracy_tune(tune_fit, test, "conscientious_score" )
