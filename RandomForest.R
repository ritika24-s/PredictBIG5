# random forest

# import the libraries
library(randomForest) # for making the model
library(caret) # for predict function
# to plot graphs
library('ggplot2')
library('cowplot')
#------------------------- extraversion --------------------
dqset.seed(101)
rf_extraversion1 <- randomForest(extraversion_score~AU01+AU02+AU05+	
                                   AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                                   AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
                                 data = train,
                                 proximity = TRUE,
                                 importance =TRUE)

# print the result 
print(rf_extraversion1)

# predict on default model
test$e_rf_default <- predict(rf_extraversion1, test)
confusionMatrix(test$e_rf_default, test$extraversion_score)

# error rate of defualt model
plot(rf_extraversion1, main = "Random Forest Extroversion Default model")


# using tuneRf function find best mtry
t <- tuneRF(train[,c(4:23, 32)], train[,33],
       stepFactor = 0.4,
       plot=TRUE,
       ntreeTry = 500,
       trace = TRUE,
       improve = 0.05)

# got mtry = 5 as most optimal and ntress = 400
# train model with these parameters
rf_extraversion <- randomForest(extraversion_score~AU01+AU02+AU05+	
                                 AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                                 AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
                                 data = train,
                                 ntree = 500,
                                 mtry = 5,
                                 proximity = TRUE,
                                 importance =TRUE)
print(rf_extraversion)

# predict on tuned model
test$e_rf <- predict(rf_extraversion, test)
confusionMatrix(test$e_rf, test$extraversion_score)

# Number of nodes for the trees
hist(treesize(rf_extraversion),
     main = "Number of nodes for trees - extraversion RF",
     col = "blue")

# to check importance of the variable
varImpPlot(rf_extraversion,
           sort = T,
           main = "Variable improtance for Extroversion - RF")
varImpPlot(rf_extraversion,
           n.var = 8,
           sort = T,
           main = "Top 5 variable improtance for Extroversion - RF")

importance(rf_extraversion)
varUsed(rf_extraversion)

# partial dependence plot
partialPlot(rf_extraversion, test, AU02,  "High",
            main = "AU02 dependency on Extroversion - RF" )
partialPlot(rf_extraversion, test, AU17,  "High",
            main = "AU17 dependency on Extroversion - RF" )
partialPlot(rf_extraversion, test, AU01,  "Low",
            main = "AU01 dependency on Low class Extroversion - RF" )

#---------------------------------------------------------
#------------------------- openness --------------------

dqset.seed(101)
rf_openness1 <- randomForest(openness_score~AU01+AU02+AU05+	
                         AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                         AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
                         data = train,
                         proximity = TRUE,
                         importance =TRUE)

# print the result 
print(rf_openness1)

# predict on default model
test$o_rf_default <- predict(rf_openness1, test)
confusionMatrix(test$o_rf_default, test$openness_score)

# error rate of defualt model
plot(rf_openness1, main = "Random Forest Openness Default model")

# trees = 500
# using tuneRf function find best mtry
t <- tuneRF(train[,c(4:23, 32)], train[,33],
            stepFactor = 0.2,
            plot=TRUE,
            ntreeTry = 500,
            trace = TRUE,
            improve = 0.05)

# got mtry = 8 as most optimal and ntress = 500
# train model with these parameters
rf_openness <- randomForest(openness_score~AU01+AU02+AU05+	
                                  AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                                  AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
                                data = train,
                                ntree = 500,
                                mtry = 5,
                                proximity = TRUE,
                                importance =TRUE)
print(rf_openness)

# predict on tuned model
test$o_rf <- predict(rf_openness, test)
confusionMatrix(test$o_rf, test$openness_score)

# Number of nodes for the trees
hist(treesize(rf_openness1),
     main = "Number of nodes for trees - Openness RF",
     col = "blue")

# to check importance of the variable
varImpPlot(rf_openness,
           sort = T,
           main = "Variable improtance for Openness - RF")
varImpPlot(rf_openness,
           n.var = 5,
           sort = T,
           main = "Top 5 variable improtance for Openness - RF")

importance(rf_openness)

varUsed(rf_openness)

# partial dependence plot
partialPlot(rf_openness, train, AU01,  "High",
            main = "AU01 dependency on Openness - RF" )
partialPlot(rf_openness, train, AU28,  "High",
            main = "AU28 dependency on Openness - RF" )
partialPlot(rf_openness, train, gender,  "High",
            main = "Gender dependency on Openness - RF" )

#---------------------------------------------------------
#------------------------- conscientious --------------------
dqset.seed(101)
rf_conscientiousness1 <- randomForest(conscientious_score~AU01+AU02+AU05+	
                               AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                               AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
                             data = train,
                             proximity = TRUE,
                             importance =TRUE)

# print the result 
print(rf_conscientiousness1)

# predict on default model
test$c_rf_default <- predict(rf_conscientiousness1, test)
confusionMatrix(test$c_rf_default, test$conscientious_score)

# error rate of defualt model
plot(rf_conscientiousness1, main = "Random Forest Conscientiousness Default model")


# using tuneRf function find best mtry
t <- tuneRF(train[,c(4:23,32)], train[,33],
            plot=TRUE,
            ntreeTry = 500,
            trace = TRUE,
            stepFactor=0.8,
            improve=0.05)

# got mtry = 5 as most optimal and ntress = 500
# train model with these parameters
rf_conscientiousness <- randomForest(conscientious_score~AU01+AU02+AU05+	
                              AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                              AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
                            data = train,
                            ntree = 500,
                            mtry = 5,
                            proximity = TRUE,
                            importance =TRUE)
print(rf_conscientiousness)

# predict on tuned model
test$c_rf <- predict(rf_conscientiousness, test)
confusionMatrix(test$c_rf, test$conscientious_score)

# defualt model mty = 4 and trees = 500 gives the best results
# Number of nodes for the trees
hist(treesize(rf_conscientiousness1),
     main = "Number of nodes for trees - conscientiousness RF",
     col = "blue")

# to check importance of the variable
varImpPlot(rf_conscientiousness1,
           sort = T,
           main = "Variable improtance for conscientiousness - RF")
varImpPlot(rf_conscientiousness1,
           n.var = 4,
           sort = T,
           main = "Top 4 variable improtance for conscientiousness - RF")

importance(rf_conscientiousness1)
varUsed(rf_conscientiousness1)

# partial dependence plot
partialPlot(rf_conscientiousness, train, AU02,  "High",
            main = "AU02 dependency on conscientiousness - RF" )
partialPlot(rf_conscientiousness1, train, AU01,  "High",
            main = "AU01 dependency on conscientiousness - RF" )
partialPlot(rf_conscientiousness, train, AU28,  "High",
            main = "AU02 dependency on conscientiousness - RF" )
#---------------------------------------------------------
#------------------------- agreeableness --------------------
dqset.seed(101)
rf_agreeableness1 <- randomForest(agreeableness_score~AU01+AU02+AU05+	
                                  AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                                  AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
                                      data = train,
                                      proximity = TRUE,
                                      importance =TRUE)

# print the result 
print(rf_agreeableness1)

# predict on default model
test$a_rf_default <- predict(rf_agreeableness1, test)
confusionMatrix(test$a_rf_default, test$agreeableness_score)

# error rate of defualt model
plot(rf_agreeableness1, main = "Random Forest Agreeableness Default model")


# using tuneRf function find best mtry
t <- tuneRF(train[,c(4:23, 32)], train[,33],
            stepFactor = 0.4,
            plot=TRUE,
            ntreeTry = 500,
            trace = TRUE,
            improve = 0.05)

# got mtry = 5 as most optimal and ntress = 500
# train model with these parameters
rf_agreeableness <- randomForest(agreeableness_score~AU01+AU02+AU05+	
                               AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                               AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
                               data = train,
                               ntree = 500,
                               mtry = 5,
                               proximity = TRUE,
                               importance =TRUE)
print(rf_agreeableness)

# predict on tuned model
test$a_rf <- predict(rf_agreeableness, test)
confusionMatrix(test$a_rf, test$agreeableness_score)

# Number of nodes for the trees
hist(treesize(rf_agreeableness),
     main = "Number of nodes for trees - Agreeableness RF",
     col = "blue")

# to check importance of the variable
varImpPlot(rf_agreeableness,
           sort = T,
           main = "Variable improtance for Agreeableness - RF")
varImpPlot(rf_agreeableness,
           n.var = 5,
           sort = T,
           main = "Top 5 variable improtance for Agreeableness - RF")

importance(rf_agreeableness)
varUsed(rf_agreeableness)

# partial dependence plot
partialPlot(rf_agreeableness, train, AU28,  "High",
            main = "AU28 dependency on Agreeableness - RF" )
partialPlot(rf_agreeableness, train, AU01,  "High",
            main = "AU01 dependency on Agreeableness - RF" )
partialPlot(rf_agreeableness, train, AU28,  "High",
            main = "AU02 dependency on Agreeableness - RF" )
#---------------------------------------------------------
#------------------------- Neuroticism --------------------
dqset.seed(101)
rf_neuroticism_ <- randomForest(neurotcism_score~AU01+AU02+AU05+	
                            AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                            AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
                            data = train,
                            ntree = 800,
                            proximity = TRUE,
                            importance =TRUE)

# print the result 
print(rf_neuroticism_)

# predict on default model
test$n_rf_default <- predict(rf_neuroticism1, test)
confusionMatrix(test$n_rf_default, test$neurotcism_score)

# error rate of default model
plot(rf_neuroticism1, main = "Random Forest Neuroticism Default model")


# using tuneRf function find best mtry
t <- tuneRF(train[,c(4:23, 32)], train[,33],
            stepFactor = 0.8,
            plot=TRUE,
            ntreeTry = 800,
            trace = TRUE,
            improve = 0.05)

# got mtry = 5 as most optimal and ntress = 500
# train model with these parameters
rf_neuroticism3 <- randomForest(neurotcism_score~AU01+AU02+AU05+	
                                   AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
                                   AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
                                 data = train,
                                 ntree = 1000,
                                 mtry = 5,
                                 proximity = TRUE,
                                 importance =TRUE)
print(rf_neuroticism3)

# predict on tuned model
test$n_rf <- predict(rf_neuroticism_, test)
confusionMatrix(test$n_rf, test$neurotcism_score)

# Number of nodes for the trees
hist(treesize(rf_neuroticism1),
     main = "Number of nodes for trees - Neuroticism RF",
     col = "blue")

# to check importance of the variable
varImpPlot(rf_neuroticism1,
           sort = T,
           main = "Variable improtance for Neuroticism - RF")
varImpPlot(rf_neuroticism1,
           n.var = 7,
           sort = T,
           main = "Top 7 variable improtance for Neuroticism - RF")

importance(rf_neuroticism1)
varUsed(rf_neuroticism)

# partial dependence plot
partialPlot(rf_neuroticism1, train, AU28,  "High",
            main = "AU28 dependency on Neuroticism - RF" )
partialPlot(rf_neuroticism1, train, AU14,  "High",
            main = "AU14 dependency on Neuroticism - RF" )
partialPlot(rf_neuroticism1, train, gender,  "High",
            main = "Gender dependency on Neuroticism - RF" )
