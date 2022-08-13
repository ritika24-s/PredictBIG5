# Modeling packages
library(caret)     # for logistic regression modeling

# Model interpretability packages
install.packages('vip')
library(vip)       # variable importance

# set seed
dqset.seed(101)
# train the model 
e_glm <- glm(extraversion_score~AU01+AU02+AU05+	
             AU06+AU07+AU09+AU10+AU11+AU12+AU14+AU15+AU17+
             AU20+AU23+AU24+AU25+AU26+AU28+AU43+gender,
             data = train,
             family = "binomial")
summary(e_glm)

# take only significant features
e_glm1 <- glm(extraversion_score~AU02+	
              AU10+AU11+AU12+AU14+
               AU26+AU28+gender,
             data = train,
             family = "binomial")
summary(e_glm1)

# further removing AU 9 7 and 20
# final features - AU02+AU10+AU11+AU12+AU14+ AU26+AU28+gender

# make predictions now
test$e_glm <- predict(e_glm1, test, type="response")
