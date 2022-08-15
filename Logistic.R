# Logistic Regression

# import the dataset
library(dplyr)

# Step 1: Check continuous variables
# Step 2: Check factor variables
# Step 3: Feature engineering
# Step 4: Summary statistic
# Step 5: Train/test set
# Step 6: Build the model
# Step 7: Assess the performance of the model
# step 8: Improve the model


# Step 1: Check continuous variables
continuous <-select_if(data_rf, is.numeric)
summary(continuous)
#------------------------------extraversion -------------------------
