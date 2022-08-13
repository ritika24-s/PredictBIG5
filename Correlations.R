# correlations between different units

# to find correlation
install.packages("ggpubr")
library("ggpubr")
library(Hmisc)
install.packages("corrplot")
library(corrplot)
install.packages('ggcorrplot')
library(ggcorrplot)
library(nortest)


# remove all the extra features and keep only aus and emotions
df_rf_au <- data_rf[, c(4:30)]
df_rf_traits <- data_rf[,c(33:37)]

df_nrf_au <- data_norm_rf[, c(4:30)]
df_nrf_traits <- data_norm_rf[,c(33:37)]

df_svm_au <- data_svm[, c(4:30)]
df_svm_traits <- data_svm[,c(33:37)]

# to test normality distribution of all the variables
dnrf <- lapply(df_nrf_au, ad.test)
drf <- lapply(df_rf_au, ad.test)
dsvm <- lapply(df_svm_au, ad.test)

# plot histogram to visualize the data
for(i in names(person7_au))
  hist(person7_au[[i]], main = paste(i, "_1frame"), xlab = "Value")
  hist(person7_au_30[[i]], main = paste(i, "_30frame"), xlab = "Value")

hist(data_norm_rf$AU01, main = "AU01 - Normalized data", xlab="value")
hist(data_rf$AU01, main="AU01", xlab="value")
hist(data_norm_rf$sadness, main = "Sadness - Normalized data", xlab="value")
hist(data_norm_rf$happiness, main = "Happiness", xlab="value")
hist(data_norm_rf$AU12, main = "AU12", xlab="value")

ggscatter(data_rf, x = "AU01", y = "sadness", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "AU01", ylab = "sadness")

# find correlation for non-normality distributed variables
corr_rf <- round(cor(df_rf_au[,c(1:20)], df_rf_au[,c(21:27)], 
                     method = "kendall"),2)

corr_nrf <- round(cor(df_nrf_au[,c(1:20)], df_nrf_au[,c(21:27)], 
                     method = "spearman"),2)

corrplot(corr_rf, is.corr=FALSE,
         title="Correlogram of Action units and emotions")

corrplot(corr_nrf, is.corr=FALSE,
         title="Correlogram of Action units and emotions for 
         Baseline modified data")
#-----------------------------------------------------------------

# correlation between AUs , emotions and personality traits in SVM
# the data is all categorical, thus correlation method of 
# categorical to categorical will be applied

# install required packages
required_packages <- c('rcompanion', 'lsr', 'vcd', 'DescTools')
for (p in required_packages) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p, dep = TRUE)
  }
}

# contigency table
prop.table(table(data_svm$AU02, data_svm$agreeableness_score))
           


# --------------------------------------------------------
# correlation between AUs and personality traits in RF
# the data is continuous to categorical, thus Wilcoxon test is applied

install.packages("dplyr")
install.packages("ggpubr")
library("ggpubr")

# Plot personality traits by group and color by group
ggboxplot(data_rf, x = "AU01", y = "extraversion_score", 
          color = "AU01", palette = c("#00AFBB", "#E7B800"),
          ylab = "Traits", xlab = "AUs")
