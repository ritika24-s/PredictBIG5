install.packages('plyr')
library(plyr)

install.packages('readr')
library(readr)

install.packages('ggplot2')
library(ggplot2)

install.packages('GGally')
library(GGally)

install.packages('dplyr')
library(dplyr)

install.packages('mlbench')
library(mlbench)

install.packages("psych")
library(psych)

# to find correlation
install.packages("ggpubr")
library("ggpubr")
library(Hmisc)
install.packages("corrplot")
library(corrplot)
install.packages('ggcorrplot')
library(ggcorrplot)

# get all the csv files inside the folders Processed_RFRF and merge them
library(dplyr)
library(readr)
df <- list.files(path = getwd(), pattern = ".csv", recursive = TRUE) %>%
  lapply(read.csv) %>%
  bind_rows


# remove all the extra features and keep only aus and emotions
df <- df[,c(143:170)]
df_au <- df[, -c(1, 29)]


# to test normality distribution of all the variables
lshap6 <- lapply(df_au, hist)
for(i in names(df_au))
  hist(df_au[[i]], main = i, xlab = "Value")

person6 <- P006_S02_front_video_Z_S_L[, c('AU01',	'AU02', 'AU05',	
            'AU06',	'AU07',	'AU09',	
            'AU10',	'AU11',	'AU12',	
            'AU14',	'AU15',	'AU17',
            'AU20',	'AU23', 'AU24',	
            'AU25',	'AU26',	'AU28',
            'AU43', 'anger', 'disgust',
            'fear',	'happiness',	
            'sadness',	'surprise',	'neutral'
)]
person7_au <- P007_S02_front_video_Z_S_L[, c('AU01',	'AU02', 'AU05',	
            'AU06',	'AU07',	'AU09',	
            'AU10',	'AU11',	'AU12',	
            'AU14',	'AU15',	'AU17',
            'AU20',	'AU23', 'AU24',	
            'AU25',	'AU26',	'AU28',
            'AU43', 'anger', 'disgust',
            'fear',	'happiness',	
            'sadness',	'surprise',	'neutral'
)]

person7_au_30 <- P007[, c('AU01',	'AU02', 'AU05',	
                  'AU06',	'AU07',	'AU09',	
                  'AU10',	'AU11',	'AU12',	
                  'AU14',	'AU15',	'AU17',
                  'AU20',	'AU23', 'AU24',	
                  'AU25',	'AU26',	'AU28',
                  'AU43', 'anger', 'disgust',
                  'fear',	'happiness', 'sadness',
                  'surprise',	'neutral'
)]

person6_hog <- P006_svm[, c('AU01',	'AU02', 'AU05',	
                                'AU06',	'AU07',	'AU09',	
                                'AU10',	'AU11',	'AU12',	
                                'AU14',	'AU15',	'AU17',
                                'AU20',	'AU23', 'AU24',	
                                'AU25',	'AU26',	'AU28',
                                'AU43', 'anger', 'disgust',
                                'fear',	'happiness',	
                                'sadness',	'surprise',	'neutral'
)]

for(i in names(person7_au))
  hist(person7_au[[i]], main = paste(i, "_1frame"), xlab = "Value")
  hist(person7_au_30[[i]], main = paste(i, "_30frame"), xlab = "Value")

hist(person6_au$AU01, main = "AU01", xlab="value")
hist(person6_au$sadness)
hist(person6_au$fear)
hist(person6_au$surprise)
hist(person6_au$AU15, main = "AU01", xlab="value")
hist(person6_hog$AU01)
hist(person7_au_30$AU23)
ggscatter(person6_au, x = "AU01", y = "surprise", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "AU01", ylab = "surprise")

# to test normality distribution of all the variables
lshap6 <- lapply(BFI_44_assessment[,c(5,7,9,11,13)], shapiro.test)
lshap7 <- lapply(person7_au_30, shapiro.test)

shapiro.test(person7_au$AU14)
shapiro.test(person6_au$fear)
shapiro.test(person6_au$sadness)
shapiro.test(person6_au$surprise)

shape(person7_au$AU14)

# find correlation for non-normalized variables
corr_p6 <- round(cor(person6_au[,1:19], person6_au[,20:26], 
                     method = "kendall"),2)
corr_p7 <- round(cor(person7_au_30[,-c(14, 20:26)], person7_au_30[,20:26],
                     method = "kendall"),2)

corrplot(corr_p6, is.corr=FALSE,
         title="Correlogram of Action units and emotions person 6")

corrplot(corr_p7, is.corr=FALSE,
         title="Correlogram of Action units and emotions person 7 except AU23")

tetrachoric(person6_hog$AU01,person6_hog$sadness)

corrplot(corr_person7_au30, 
         type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


# create correlation matrix
# to identify multicollinearity among numerical variables
corr_emotions7 <- round(cor(person7_au_30[,20:26], method="kendall"),2)
corr_emotions6 <- round(cor(person6_au[,20:26], method="kendall"),2)

corrplot(corr_person7_au30, method="number")


# plot correlation plots
ggcorrplot(corr_emotions6, hc.order = TRUE, type = "lower",
           lab = TRUE, lab_size = 3, method="circle", 
           colors = c("blue", "white", "red"), outline.color = "white",
           show.legend = TRUE, show.diag = FALSE, 
           title="Correlogram of emotions person 6")

ggcorrplot(corr_emotions7, hc.order = TRUE, type = "lower", 
           lab = TRUE, lab_size = 3, method="circle", 
           colors = c("blue", "white", "red"), outline.color = "white",
           show.legend = TRUE, show.diag = FALSE, 
           title="Correlogram of emotions person 7")

corrplot(corr_person6_au30_all, method = 'number', order = 'alphabet')

# relationship between cagtegorical variables
# create a two-way table between the variables
person6_hog_table <- table(person6_hog$AU12, person6_hog$happiness)


# convert personality scores into binary values based on percentile

# exclude annotator data
BFI_44_assessment <- BFI_44_assessment[-c(1,2,3,4,5),]
summary(BFI_44_assessment)
BFI_44_assessment <- na.omit(BFI_44_assessment)
BFI_44_assessment [ , c(3:13)] <- apply(BFI_44_assessment[ , c(3:13),drop=F], 2,           
                            function(x) as.numeric(as.character(x)))

BFI_44_assessment$extraversion_score <- ifelse(
                            strtoi(BFI_44_assessment$...5)>=50, 1, 0)
BFI_44_assessment$agreeableness_score <- ifelse(
                            strtoi(BFI_44_assessment$...7)>=50, 1, 0)
BFI_44_assessment$conscientious_score <- ifelse(
                            strtoi(BFI_44_assessment$...9)>=50, 1, 0)
BFI_44_assessment$neurotcism_score <- ifelse(
                            strtoi(BFI_44_assessment$...11)>=50, 1, 0)
BFI_44_assessment$openness_score <- ifelse(
                            strtoi(BFI_44_assessment$...13)>=50, 1, 0)

BFI_44_scores <- BFI_44_assessment[, c(2,14:18)]

# correlation between personality scores and aus
corr_bfi_6 <- round(cor(person6_au[,20:26], BFI_44_scores, method = "kendall"), 2)
