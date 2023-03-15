#Library
install.packages('Hmisc') #fungsi describe()
library(Hmisc)
install.packages('Amelia') #visualisasi Missing Data
library(Amelia) 
install.packages('mice') #imputation
library(mice)
install.packages('ggplot2') #plotting
library(ggplot2)
install.packages('caret') #mbagi data jadi Training dan Test Set
library(caret)
install.packages('e1071') #fungsi Naive Bayes Classification
library(e1071)
install.packages('dplyr')
library(dplyr) #fungsi count() buat datanya jadi Factor
install.packages('stargazer')
library(stargazer)
install.packages('gmodels')
library(gmodels)
install.packages('missForest') #fungsi prodNA, masalah ngisi value missing
library(missForest)
install.packages("missMDA")
library(missMDA)
install.packages("BaBooN")
library(BaBooN)
install.packages("miceRanger")
library(miceRanger)
install.packages("klaR")
library(klaR)

####Data Sets battles.csv####
#ambil data
battles <- read.csv("D:/Kuliah/ICITDA 2020/battles.csv", header = TRUE, 
                    stringsAsFactors = FALSE)
#ambil data yang dibutuhkan
battlesClean <- battles[c("attacker_1", "defender_1", "attacker_size",
                          "defender_size","battle_type", "region", "summer",
                          "attacker_outcome")]

#detil data
head(battlesClean)
summary(battlesClean)
str(battlesClean) #struktur data
describe(battlesClean)

##Data Cleaning
#visualisasi Missing Data
missmap(battlesClean)

#Ngurus nilai data yg kosong
#imputation, buat prediksi nilai data Continous yg kosong
mice_mod <- mice(battlesClean[, c("attacker_size","defender_size","summer")], 
                 method='rf')
mice_complete <- complete(mice_mod)

#Ngisi nilai prediksi data yg kosong ke data set (Data yg bentuknya nilai)
battlesClean$attacker_size <- mice_complete$attacker_size
battlesClean$defender_size <- mice_complete$defender_size
battlesClean$summer <- mice_complete$summer

#mode imputation, buat prediksi nilai data Categorical yg kosong pake modus; 
#battle_type, defender_1, attacker_outcome
battle_typeM <- battlesClean$battle_type[which.max(tabulate(match(
  battlesClean$battle_type, battlesClean)))] #modus variabel battle_type
defender_1M <- battlesClean$defender_1[which.max(tabulate(match(
  battlesClean$defender_1, battlesClean)))] #modus variabel defender_1
attacker_outcomeM <- battlesClean$attacker_outcome[which.max(tabulate(match(
  battlesClean$attacker_outcome, battlesClean)))] #modus variabel attacker_outcome

battlesClean[30, 2] <- defender_1M #ngisi nilai variabel defender_1
battlesClean[38, 5] <- battle_typeM #ngisi nilai variabel battle_type
battlesClean[38, 8] <- attacker_outcomeM #ngisi nilai variabel attacker_outcome

#konversi data yg nilainya continous jadi categorical
battlesClean$attacker_size <- cut(battlesClean$attacker_size, breaks = c(0,3000,100000), labels = c("Small", "Huge"))
battlesClean$defender_size <- cut(battlesClean$defender_size, br=c(0,5000,20000), labels = c("Small", "Huge"))
battlesClean[which(battlesClean$summer==1),7] <- "Yes"
battlesClean[which(battlesClean$summer==0),7] <- "No"

#konversi data yg tipenya char jadi Factor
battlesClean$attacker_size <- as.factor(battlesClean$attacker_size)
battlesClean$defender_size <- as.factor(battlesClean$defender_size)
battlesClean$summer <- as.factor(battlesClean$summer)
battlesClean$attacker_1 <- as.factor(battlesClean$attacker_1)
battlesClean$defender_1 <- as.factor(battlesClean$defender_1)
battlesClean$battle_type <- as.factor(battlesClean$battle_type)
battlesClean$region <- as.factor(battlesClean$region)
battlesClean$attacker_outcome <- as.factor(battlesClean$attacker_outcome)

#k-fold cv
train_control <- trainControl(method="cv", number=10)
#modelNB1 <- train(attacker_outcome ~ attacker_size + 
#                    defender_size + summer + attacker_1 + defender_1 + battle_type
#                  + region
#                  , method = "naive_bayes", trControl = train_control, data = battlesClean,
#                  tuneGrid = data.frame(laplace=1,
#                                        usekernel=TRUE,
#                                        adjust=TRUE))

modelNB1 <- train(attacker_outcome ~ attacker_size + 
                    defender_size + summer + attacker_1 + defender_1 + battle_type
                  + region
                  , method = "naive_bayes", trControl = train_control, data = battlesClean)

modelNB1$resample
confusionMatrix(modelNB1)

#repeated k-fold cv
train_control2 <- trainControl(method="repeatedcv", number=10, repeats=5)
modelNB2 <- train(attacker_outcome ~ attacker_size + 
                    defender_size + summer + attacker_1 + defender_1 + battle_type
                  + region
                  , method = "naive_bayes", trControl = train_control2, 
                  data = battlesClean)

modelNB2$resample
confusionMatrix(modelNB2)

#leave-one-out CV (LOOCV)
train_control3 <- trainControl(method="LOOCV")

modelNB3 <- train(attacker_outcome ~ attacker_size + 
                   defender_size + summer + attacker_1 + defender_1 + battle_type
                 + region, 
                 method = "naive_bayes",
                 trControl = train_control3, data = battlesClean)

modelNB3$results
