install.packages("tidyverse")

install.packages("caTools")
install.packages("knitr")
install.packages("lubridate")
install.packages("magrittr")
install.packages("modelr")
install.packages("plotly")
install.packages("data.table")
install.packages("formattable")
install.packages("corrplot")
install.packages("broom")
install.packages("ggplot2")

install.packages("lattice")
install.packages("caret")
install.packages("writexl")

library(dslabs)
library(dplyr)
library(tidyverse)
library(caTools)
library(knitr)
library(lubridate)
library(magrittr)
library(modelr)
library(plotly)
library(data.table)
library(formattable)
library(corrplot)
library(broom)
library(lattice)
library(caret)

library(writexl)


dataset = read.csv('Rx_Regional_Monthly_04_2023.csv')

#Check NA are there
colSums(is.na(dataset))

str(dataset)


#convert to integer
dataset <- dataset %>%
  mutate(TRx = as.integer(TRx),
         EUTRx = as.numeric(gsub(",", "", EUTRx)),
         RRx = as.numeric(RRx),
         NRx = as.integer(NRx))


str(dataset)

#Check the NAs that is been added to during conversion 0's converted to na 
colSums(is.na(dataset))

#Total in the dataset
nrow(dataset)

zeroRows <- dataset %>% filter(dataset$TRx == 0 | is.na(dataset$TRx))
nrow(zeroRows)
#remove the zeros as this is 20% of rows 
remRowDataset1 <- dataset %>% filter(dataset$TRx != 0 | !is.na(dataset$TRx))
nrow(remRowDataset1)

library(dplyr)
#add num id to the categories column 
colPos <- c(1,2,3,4,5)
modifiedDataset <-  remRowDataset1 %>% mutate_at(colPos, funs(factor(.)))


str(modifiedDataset)

#split it into training set
set.seed(123)
split = sample.split(modifiedDataset$TRx, SplitRatio = 0.8)
training_set = subset(modifiedDataset, split == TRUE)
test_set = subset(modifiedDataset, split == FALSE)

nrow(training_set)

nrow(test_set)


ggplot(data =test_set, mapping = aes(x = TRx, y = `Form..TLC1.`)) +
  geom_point(alpha = 0.1, aes(color = Channel))

#Since its a large dataset we use box plot to view the spread of data
#Channel wise TRx
ggplot(data = modifiedDataset, aes(x = Channel, y = log(TRx), fill = Channel)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  ggtitle("Trx by Channel") +
  scale_y_continuous("TRx (log)", labels = scales::comma) +
  scale_x_discrete("Channel") +
  coord_flip()

#Form TLC1 wise TRx
ggplot(data = modifiedDataset, aes(x = `Form..TLC1.`, y = log(TRx), fill = `Form..TLC1.`)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  ggtitle("Trx by Form TLC1") +
  scale_y_continuous("TRx (log)", labels = scales::comma) +
  scale_x_discrete("Form TLC1") +
  coord_flip()

#Channel wise Form TLC1 By TRx
ggplot(data = modifiedDataset, aes(x = `Form..TLC1.`, y = log(TRx), fill = `Channel`)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  ggtitle("Channel wise Form TLC1 By TRx") +
  scale_y_continuous("TRx (log)", labels = scales::comma) +
  scale_x_discrete("Form TLC1") +
  coord_flip()


#Correlation

library(corrplot)
#calculate correlation
distance_corr <- vapply(training_set, 
                        function(x) { cor(training_set$TRx, as.numeric(x), use = "pairwise.complete.obs") }, 
                        FUN.VALUE = numeric(1))
effect_corr <- vapply(distance_corr, function(x) { ifelse(x >= 0, "Positive", "Negative")}, 
                      FUN.VALUE = character(1))
var_corr <- names(training_set)

# create correlation table
table1 <- data.frame(var_corr, abs(distance_corr), effect_corr)
table1 <- table1[order(-abs(distance_corr)),]
names(table1) <- c("Column/Variable", "Correlation Size", "Correlation Effect")
table1

#generate corelation Table 2 on categorical column
temp <- training_set[,c(1:5)]    
num <- c(1:5)
temp <- temp %>% mutate_at(num, funs(as.numeric(.)))
#plot table of correlations
corrplot(cor(temp), type = "upper",order = "hclust", 
         tl.col = "black", tl.srt = 45)


round(cor(temp), 3)

#generate corelation Table 2 on metrics column
correl <- cor(training_set[sapply(training_set, is.numeric)], use = "pairwise.complete.obs")
#plot the corelations chart
corrplot(correl, type = "upper",order = "AOE", 
         tl.col = "black", tl.srt = 45)

round(correl, 3)



#setting the items ithats present in present in test set and not present in traning set 
data_test_new <- test_set
data_test_new$Specialty[which(!(data_test_new$Specialty %in% unique(training_set$Specialty)))] <- NA
data_test_new$Channel[which(!(data_test_new$Channel %in% unique(training_set$Channel)))] <- NA
data_test_new$ï..State[which(!(data_test_new$ï..State %in% unique(training_set$ï..State)))] <- NA
data_test_new$Form..TLC1.[which(!(data_test_new$Form..TLC1. %in% unique(training_set$Form..TLC1.)))] <- NA

#write_xlsx( data.frame(data_test_new) , "PredAlldata.xlsx")

#Regression with only dimension data
regressor = lm(formula = TRx ~ `ï..State` + Channel + Specialty + `Form..TLC1.` + `Calendar.Quarter`,
               data = training_set)

#Summary of the regression
summary(regressor)


y_pred_test = predict(regressor, newdata = data_test_new)
y_pred_test

rSquared<- summary(regressor)$r.squared
rSquared
standerdError <- summary(regressor)$sigma
standerdError


#write_xlsx( data.frame(y_pred_test) , "PredDiadata.xlsx")


#Regression with only Metrics data
regressor = lm(formula = TRx ~ `NRx` + `RRx` +`EUTRx` ,
               data = training_set)

#Summary of the regression
summary(regressor)


rSquared<- summary(regressor)$r.squared
rSquared
standerdError <- summary(regressor)$sigma
standerdError

y_pred_test = predict(regressor, newdata = data_test_new)
y_pred_test

#write_xlsx( data.frame(y_pred_test) , "PredDiadata.xlsx")


#Regression with only dimension data
regressor = lm(formula = TRx ~ Channel + `Form..TLC1.`  +`NRx` + `RRx` ,
               data = training_set)

#Summary of the regression
summary(regressor)


rSquared<- summary(regressor)$r.squared
rSquared
standerdError <- summary(regressor)$sigma
standerdError

y_pred_test = predict(regressor, newdata = test_set)
y_pred_test

#write_xlsx( data.frame(y_pred_test) , "PredDiadata.xlsx")

#Regression with only dimension data
regressor = lm(formula = TRx ~ `Channel` ,
               data = training_set)

#Summary of the regression
summary(regressor)


rSquared<- summary(regressor)$r.squared
rSquared
standerdError <- summary(regressor)$sigma
standerdError

y_pred_test = predict(regressor, newdata = data_test_new)
y_pred_test
write_xlsx( data.frame(y_pred_test) , "PredDiadata.xlsx")



#Regression with all data
regressor = lm(formula = TRx ~ . ,
               data = training_set)
summary(regressor)


rSquared<- summary(regressor)$r.squared
rSquared
standerdError <- summary(regressor)$sigma
standerdError

y_pred_test = predict(regressor, newdata = data_test_new)
y_pred_test


#write_xlsx( data.frame(data_test_new) , "TestAlldata.xlsx")
#write_xlsx( data.frame(y_pred_test) , "PredAlldata.xlsx")


rSquared<- summary(regressor)$r.squared
standerdError <- ssummary(regressor)$sigma

