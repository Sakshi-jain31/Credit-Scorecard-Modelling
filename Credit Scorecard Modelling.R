setwd("C:/Users/Sakshi/Desktop/Github Projects/Credit Risk Model and Scorecard")
library(tidyverse)
library(dplyr)
library(DataExplorer)
library(purrr)
library(httr)
install.packages("Information")
library(Information)
library(gmodels)
library(pROC)
install.packages("scorecard")
library(scorecard)
install.packages("skimr")
library(skimr)
library(forcats)
url <- ("http://www.biz.uiowa.edu/faculty/jledolter/DataMining/germancredit.csv")
GET(url, write_disk(tf <- tempfile(fileext = ".data")))
data=read.csv(tf)

###############################Exploratory Data Analysis################
plot_missing(data)
summary(data)
str(data)
data$age=as.numeric(data$age)
data$duration=as.numeric(data$duration)
data$amount=as.numeric(data$amount)
names(data)=tolower(names(data))
data <- data %>% 
  map_if(is.integer,as.character) %>% 
  data.frame() 
data$default=as.numeric(data$default)
data$default=recode(data$default, '1'=0L, '2'=1L)
View(data)

#######################################Feature Engineering#################

data$savings <- fct_recode(data$savings, less100DM='A61',f100to500DM='A62',f500to1000DM='A63',greater1000DM='A64',unknown='A65')
data$employ <- fct_recode(data$employ, unemployed='A71',less1year='A72',f1to4years='A73',f4to7years='A74',over7years='A75')
data$job <- fct_recode(data$job, unemployed='A171',unskilled='A172',skilled='A173',management='A174')
data$foreign <- fct_recode(data$foreign, yes='A201',no='A202')
data$housing <- fct_recode(data$housing, rent='A151',own='A152',free='A153')
data$tele <- fct_recode(data$tele, none='A191',yes='A192')

data$checkingstatus1 <- fct_recode(data$checkingstatus1, less0='A11', low0to200DM='A12', highover200DM='A13', noaccount='A14')
data$history <- fct_recode(data$history, nocredit='A30', creditpaid='A31',creditpaidtilnow ='A32', pastdelays='A33',otherbankcredit='A34')
data$purpose <- fct_recode(data$purpose, car_new='A40', car_used='A41',furniture ='A42', radio_tv='A43',appliance='A44', repairs='A45', education='A46',  retraining='A48', business='A49', others='A410') #vacation='A47'does not exist in data
data$others <- fct_recode(data$others, noguarantors='A101', coapplicant='A102',guarantor ='A103')
data$property <- fct_recode(data$property, realestate='A121', othersavings='A122',othercar ='A123',noproperty='A124')
data$otherplans <- fct_recode(data$otherplans, bankplans='A141', storeplans='A142',noplans ='A143')
data$status <- fct_recode(data$status, male_other='A91',female_other='A92', male_single='A93', male_other='A94', female_single='A95')

#####Converting continuos variable into bins

data <- data %>% 
  mutate(age_group=factor(case_when(
    .$age <  25 ~ "25less",
    .$age >= 25 & .$age <= 29 ~ "25to29",
    .$age >= 30 & .$age <= 39 ~ "30to39",
    .$age >= 40 & .$age <= 49 ~ "40to49",
    .$age >= 50 & .$age <= 59 ~ "50to59",
    .$age >= 60 & .$age <= 70 ~ "60over",
    .$age >= 70 ~ "6")))


data <- data %>% 
  mutate(amount_group=factor(case_when(
    .$amount <  1250 ~ "1250less",
    .$amount >= 1250 & .$amount <= 5000 ~ "1250to5000",
    .$amount >= 5000  ~ "5000over")))


data <- data %>% 
  mutate(duration_group=factor(case_when(
    .$ duration <  12 ~ "1yearunder",
    .$ duration >= 12 & .$ duration <= 24 ~ "1to2year",
    .$ duration >= 24 & .$ duration <= 36 ~ "2to3year",
    .$ duration >= 36 & .$ duration <= 48 ~ "3to4year",
    .$ duration >= 48  ~ "4yearover")))


data <- data[,-c(3,6,14)] #deletes columns 5 and 7

####################Logistics Regression with manual binning############

View(data)
library(caTools)
set.seed(1000)
split=sample.split(data,SplitRatio = 0.70)
train=subset(data,split==TRUE)
test=subset(data,split==FALSE)

glm_model <- glm(default ~ ., family = "binomial", data= train)

prediction_prob <- predict(glm_model, newdata = test, type = "response")
ROC <- pROC::roc(test$default,prediction_prob)
# AUC for the logistic model
AUC <- pROC::auc(ROC )
AUC


# KS for logistic
pred <- ROCR::prediction(prediction_prob,test$default)
perf <- ROCR::performance(pred,"tpr","fpr")
KS <- max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
KS


# performance
test_perf <- scorecard::perf_eva(prediction_prob, test$default, 
                                 title = "Model 1")

########################Logistics Regression with WOE Binning###########

creditdata_base=data
# Create a WOE table
IV <- create_infotables(data=creditdata_base,
                        y="default")
# Print the summary of the IV table information values. Thes IVs are derived from the WOE.
IV$Summary %>% 
  knitr::kable()


creditdata_filt <- var_filter(creditdata_base, y="default")
# Print the remaining number variables of the filtered dataset
dim(creditdata_filt)[2]
# Weight of average (WOE) binning 
bins <- woebin(creditdata_filt, y="default")
woebin_plot(bins$age)
woebin_plot(bins$duration)
woebin_plot(bins$amount)
woebin_plot(bins$checkingstatus1)
training_woe <- woebin_ply(train, bins)
test_woe <- woebin_ply(test, bins)
glm_model2 <- glm(default ~ ., family = "binomial", data = training_woe)

prediction_prob2 <- predict(glm_model2, newdata = test_woe, type = "response")
# Calculate the ROC
ROC_2 <- pROC::roc(test_woe$default,prediction_prob2)
# AUC for fourth model
AUC2 <- auc(ROC_2)
AUC2


# KS for fourth model
prediction_2 <- ROCR::prediction(prediction_prob2,test_woe$default)
perf_2 <- ROCR::performance(prediction_2,"tpr","fpr")
KS2 <- max(attr(perf_2,'y.values')[[1]]-attr(perf_2,'x.values')[[1]])
KS2


# Performance of the WOE model
test_perf <- perf_eva(prediction_prob2, test_woe$default, title = "Model 2")


# Calculate score card
card <- scorecard(bins, glm_model2)
# Take a look at the scorecard for duration, which includes scorecard points
card$duration %>% 
  knitr::kable()
AUC
AUC2







