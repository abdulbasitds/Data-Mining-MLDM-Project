df = read.csv("stackdata.csv")

#df = df[df$Country %in% c("France","Germany","Austria"),]
#df = df[df$Country %in% c("United States"),]

install.packages("dplyr")
install.packages("caret")
install.packages("magrittr")
install.packages("e1071")
install.packages("randomForest")



library(forecast)
library(randomForest)
library(dplyr)
library(magrittr) 
library(knitr)
library(e1071)

#na.omit(df$Salary)
df = df[!is.na(df$Salary),]
  df = df[!is.na(df$FormalEducation),]
  df$Salary = as.numeric(df$Salary)



  
  #na.omit(df$Salary)
  df = df[!is.na(df$ConvertedSalary),]
  df = df[!is.na(df$FormalEducation),]
  df = df[!is.na(df$Country),]
  df$ConvertedSalary = as.numeric(df$ConvertedSalary)
  
  feducatio_mean = df %>%
    group_by(FormalEducation) %>%
    summarise(mean_salary = mean(ConvertedSalary))
  kable(feducatio_mean)
  
  


cmean_n = df %>%
  group_by(Country) %>%
  summarise(mean_salary = mean(ConvertedSalary)) %>%
  arrange(desc(mean_salary))

cmean_n= cmean_n[cmean_n$Country %in% c("France","Germany","Austria","Sweden","Switzerland","Belgium","Luxembourg","Netherlands","Spain","Italy","Finland","Norway","Denmark"),]


ggplot(cmean_n, aes(x=Country, y=mean_salary)) + geom_bar(stat = "identity",fill = "#FF6666")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename="myPlot1.jpg", plot=last_plot())




#na.omit(df$Salary)

df = df[!is.na(df$YearsCoding),]
df$ConvertedSalary = as.numeric(df$ConvertedSalary)

feducatio_mean = df %>%
  group_by(YearsCoding) %>%
  summarise(mean_salary = mean(ConvertedSalary))
kable(feducatio_mean)

ggplot(feducatio_mean, aes(x=YearsCoding, y=mean_salary)) + geom_bar(stat = "identity",fill = "darkslategray2")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename="myPlot2.jpg", plot=last_plot())

df = df[df$ConvertedSalary>(1498*12),]
df = df[df$ConvertedSalary <250000,]

#Missing percentage per coloumn
clmeans= sort(colMeans(is.na(df)))

#drop according to missing percentage
drops <- c("AdsPriorities7", "AdsAgreeDisagree3", "AdsPriorities6", "JobEmailPriorities5", "JobEmailPriorities1", "JobEmailPriorities4", "AdBlockReasons", "JobContactPriorities4", "StackOverflowJobRecommend", "AIInteresting", "VersionControl", "SurveyEasy", "AdsPriorities2", "AdsAgreeDisagree2", "StackOverflowParticipate", "JobEmailPriorities6", "AdBlockerReasons", "DatabaseDesireNextYear", "JobEmailPriorities7", "EngonomicDevices", "AdBlocker", "CurrencySymbol", "HypotheticalTools4", "PlatformDesireNextYear", "AdBlockerDisable", "ErgonomicDevices", "AdsPriorities5", "CommunicationTools", "AdsAgreeDisagree1", "LanguageDesireNextYear", "HackatonReasons", "AdsPriorities3", "LanguageWorkedWith", "AIFuture", "AdsActions", "StackOverflowVisit", "StackOverflowRecommend", "TimeAfterBootcamp", "IDE", "StackOverflowHasAccount", "AdsPriorities1", "FrameworkWorkedWith", "DatabaseWorkedWith", "HypotheticalTools1", "SurveyTooEasy", "FrameworkDesireNextYear", "MilitaryUS", "JobContactPriorities2", "StackOverflowDevStory", "JobContactPriorities3", "Respondent", "StackOverflowJobsRecommend", "StackOverflowJobs", "UpdateCV", "PlatformWorkedWith", "AdsPriorities4", "AIDangerous", "AIResponsible", "JobEmailPriorities3", "HypotheticalTools3", "JobContactPriorities5", "JobContactPriorities1", "JobEmailPriorities2", "CheckInCode", "HackathonReasons", "SurveyTooLong", "HypotheticalTools2", "HypotheticalTools5")
selected_features = colnames( df[,!(names(df) %in% drops)] )
df_sf = df[,!(names(df) %in% drops)]


unique_count = apply(df_sf, 2, function(x) length(unique(x)))
df_sf[2,]$ConvertedSalary
df_sf[2,]$Country
csalary_temp = df_sf$ConvertedSalary
df_sf = df_sf[,unique_count<200]
df_sf$ConvertedSalary = csalary_temp
df_sf[2,]$ConvertedSalary
df_sf[2,]$Country
colnames(df_sf)

df_sf = df_sf[df_sf$Country %in% c("France","Germany","Austria","Sweden","Switzerland","Belgium","Luxembourg","Netherlands","Spain","Italy","Finland","Norway","Denmark"),]
#drop according to missing percentage

drops <- c("Salary","SalaryType" , "Country" , "Currency")
selected_features = colnames( df_sf[,!(names(df_sf) %in% drops)] )
df_sf = df_sf[,!(names(df_sf) %in% drops)]


df_sf = na.omit(df_sf)

#Bining the Salary in 4 classes
df_sf$group <- as.numeric(cut(df_sf$ConvertedSalary, 4))
#df_sf = df_sf[,-c("ConvertedSalary")]
tempsalary = df_sf$ConvertedSalary
df_sf$ConvertedSalary = NULL
numerical_colomns = split(names(df_sf),sapply(df_sf, function(x) paste(class(x), collapse=" ")))$numeric
categorical_colomns = split(names(df_sf),sapply(df_sf, function(x) paste(class(x), collapse=" ")))$factor
interger_colomns = split(names(df_sf),sapply(df_sf, function(x) paste(class(x), collapse=" ")))$integer


#df_sf = df_sf[,!(names(df_sf) %in% interger_colomns)]
drop_final = c('AssessBenefits3', 'AssessBenefits4', 'AssessBenefits6', 'AssessBenefits9', 'AssessBenefits10','AssessJob3', 'AssessJob5', 'AssessJob7')
df_sf = df_sf[,!(names(df_sf) %in% drop_final)]


library(caret)
dummyv<- dummyVars(" ~ .", data = df_sf)
hencoded <- data.frame(predict(dummyv, newdata = df_sf))
colnames(hencoded)



df_sf$ConvertedSalary = NULL
dummyv_cat<- dummyVars(" ~ .", data = df_sf)
hencoded_cat <- data.frame(predict(dummyv_cat, newdata = df_sf))
colnames(hencoded_cat)
#data(mtcars)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(hencoded_cat))

## set the seed to make your partition reproducible
set.seed(123)
train_ind_cat <- sample(seq_len(nrow(hencoded_cat)), size = smp_size)

train_cat <- hencoded_cat[train_ind_cat, ]
test_cat <- hencoded_cat[-train_ind_cat, ]



#y = hencoded$group

train_cat_y<- factor(train_cat$group)
train_cat_X = train_cat[,names(train_cat) != "group"]
test_cat_y<- factor(test_cat$group)
test_cat_X = test_cat[,names(test_cat) != "group"]



###############################################################
# SVM
#############################################################


#svm_model_cat <- svm(train_cat_X,train_cat_y,kernel="radial",degree=8,gamma=1,cost=1,probability = TRUE)

svm_model_cat <- svm(train_cat_X,train_cat_y,kernel="polynomial", degree=8,cost=1,gamma = 0.1,epsilon = 0.05)


#tuned_parameters <- tune.svm(train_X,train_y, gamma = 10^(-5:-1), cost = 10^(-3:1))
#rbf.tune = tune.svm(train_X,train_y,probability = TRUE, kernel="radial",
#                   gamma=c(0.1,0.5,1,2,3,4))

best.rbf = tuned_parameters$best.model
#rbf.test = predict(best.rbf, newdata=test)

summary(svm_model)
pred=NULL
pred_cat <- predict(svm_model_cat, test_cat_X,decision.values = TRUE, probability = TRUE)
acc_table = table(pred_cat,test_cat_y)
cm_cat = confusionMatrix(acc_table)
cm_cat$overall['Accuracy']
attr(pred, "probabilities")
acc_table
accuracy(test_cat,pred_cat)
cm_cat$overall['Accuracy']
#pred
#table(pred, y)

###############################################################
#Random Forrest
#############################################################

model1 <- randomForest(train_cat_X,train_cat_y, importance = TRUE)
model1
predTrain <- predict(model1, test_X, type = "class")
acc_table = table(predTrain,test_y)
confusionMatrix(acc_table)


###############################################################
## Hyperparameter Tuning
###############################################################

#Tune function can be used for searching various hyperparamers


#Tune function can be used for searching various hyperparamers
#Lets use this to find good values for "c" and "gamma"


svm_tuned = tune.svm(train_cat_x,train_cat_y,kernel="polynomial",degree=8 , gamma = c(0.1,0.5,1),cost=c(1,2,5,10))
svm_tuned$best.parameters


###############################################################
#As Regression  Problem
#############################################################
df_sf$ConvertedSalary = tempsalary
dummyv<- dummyVars(" ~ .", data = df_sf)
hencoded <- data.frame(predict(dummyv, newdata = df_sf))
colnames(hencoded)


dummyv<- dummyVars(" ~ .", data = df_sf)
hencoded <- data.frame(predict(dummyv, newdata = df_sf))
colnames(hencoded)
smp_size <- floor(0.75 * nrow(hencoded))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(hencoded)), size = smp_size)

train <- hencoded[train_ind, ]
test <- hencoded[-train_ind, ]

train_y = train$ConvertedSalary
train_X = train[,names(train) != "ConvertedSalary"]
test_y = test$ConvertedSalary
test_X = test[,names(test) != "ConvertedSalary"]

#svm_model <- svm(train_X,train_y, kernel = "polynomial", gamma = 1, cost = 1,coef0 = 1)
svm_model_regression <- svm(train_X,train_y, kernel="polynomial", degree=8,cost=1,gamma = 0.1,epsilon = 0.05,coef0 = 1)
#svm_model <- svm(train_X,train_y, kernel = "polynomial", degree=8, gamma = 0.1, cost = 10,epsilon = 0.05)


predicted <- predict(svm_model_regression, test_X)
RMSE_Custom = sqrt(mean(predicted - test_y)^2)
RMSE_Custom



