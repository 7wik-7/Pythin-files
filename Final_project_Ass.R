#FINAL PROJECT

setwd("D:/Intellipaat/Censu_project-20200918T020658Z-001/Censu_project")
CenData <- read.csv("census-income_.csv", stringsAsFactors = T)
str(CenData)
table(is.na(CenData))
CenData$workclass<-as.character(CenData$workclass)
CenData$occupation<-as.character(CenData$occupation)
CenData$native.country<-as.character(CenData$native.country)
CenData$education<-as.character(CenData$education)
CenData$marital.status<-as.character(CenData$marital.status)
CenData$relationship<-as.character(CenData$relationship)
CenData$race<-as.character(CenData$race)
CenData$sex<-as.character(CenData$sex)
CenData$X<-as.character(CenData$X)

CenData[CenData==" ?"]<- NA
table(is.na(CenData))
colSums(is.na(CenData))
CenData<-na.omit(CenData)
CenData<-CenData %>% mutate_if(is.character, str_trim)


CenData$workclass<-as.factor(CenData$workclass)
CenData$occupation<-as.factor(CenData$occupation)
CenData$native.country<-as.factor(CenData$native.country)
CenData$education<-as.factor(CenData$education)
CenData$marital.status<-as.factor(CenData$marital.status)
CenData$relationship<-as.factor(CenData$relationship)
CenData$race<-as.factor(CenData$race)
CenData$sex<-as.factor(CenData$sex)
CenData$X<-as.factor(CenData$X)
str(CenData)

library(stringr) 

library(dplyr)
census_ed <- select(CenData,education)
View(census_ed)
select(CenData,age:relationship) -> census_seq
View(census_seq)
census_col <- select(CenData,5,8,11)
View(census_col)
male_gov <-CenData %>% filter(sex == "Male" & workclass=="State-gov")
#male_gov <-filter(CenData, sex == "Male" & workclass == "State-gov")
#male_gov <-filter(CenData, sex == "Male" , workclass == "State-gov")
census_us <-CenData %>% filter(age ==39&(education == "Bachelors"|native.country=="United-States"))
View(census_us)
set.seed(7)
census_200 <-sample_n(CenData,200)
View(census_200)
workcls_cou <-count(CenData,workclass)
workcls_cou
#table(CenData$workclass)
summarise(group_by(CenData,workclass),mean(capital.gain))


library(ggplot2)
ggplot(CenData,aes(x=relationship,fill=race))+geom_bar()
ggplot(CenData,aes(x=relationship,fill=sex))+
  geom_bar(position = "dodge")+
  labs(x="Categories of Relationships",y="Count of Categories",title = "Distribution of Relationships by Sex")


ggplot(CenData, aes(x=age)) + geom_histogram(bins = 50)
ggplot(CenData,aes(x=age, fill=X))+geom_histogram(bins = 50)+
  labs(title = "Distribution of Age")+theme_bw()
ggplot(CenData,aes(x=age,fill=X))+geom_histogram(bins = 50)+
  labs(title = "Distribution of Age",fill='Yearly income')


ggplot(CenData, aes(x=capital.gain, y=hours.per.week))+geom_point(alpha=0.6,size=2)
ggplot(CenData, aes(x=capital.gain, y=hours.per.week, col=X))+geom_point(alpha=0.6,size=2)
ggplot(CenData, aes(x=capital.gain, y=hours.per.week, col=X))+geom_point(alpha=0.6,size=2)+
  labs(x="Capital Gain",y="Hours per Week",title = "Capital Gain vs Hours per Week by Income", col="Yearly Income")
  
ggplot(CenData,aes(x=education,y=age))+geom_boxplot()
ggplot(CenData,aes(x=education,y=age,fill=sex))+
  geom_boxplot()+labs(title = "Box-Plot of age by Education and Sex")

library(caTools)
set.seed(77)
sample.split(CenData$hours.per.week,SplitRatio = 0.7)-> split_tag
View(split_tag)
subset(CenData, split_tag==T)->trainCen
subset(CenData, split_tag==F)->testCen
View(trainCen)
View(testCen)
nrow(trainCen)
nrow(testCen)

LRmodelCen <- lm(hours.per.week~education.num,trainCen)
summary(LRmodelCen)

LRmodelPre <- predict(LRmodelCen,testCen)
head(LRmodelPre)
cbind(Actual=CenData$hours.per.week,Predicted=LRmodelPre)->final_data_Census
as.data.frame(final_data_Census)->final_data_Census
final_data_Census$Actual - final_data_Census$Predicted -> final_data_Census$error
View(final_data_Census)
sqrt(mean((final_data_Census$error)^2)) -> rmse1
rmse1
rm<-rmse(final_data_Census$Actual,final_data_Census$Predicted)
rm
head(final_data_Census)

#############################################
split_data1<-sample.split(CenData$X,SplitRatio = 0.65)
censusTrain1<-subset(CenData,split_data1==T)
censusTest1<-subset(CenData,split_data1==F)
nrow(censusTrain1)
nrow(censusTest1)

log_mod<-glm(X~occupation,data=censusTrain1,family = "binomial")

pred_val<-predict(log_mod,newdata =censusTest1,type = "response")#probability
head(pred_val)
range(pred_val)

library(ROCR) ## TO decide Accuracy
predict_log_roc<-prediction(pred_val,censusTest1$X)
predict_log_roc
acc<-performance(predict_log_roc,"acc")
plot(acc)## Check for which valve accuracy get constant
acc
table(CenData$X)

lm.pred<-ifelse(pred_val>0.47,">50K","<=50K")  
lm.pred

tab<-table(lm.pred,censusTest1$X)
tab


(7188+660)/(7188+660+1968+741)
accuracy<-sum(diag(tab))/sum(tab)
accuracy

# vi)	Plot the ROC curve and find the auc(Area Under Curve). 
roc<-performance(predict_log_roc,"tpr","fpr")
plot(roc)
performance(predict_log_roc, "auc")->auc
auc
auc<-auc@y.values[[1]]
auc
##########################


split_data1<-sample.split(CenData$X,SplitRatio = 0.80)
censusTrain2<-subset(CenData,split_data1==T)
censusTest2<-subset(CenData,split_data1==F)

log_mod2<-glm(X~age+workclass+education,data=censusTrain2,family = "binomial")
summary(log_mod2)
pred_val<-predict(log_mod2,newdata =censusTest2,type = "response")
head(pred_val)

library(ROCR) ## TO decide Accuracy
predict_log_roc<-prediction(pred_val,censusTest2$X)
predict_log_roc
acc<-performance(predict_log_roc,"acc")
plot(acc)
lm.pred<-ifelse(pred_val>0.45,">50K","<=50K")  
lm.pred

tab<-table(lm.pred,censusTest2$X)
tab
accuracy<-sum(diag(tab))/sum(tab)
accuracy

roc<-performance(predict_log_roc,"tpr","fpr")
plot(roc)
performance(predict_log_roc, "auc")->auc
auc
auc<-auc@y.values[[1]]
auc


library(rpart)
library(rpart.plot) 

census_tree<-rpart(X~.,trainCen,method = "class")
rpart.plot(x= census_tree, type= 5, extra = 0,tweak = 1.5)


class_tree_pre<-predict(census_tree,
                          newdata = testCen,
                          type = "class")
Con_mat<-table(class_tree_pre,testCen$X)
Con_mat
sum(diag(tab))/sum(tab)

set.seed(777)
split_data<-sample.split(CenData$X,SplitRatio = 0.8)
cenTrain<-subset(CenData,split_data==T)
cenTest<-subset(CenData,split_data==F)
nrow(cenTrain)
nrow(cenTest)

library(randomForest)

census_model<-randomForest(X~.,cenTrain,ntree=300)

plot(census_model)
text(census_model)

cenus_prediction<-predict(census_model,
                          newdata = cenTest,
                          type = "class")

con_mat_ran<-table(cenus_prediction,cenTest$X)
con_mat_ran
sum(diag(tab))/sum(tab)


importance(census_model)
varImpPlot(census_model)
varImpPlot(census_model, col="red")


