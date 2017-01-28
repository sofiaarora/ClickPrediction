# This model predicts when an ad will be clicked or not.

#installing packages
install.packages("caret")
install.packages("plyr")
install.packages("dplyr")
install.packages("dummies")
install.packages("DMwR")
install.packages("e1071")
install.packages("rpart")
install.packages("randomForest")
library(caret)
library(plyr)
library(dplyr)
library(dummies)
library(DMwR)
library(e1071)
library(rpart)
library(randomForest)
library(ROCR)
#reading the file from the sample codes provided
file<-read.delim("/Users/sofiaarora/Desktop/code-test0000_part_00",sep="|",
                 quote=NULL,header=F,colClasses = "character",na.strings ="")
#renaming the columns
colnames(file)<-c("timestamp","placement_id","browser_id","os_id","region",
                  "country","is_adserver","campaign","creative_asset_id",
                  "mouseovers","clicks","max_duration","video_length","viewable")
str(file)
#changing the datatypes of the variables
index<-sapply(file,is.character)  
file[index]<-lapply(file[index],as.factor)#datatype conversion from character to factor
file$placement_id<-as.numeric(as.character(file$placement_id))
file$campaign<-as.numeric(as.character(file$campaign))
file$creative_asset_id<-as.numeric(as.character(file$creative_asset_id))

summary(file$clicks)

#removing is_adserver(ignored column)
#max_duration and video_length are empty predictors(1032653 missing values) hence removing them too
file$is_adserver<-NULL
file$max_duration<-NULL
file$video_length<-NULL

#checking missing values

table(is.na(file))
colSums(is.na(file))

#missing value treatment
file<-file[complete.cases(file$region),]

#removing non-informative predictors
file$timestamp<-NULL

#recoding few predictors
table(file$browser_id)
file$browser_id<-revalue(file$browser_id,c("0"="Other","1"="Other",
                                           "10"="Other","14"="Other","15"="Other",
                                           "5"="Other","7"="Other","8"="Other",
                                           "9"="Other",
                                           "11"="Other","12"="Other"))

file$os_id<-revalue(file$os_id,c("0"="OtherOS","1"="OtherOS","10"="OtherOS",
                                 "4"="OtherOS","9"="OtherOS"))
levels(file$country)[c(1:138,140:145)]<-"Non_US"

file$region<-as.character(file$region)
file$region[file$country!="US"]<-"OtherRegions"
file$region<-as.factor(file$region)

#creating dummy variables

file<-dummy.data.frame(file, names = c('browser_id','os_id','region', 
                                      'country'),  sep='_')

set.seed(1)
#splitting the highly imbalanced data in training and test sets
splitIndex<-createDataPartition(file$clicks,p=0.5,list=FALSE,times=1)
trainSplit<-file[splitIndex,]
testSplit<-file[-splitIndex,]

#applying the technique of SMOTE to oversample 1s and undersample 0s
trainSplit<-SMOTE(clicks ~.,trainSplit,perc.over=100,perc.under=400)

#best fit model
rfmodel<-randomForest(clicks ~ placement_id+ browser_id_Other+ browser_id_13+ browser_id_16+ 
                        browser_id_2+ browser_id_3+ browser_id_4+ os_id_OtherOS+ 
                        os_id_2+ os_id_3+ os_id_5+ os_id_7+ os_id_8+ region_AK+ 
                        region_AL+ region_AR+ region_AZ+ region_CA+ region_CO+ 
                        region_CT+ region_DC+ region_DE+ region_ENG+ region_FL+ 
                        region_GA+ region_HI+ region_IA+ region_ID+ region_IL+ 
                        region_IN+ region_KS+ region_KY+ region_LA+ region_MA+ 
                        region_MD+ region_ME+ region_MI+ region_MN+ region_MO+ 
                        region_MS+ region_MT+ region_NC+ region_ND+ region_NE+ 
                        region_NH+ region_NJ+ region_NM+ region_NV+ region_NY+ 
                        region_OH+ region_OK+ region_ON+ region_OR+ region_OtherRegions+ 
                        region_OXF+ region_PA+ region_PTF+ region_QC+ region_QLD+ 
                        region_RI+ region_SC+ region_SD+ region_SP+ region_STT+ 
                        region_SWA+ region_TN+ region_TX+ region_UT+ region_VA+ 
                        region_VIC+ region_VT+ region_WA+ region_WB+ region_WDU+ 
                        region_WI+ region_WKF+ region_WKO+ region_WLL+ region_WLV+ 
                        region_WOR+ region_WRL+ region_WRT+ region_WSX+ region_WV+ 
                        region_WX+ region_WY+ region_Y+ region_YOR+country_Non_US+
                        country_US+ campaign+ creative_asset_id+ mouseovers+ 
                        viewable,data=trainSplit,importance=TRUE,ntree=100)
pred.rfmodel<-predict(rfmodel,newdata=testSplit)
table(pred.rfmodel,testSplit$clicks)
test.forest<-predict(rfmodel,type="prob",newdata=testSplit)
forestprediction<-prediction(test.forest[,2],testSplit$clicks)
forestperf = performance(forestprediction, 'tpr', 'fpr')
plot(forestperf) #plotting ROC curve






 

