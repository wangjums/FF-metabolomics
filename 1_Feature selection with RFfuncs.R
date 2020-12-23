library(caret)
library(tidyverse)
library(nnet)
library(klaR)
library(e1071)
library(ranger)
library(dplyr)
library(pROC)
library(ROCR)
########feature selection
for (i in 1:1000){
  
  set.seed(i)
  setwd("xxxx") #matrix location
  df<-read.csv("xxx.csv",header = T) #import raw data
  head(df)
  sample_name<-df[1]
  ##feature filtration with three methods
  df_n<-subset(df,select = -c(samplename,label)) 
  df_scale <-scale(df_n,center=T,scale=F)#centralized
  #zerovar=nearZeroVar(df_scale)#Detect features which have the same value
  #zerovar
  #df1=df_scale[,-zerovar] #if zerovar is not none
  df1=df_scale #if zerovar is none
  

  #descrCorr = cor(df1)
  #highCorr = findCorrelation(descrCorr, 0.90)#Detect features which share high correalation
  #highCorr
  #df2 = df1[, -highCorr] #if highCorr is not none
  df2 = df1 #if highCorr is none
  
  df2<-as.data.frame(df2)
  #comboInfo = findLinearCombos(df2)# Detect features which are in collinearity
  #comboInfo
  #df2=df2[, -comboInfo$remove] #if comboInfo is not none
  #df2=df_scale #if comboInfo is none
  str(df2)
  Label<-as.factor(df[,2])
  
  ##feature selection based on rfFuncs
  subsets = c(15:20)#set the feature selection number as x
  ctrl= rfeControl(functions = rfFuncs, method = "cv",verbose = FALSE, returnResamp = "final") #set feature selection method and cross validation method
  Profile = rfe(df2, Label, sizes = subsets, rfeControl = ctrl)
  print(Profile)#check the accuracy of predicition model to decide the selected feature number
  plot(Profile)
  Profile$optVariables
  Profile$optsize
  
  feature_new<-as.data.frame(Profile$optVariables)#extract the slected features
  setwd("xxxx")
  write.table(feature_new,file=paste("Opt",i,".csv"),sep=",",row.names = F)
}

######The AUC(ROC) results of different feature numbers
library(caret)
library(tidyverse)
library(nnet)
library(klaR)
library(e1071)
library(ranger)
library(dplyr)
library(pROC)
library(ROCR)
setwd("xxxx")
file_path <- "xxxx"
files_list <- list.files(file_path, pattern = "Opt")
All_data <- data.frame(File_name = character(),AUC = numeric(), Feature_number = numeric(),Feature = character())

for(x in files_list){
  temp_data <- read_csv(x) %>% mutate(file_name = x)
  df<-read.csv("xxx.csv",header = T)
  features <- temp_data$`Profile$optVariables`
  num <- length(features)
  df_n <- df%>%dplyr::select(features)
  df_scale <-scale(df_n,center=T,scale=F)
  df3<-df_scale
  Label<-as.factor(df[,2])
  inTrain = createDataPartition(Label, p = 3/4, list = FALSE)
  trainx = df3[inTrain,]
  testx = df3[-inTrain,]
  trainy = Label[inTrain]
  testy = Label[-inTrain]

  fitControl = trainControl(method = "repeatedcv", number = 10, repeats = 3,returnResamp = "all")
  grid_rf <- expand.grid(.mtry=c(1,2,3)) #set hyper-parameter 
  rfFit1 = train(trainx,trainy,method = "rf",trControl = fitControl,tuneGrid = grid_rf,verbose = FALSE)
  plot(rfFit1)
  vip <- as.data.frame(varImp(rfFit1)[1])
  vip$features <- rownames(vip)
  vip1 <- arrange(vip,-Overall)
  vip2 <- paste(vip1$features,collapse = ";")
  x1 <- str_extract(x,"\\s\\d+")
  file_name <- paste("Feature_importance",x1,".csv",sep = "")
  write.csv(vip1,file = file_name,row.names = F)
  models_rf=list(rfFit1)
  predValues = extractPrediction(models_rf,testX = testx, testY = testy)
  head(predValues)
  testValues = subset(predValues, dataType == "Test")#Extract the test set result
  probValues = extractProb(models_rf,testX = testx, testY = testy)#The prediction result of train and test setÂÊ

  testProbs = subset(probValues, dataType == "Test")#Extract the result and probability of test set
  head(testValues)
  Pred1 = subset(testValues, model == "rf")
  confusionMatrix(Pred1$pred, Pred1$obs)#Generate the confusion matrix
  #ROC
  testProbs$lable=ifelse(testProbs$obs=='P',yes=1,0)
  pred1 = prediction(testProbs$P,testProbs$lable)
  perf1 = performance(pred1, measure="tpr", x.measure="fpr" )
  pred.rocr.auc.perf = performance(pred1,measure = "auc",x.measure = "cutoff")
  AUC<- as.data.frame(pred.rocr.auc.perf@y.values[[1]])
  auc1 <- AUC$`pred.rocr.auc.perf@y.values[[1]]`
  temp_data <- data.frame(File_name = file_name,AUC = auc1, Feature_number = num,Feature = vip2)
  All_data <- rbind(All_data,temp_data)
}
write.csv(All_data,"All_features_results.csv",row.names = F)
a1 <- All_data%>%dplyr::select(2,3)%>%group_by(Feature_number)%>%
  summarise_all(sd) 
a2 <- All_data%>%dplyr::select(2,3)%>%group_by(Feature_number)%>%
  summarise_all(mean) 
a3<- All_data%>%dplyr::select(2,3)%>%
  group_by(Feature_number)%>%
  #tally()
  do(data.frame(nrow=nrow(.)))
a12 <- full_join(a1,a2,"Feature_number")
a123 <- full_join(a12,a3,"Feature_number")
colnames(a123) <- c("Feature_number","sd","mean","number")
write.csv(a123,"All_feautres_result_summary.csv",row.names = F)

