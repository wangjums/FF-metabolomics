library(caret)
library(tidyverse)
library(nnet)
library(klaR)
library(e1071)
library(ranger)
library(dplyr)
#library(pROC)
library(ROCR)
library(cutpointr)
library(extrafont)
library(export)
setwd("C:/Users/wangju/Desktop/subgroupE/subE_opt_ROC") #修改路径
AUC_ROCR<-as.data.frame("AUC")
Train_acc<-as.data.frame("ACC_Train")
Test_acc <- as.data.frame("ACC_Test")
pre2=NULL
act2=NULL
import=NULL
for (i in 1:100){

  set.seed(i)
  df<-read.csv("data6_mac _6F_E.csv",header = T) #修改文件名
  df_n<-subset(df,select = -c(samplename,label)) 
  df_scale <-scale(df_n,center=T,scale=F)#标准化
  df3<-df_scale
  Label<-as.factor(df[,2])
  #分训练集和测试集数据
  inTrain = createDataPartition(Label, p = 3/4, list = FALSE)
  trainx = df3[inTrain,]
  testx = df3[-inTrain,]
  trainy = Label[inTrain]
  testy = Label[-inTrain]
  ##rf建模
  #选样本抽样方式（10foldsCV,重复3次)
  fitControl = trainControl(method = "repeatedcv", number = 10, repeats = 3,returnResamp = "all")
  #给模型调参
  grid_rf <- expand.grid(.mtry=c(1,2,3))
  #迭代建模
  rfFit1 = train(trainx,trainy,method = "rf",trControl = fitControl,tuneGrid = grid_rf,verbose = FALSE)
  plot(rfFit1)#调参结果
  con1 <- rfFit1$finalModel$confusion
  acc_train <- as.data.frame(sum(diag(con1))/sum(con1))
  colnames(acc_train) <- i
  Train_acc<-merge(acc_train,Train_acc)
  vip <- as.data.frame(varImp(rfFit1)[1])
  vip$features <- rownames(vip)
  import=rbind(import,vip)
  ##验证集结果
  models_rf=list(rfFit1)
  predValues = extractPrediction(models_rf,testX = testx, testY = testy)#训练集和验证集的预测结果和实际结果
  head(predValues)
  testValues = subset(predValues, dataType == "Test")#提取验证集的结果
  probValues = extractProb(models_rf,testX = testx, testY = testy)#训练集和验证集的预测结果和实际结果以及概率
  testProbs = subset(probValues, dataType == "Test")#提取验证集的结果以及概率
  head(testValues)
  Pred1 = subset(testValues, model == "rf")
  test <- confusionMatrix(Pred1$pred, Pred1$obs)$table#生成验证集的混合矩阵
  acc_test <- as.data.frame(sum(diag(test))/sum(test))
  colnames(acc_test) <- i
  Test_acc <- merge(acc_test,Test_acc)
  #绘制ROC曲线
  testProbs$lable=ifelse(testProbs$obs=='B',yes=1,0)
  pred1 = prediction(testProbs$B,testProbs$lable)
  pre2 = c(pred1@predictions,pre2)
  act2 = c(pred1@labels,act2)
  perf1 = performance(pred1, measure="tpr", x.measure="fpr" )#计算tpr和fpr用来绘制ROC
  pred.rocr.auc.perf = performance(pred1,measure = "auc",x.measure = "cutoff")
  AUC<- as.data.frame(pred.rocr.auc.perf@y.values[[1]])
  colnames(AUC) <- i
  AUC_ROCR<-merge(AUC,AUC_ROCR)
}
AUC_ROCR
write.table(AUC_ROCR,"data6_mac _6F_E_auc100.csv",sep=",",row.names = F)
Train_acc
Test_acc
names(Test_acc)[101] <- "ACC"
names(Train_acc)[101] <- "ACC"
ACC <- rbind(Train_acc,Test_acc)
write.table(Train_acc,"Train_acc.csv",sep=",",row.names = F)
write.table(Test_acc,"Test_acc.csv",sep=",",row.names = F)
mean(as.numeric(AUC_ROCR[1,1:100]))

#####################计算importance
library(Hmisc)
importance1 <- import %>% group_by(features) %>% 
  dplyr::summarise(Mean = mean(Overall), SD = sd(Overall))
colnames(importance1) <- c("Features","Importance_Mean","Importance_SD")
importance2 <- arrange(importance1,desc(importance1$Importance_Mean))
label <- data.frame(Features=c("C2","PCaaC406","PCaaC421","PCaeC341","PCaeC404","PCaeC406","PCaeC446","SM160","E1","VD3","BMI","FSH","Ie2","RON"),
                    Group = c("ACR","PC","PC","PC","PC","PC","PC","SP","HO","Vit","CL","HO","HO","CL"))
im3 <- merge(importance2,label,id="Features")
im3$Features=capitalize(im3$Features)

p1 <- ggplot(im3,aes(reorder(Features,Importance_Mean),Importance_Mean,fill=Group))+
  geom_bar(stat = "identity")+coord_flip()+
  geom_errorbar(aes(x=reorder(Features,Importance_Mean), ymin= Importance_Mean - Importance_SD, ymax = Importance_Mean + Importance_SD),
                width = 0.4, size = 1)+theme_bw()+
  ylab("Importance")+xlab("Metabolites")+
  theme(axis.title = element_text(family="Times New Roman",face="bold",size = 18),
        axis.text = element_text(family="Times New Roman",face="bold",size = 14),
        legend.title = element_text(family="Times New Roman",face="bold",size = 18),
        legend.text = element_text(family="Times New Roman",face="bold",size = 14))
p1
########################改文件名
graph2pdf(file="Bar7.pdf",width=5,height=5)
write.table(AUC_ROCR,"All_ROC7.csv",sep=",",row.names = F)
write.table(ACC,"All_ACC.csv",sep=",",row.names = F)
write.table(importance2,"Feature_importance_range7.csv",sep=",",row.names = F)
###########绘制多样本ROC曲线
mean_roc <- function(data, cutoffs = seq(from = 0, to = 1, by = 0.05)) {
  map_df(cutoffs, function(cp) {
    out <- cutpointr(data = data, x = PredictionValues, class = RealClass,
                     subgroup = Sample, method = oc_manual, cutpoint = cp,
                     pos_class = "1", direction = ">=")
    data.frame(cutoff = cp, 
               sensitivity = mean(out$sensitivity),
               specificity = mean(out$specificity))
  })
}
#############ROC图数据构建
table(testy)#看验证集数目
sample=NULL
for (i in 1:100){
  sample=c(sample,rep(i,times=24))  #根据验证集样本数目修改该值
  i=i+1
}
predictions_samples <- data.frame(
  PredictionValues = unlist(pre2),
  RealClass = unlist(act2)
)
predictions_samples$Sample <- sample
data1 <- cutpointr(data = predictions_samples, 
                   x = PredictionValues, class = RealClass, subgroup = Sample,
                   pos_class = "1", direction = ">=")
AUC_all <- data1$AUC
mean(AUC_all)
mr <- mean_roc(predictions_samples)
data_cut <- data.frame(Cutoff = mr$cutoff, Sensitivity = mr$sensitivity, Specificity = 1-mr$specificity, Sum =  mr$sensitivity+mr$specificity-1)
Opt_cutoff <- data_cut%>%filter(Sum==max(Sum))
Opt_cutoff#标图上的数据
####Plot ROC
p2 <- cutpointr(data = predictions_samples, 
          x = PredictionValues, class = RealClass, subgroup = Sample,
          pos_class = "1", direction = ">=") %>% 
  plot_roc(display_cutpoint = F) + theme_bw() +
  geom_line(data = mr, mapping = aes(x = 1 - specificity, y = sensitivity), 
            color = "black",size=1.2)+
  geom_point(aes(x=Opt_cutoff$Specificity, y = Opt_cutoff$Sensitivity),col="red",size=2.5)+
  annotate("text",x=Opt_cutoff$Specificity+.1, y = Opt_cutoff$Sensitivity, label = "0.40 (0.349, 0.708)", #自己改文字和位置
           family="Times New Roman",fontface="bold",size = 6)+
  annotate("text",x=0.9,y=0.1,label = "AUC 0.745",family="Times New Roman",fontface="bold",size = 6)+  #自己改文字和位置
  theme(legend.position="none",
        axis.title = element_text(family="Times New Roman",face="bold",size = 18),
        axis.text = element_text(family="Times New Roman",face="bold",size = 14))
p2
library(Rmisc)
########################改文件名
graph2pdf(file="ROC7.pdf",width=5,height=5)
multiplot(p2,p1,cols = 2)
graph2pdf(file="All7.pdf",width=10,height=5)  #自己改名字
write.csv(predictions_samples,"ROC_result7.csv",row.names = F) #改名字

