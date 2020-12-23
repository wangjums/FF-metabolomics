library(caret)
library(tidyverse)
library(nnet)
library(klaR)
library(e1071)
library(ranger)
library(dplyr)
library(ROCR)
library(cutpointr)
library(extrafont)
library(export)
setwd("XXXX") 
AUC_ROCR<-as.data.frame("AUC")
Train_acc<-as.data.frame("ACC_Train")
Test_acc <- as.data.frame("ACC_Test")
pre2=NULL
act2=NULL
import=NULL
for (i in 1:100){

  set.seed(i)
  df<-read.csv("XXX.csv",header = T) 
  df_n<-subset(df,select = -c(samplename,label)) 
  df_scale <-scale(df_n,center=T,scale=F)
  df3<-df_scale
  Label<-as.factor(df[,2])
  inTrain = createDataPartition(Label, p = 3/4, list = FALSE)
  trainx = df3[inTrain,]
  testx = df3[-inTrain,]
  trainy = Label[inTrain]
  testy = Label[-inTrain]
  fitControl = trainControl(method = "repeatedcv", number = 10, repeats = 3,returnResamp = "all")
  grid_rf <- expand.grid(.mtry=c(1,2,3))
  rfFit1 = train(trainx,trainy,method = "rf",trControl = fitControl,tuneGrid = grid_rf,verbose = FALSE)
  plot(rfFit1)
  con1 <- rfFit1$finalModel$confusion
  acc_train <- as.data.frame(sum(diag(con1))/sum(con1))
  colnames(acc_train) <- i
  Train_acc<-merge(acc_train,Train_acc)
  vip <- as.data.frame(varImp(rfFit1)[1])
  vip$features <- rownames(vip)
  import=rbind(import,vip)
  models_rf=list(rfFit1)
  predValues = extractPrediction(models_rf,testX = testx, testY = testy)
  head(predValues)
  testValues = subset(predValues, dataType == "Test")
  probValues = extractProb(models_rf,testX = testx, testY = testy)
  testProbs = subset(probValues, dataType == "Test")
  head(testValues)
  Pred1 = subset(testValues, model == "rf")
  test <- confusionMatrix(Pred1$pred, Pred1$obs)$table
  acc_test <- as.data.frame(sum(diag(test))/sum(test))
  colnames(acc_test) <- i
  Test_acc <- merge(acc_test,Test_acc)
  testProbs$lable=ifelse(testProbs$obs=='P',yes=1,0)
  pred1 = prediction(testProbs$P,testProbs$lable)
  pre2 = c(pred1@predictions,pre2)
  act2 = c(pred1@labels,act2)
  perf1 = performance(pred1, measure="tpr", x.measure="fpr" )
  pred.rocr.auc.perf = performance(pred1,measure = "auc",x.measure = "cutoff")
  AUC<- as.data.frame(pred.rocr.auc.perf@y.values[[1]])
  colnames(AUC) <- i
  AUC_ROCR<-merge(AUC,AUC_ROCR)
}
AUC_ROCR
write.table(AUC_ROCR,"AUC.csv",sep=",",row.names = F)
Train_acc
Test_acc
names(Test_acc)[101] <- "ACC"
names(Train_acc)[101] <- "ACC"
ACC <- rbind(Train_acc,Test_acc)
write.table(Train_acc,"Train_acc.csv",sep=",",row.names = F)
write.table(Test_acc,"Test_acc.csv",sep=",",row.names = F)
mean(as.numeric(AUC_ROCR[1,1:100]))


library(Hmisc)
library(Cairo)
importance1 <- import %>% group_by(features) %>% 
  dplyr::summarise(Mean = mean(Overall), SD = sd(Overall))
colnames(importance1) <- c("Features","Importance_Mean","Importance_SD")
importance2 <- arrange(importance1,desc(importance1$Importance_Mean))
label <- data.frame(Features=c("PCaeC341","PCaeC404","PCaaC406","PCaaC421","fsh","Ie2","BMI","VD3","PCaeC446","Kynurenine","E1","SMC241","C2","RON","Creatinine","Asp","ON"),
                    Group = c("PC","PC","PC","PC","HO","HO","CL","VI","PC","BA","HO","SM","AC","CL","BA","AA","CL"))

im3 <- merge(importance2,label,id="Features")
im3$Features=capitalize(im3$Features)
my_color <- data.frame(Group = c("PC","HO","CL","VI","BA","SM","AC","AA"),color = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),stringsAsFactors = F)
color_temp <- my_color %>%filter(Group %in% im3$Group)
p <- ggplot(im3,aes(reorder(Features,-Importance_Mean),Importance_Mean,fill=Group))+
  scale_y_continuous(expand = c(0,0),limits = c(0,120))+
  geom_bar(stat = "identity",color= "black",size = 1)+
  scale_fill_manual(values = color_temp$color,limits = color_temp$Group)+
  geom_errorbar(aes(x=reorder(Features,Importance_Mean), ymin= Importance_Mean, ymax = Importance_Mean + Importance_SD),
                width = 0.4, size = 1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.9,0.77),
        title = element_text(size=15),
        legend.key.size = unit(2,"line"),
        legend.title = element_text(family="Times New Roman",face="bold",size = 24),
        legend.text = element_text(family="Times New Roman",face="bold",size = 18))
p
CairoPDF("FI.pdf", 8, 9)
p
dev.off()


write.table(AUC_ROCR,"All_ROC7.csv",sep=",",row.names = F)
write.table(ACC,"All_ACC.csv",sep=",",row.names = F)
write.table(importance2,"Feature_importance_range7.csv",sep=",",row.names = F)
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

table(testy)
sample=NULL
for (i in 1:100){
  sample=c(sample,rep(i,times=44))  #set test sizw
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
Opt_cutoff

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
graph2pdf(file="ROC.pdf",width=5,height=5)

