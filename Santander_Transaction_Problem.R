#Clear R environment
rm(list=ls(all=T))

#Set Current Working Directory
setwd("C:/Users/HP/Desktop/Edwisor/Project 2")


#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#Install Packages
install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

#Read the data
train_org = read.csv("train.csv", header = T, na.strings = c(" ","", "NA"))
test_org = read.csv("test.csv", header = T, na.strings = c(" ","","NA"))

#####-------------------------EXPLORATORY DATA ANALYSIS-------------------------------#####
#Summary of Data Sets
str(train_org)
str(test_org)

#Dimensions of DataSets
dim(train_org)
dim(test_org)

#convert to factor
train_org$target<-as.factor(train_org$target)
class(train_org$target)

#####-------------------------------MISSING VALUE ANALYSIS----------------------------#####
#Finding the missing values in train data
missing_val_train<-data.frame(missing_val_train=apply(train_org,2,function(x){sum(is.na(x))}))
missing_val_train<-sum(missing_val_train)
missing_val_train

#Finding the missing values in test data
missing_val_test<-data.frame(missing_val_test=apply(test_org,2,function(x){sum(is.na(x))}))
missing_val_test<-sum(missing_val_test)
missing_val_test

#####----------------------------------OUTLIER ANALYSIS-------------------------------#####
#BoxPlots - Distribution and Outlier Check
cnames = c("var_2","var_12","var_50","var_85","var_112","var_199")
for (i in 1:6){
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "target"), data = subset(train_org))+ 
                        stat_boxplot(geom = "errorbar", width = 0.5) +
                        geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,outlier.size=1, notch=FALSE) +
                        theme(legend.position="bottom")+
                        labs(y=cnames[i],x="Target")+
                        ggtitle(paste("Box plot of Target for",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,ncol=3)
rm(list = "i")

#Remove outliers using boxplot method from TRAIN dataset
for(i in 3:202){
   print(i)
   val = train_org[,i][train_org[,i] %in% boxplot.stats(train_org[,i])$out]
   print(length(val))
   train_org = train_org[which(!train_org[,i] %in% val),]
}

#Remove outliers using boxplot method from TEST dataset
for(i in 2:201){
  print(i)
  val = test_org[,i][test_org[,i] %in% boxplot.stats(test_org[,i])$out]
  print(length(val))
  test_org = test_org[which(!test_org[,i] %in% val),]
}

#Replace all outliers in TRAIN Dataset with NA
for(i in 3:202){
  val = train_org[,i][train_org[,i] %in% boxplot.stats(train_org[,i])$out]
  print(length(val))
  train_org[,i][train_org[,i] %in% val] = NA
}

#Replace all outliers in TEST Dataset with NA
for(i in 2:201){
  val = test_org[,i][test_org[,i] %in% boxplot.stats(test_org[,i])$out]
  print(length(val))
  test_org[,i][test_org[,i] %in% val] = NA
}

#Impute NA with mean
for(i in 3:202){
  train_org[is.na(train_org[,i]),i] = mean(train_org[,i], na.rm = T)
}

for(i in 2:201){
  test_org[is.na(test_org[,i]),i] = mean(test_org[,i], na.rm = T)
}

################################Distribution of Target Variable#########################################
table(train_org$target)

#Percentage counts of target classes
table(train_org$target)/length(train_org$target)*100

#Bar plot for count of target classes
plot1<-ggplot(train_org,aes(target))+theme_bw()+geom_bar(stat='count',fill='lightblue')
grid.arrange(plot1, ncol=1)

########################################Feature Selection################################################
## Correlation Plot 
corrgram(train_org[,45:50], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Correlations in train data
#convert factor to int
train_org$target<-as.numeric(train_org$target)
train_correlations<-cor(train_org[,c(2:202)])
train_correlations

#Correlations in test data
test_correlations<-cor(test_org[,c(2:201)])
test_correlations

rm(list = "i","val")

#Distribution of Train attributes
train_org$target<-as.factor(train_org$target)
for (var in names(train_org)[c(3:202)]){
  target<-train_org$target
  plot<-ggplot(train_org, aes(x=train_org[[var]], fill=target)) +
    geom_density(kernel='gaussian') + ggtitle(var)+theme_classic()
  print(plot)
}

#Distribution of Test attributes
for (var in names(test_org)[c(195:201)]){
  plot<-ggplot(test_org, aes(x=test_org[[var]])) +
    geom_density(kernel='gaussian') + ggtitle(var)+theme_classic()
  print(plot)
}

rm(list = "i")

#Applying the function to find skewness values per row in train and test data.
train_skew<-apply(train_org[,-c(1,2)],MARGIN=1,FUN=skewness)
test_skew<-apply(test_org[,-c(1)],MARGIN=1,FUN=skewness)

#Distribution of skewness values per row in train and test data
ggplot()+geom_density(aes(x=train_skew),kernel='gaussian',show.legend=TRUE,color='green')+theme_classic()+
geom_density(aes(x=test_skew),kernel='gaussian',show.legend=TRUE,color='blue')+
labs(x='skewness values per row',title="Distribution of skewness values per row in train and test dataset")

#Applying the function to find skewness values per column in train and test data.
train_skew<-apply(train_org[,-c(1,2)],MARGIN=2,FUN=skewness)
test_skew<-apply(test_org[,-c(1)],MARGIN=2,FUN=skewness)

#Distribution of skewness values per column in train and test data
ggplot()+geom_density(aes(x=train_skew),kernel='gaussian',show.legend=TRUE,color='green')+theme_classic()+
geom_density(aes(x=test_skew),kernel='gaussian',show.legend=TRUE,color='blue')+
labs(x='skewness values per column',title="Distribution of skewness values per column in train and test dataset")

rm(list="test_skew","train_skew")

##############------------------------------Variable Importance--------------------------------##########
#Split the training data using simple random sampling
train_index<-sample(1:nrow(train_org),0.75*nrow(train_org))
#train data
train_data<-train_org[train_index,]
#validation data
valid_data<-train_org[-train_index,]
#dimension of train and validation data
dim(train_data)
dim(valid_data)

#Training the Random forest classifier to identify Important Variables
set.seed(2732)
train_data$target<-as.factor(train_data$target)

#Setting the mtry
mtry<-floor(sqrt(200))

#Setting the tunegrid
tuneGrid<-expand.grid(.mtry=mtry)

#Fitting the Random Forest
rf<-randomForest(target~.,train_data[,-c(1)],mtry=mtry,ntree=10,importance=TRUE)

#Variable importance
VarImp<-importance(rf,type=2)
VarImp

data = cbind(rownames(VarImp), VarImp)
rownames(data) = NULL
colnames(data)[1] = "Var"

#Considering variables having High Imp in Prediction
data = data[VarImp>mean(VarImp),]
train_org_2 = train_org[,c("target","var_0","var_1","var_2","var_5","var_6","var_9","var_12","var_13","var_18","var_21","var_22","var_24","var_26","var_33","var_34","var_40","var_44","var_49","var_51","var_53","var_56","var_75","var_76","var_78","var_80","var_81","var_83","var_86","var_91","var_92","var_93","var_94","var_95","var_99","var_106","var_108","var_109","var_110","var_115","var_119","var_121","var_122","var_123","var_133","var_139","var_141","var_145","var_146","var_147","var_148","var_150","var_154","var_155","var_162","var_164","var_165","var_166","var_169","var_170","var_172","var_174","var_177","var_179","var_180","var_184","var_188","var_190","var_191","var_194","var_197","var_198")]

rm(list = "i")
write.csv(train_org_2, "train_org_final.csv", row.names = F)

#Standardisation
for(i in 2:72){
   print(i)
   train_org_2[,i] = (train_org_2[,i] - mean(train_org_2[,i]))/sd(train_org_2[,i])
}

#####-------------------------MODEL DEVELOPMENT------------------------------------#########
#Divide data into train and test using stratified sampling method
set.seed(2375)
train.index = createDataPartition(train_org_2$target, p = .75, list = FALSE)
train = train_org_2[ train.index,]
train_test  = train_org_2[-train.index,]

##Decision tree for classification
#Develop Model on training data
train$target = as.factor(train$target)
C50_model = C5.0(target ~., train, trials = 50, rules = TRUE)

#Summary of DT model
summary(C50_model)

#write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")

#Lets predict for test cases
C50_Predictions = predict(C50_model, train_test, type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(train_test$target, C50_Predictions)
confusionMatrix(ConfMatrix_C50)

#False Negative rate
FNR = FN/FN+TP 

#Recall
RC = TP/TP+FN

#Precision
P = (TN*100)/TN+FP
#Accuracy: 83.472%
#FNR: 81.344%
#RC: 18.655%
#Precision : 90.649%

###Random Forest
RF_model = randomForest(target ~ ., train, importance = TRUE, ntree = 500)

#Presdict test data using random forest model
RF_Predictions = predict(RF_model, train_test)

##Evaluate the performance of classification model
ConfMatrix_RF = table(test$responded, RF_Predictions)
confusionMatrix(ConfMatrix_RF)

#False Negative rate
FNR = FN/FN+TP 
#Recall
RC = TP/TP+FN
#Precision
P = (TN*100)/TN+FP

#Accuracy: 90.086%
#FNR: 99.297%
#RC: 0.70%
#Precision : 99.984%

#Logistic Regression
logit_model = glm(target ~ ., data = train, family = "binomial")

#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = train_test, type = "response")

#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)

##Evaluate the performance of classification model
ConfMatrix_RF = table(train_test$target, logit_Predictions)
confusionMatrix(ConfMatrix_RF)
#False Negative rate
FNR = FN/FN+TP 
#Recall
RC = TP/TP+FN
#Precision
P = (TN*100)/TN+FP

#Accuracy: 91.011%
#FNR: 73.996%
#RC: 26.003%
#Precision : 98.543%
