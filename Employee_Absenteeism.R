#Clear R environment
rm(list=ls(all=T))

#Set Current Working Directory
setwd("C:/Users/HP/Desktop/Edwisor/Project 1")

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#Install Packages
install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

#Read Data
Absenteeism_Data = read.csv("Absenteeism_at_work_Project.csv", header = T, na.strings = c(" ", "", "NA"))
Data_Copy = Absenteeism_Data
###-----------------------------------------DATA EXPLORATION-------------------------------------------###
#Check Structure of data
str(Absenteeism_Data)

#Renaming Column Variables
names(Absenteeism_Data)[1] = "Emp_Id"
names(Absenteeism_Data)[2] = "Abs_Reason"
names(Absenteeism_Data)[3] = "Abs_Month"
names(Absenteeism_Data)[4] = "Abs_Day"
names(Absenteeism_Data)[5] = "Abs_Season"
names(Absenteeism_Data)[6] = "Transport_Expense"
names(Absenteeism_Data)[7] = "Office_Distance"
names(Absenteeism_Data)[8] = "Service_Time"
names(Absenteeism_Data)[9] = "Emp_Age"
names(Absenteeism_Data)[10] = "Avg_Workload"
names(Absenteeism_Data)[11] = "Hit_Target"
names(Absenteeism_Data)[12] = "Disciplinary_Failure"
names(Absenteeism_Data)[14] = "Num_of_Kids"
names(Absenteeism_Data)[15] = "Drinker"
names(Absenteeism_Data)[16] = "Smoker"
names(Absenteeism_Data)[17] = "Num_of_Pets"
names(Absenteeism_Data)[20] = "Emp_BMI"
names(Absenteeism_Data)[21] = "Abs_Hrs"

#Converting data types as required
Absenteeism_Data$Emp_Id = as.factor(as.character(Absenteeism_Data$Emp_Id))
Absenteeism_Data$Abs_Reason[Absenteeism_Data$Abs_Reason %in% 0] = 20
Absenteeism_Data$Abs_Reason <- as.factor(as.character(Absenteeism_Data$Abs_Reason))
Absenteeism_Data$Abs_Month[Absenteeism_Data$Abs_Month %in% 0] = NA
Absenteeism_Data$Abs_Month <- as.factor(as.character(Absenteeism_Data$Abs_Month))
Absenteeism_Data$Abs_Day <- as.factor(as.character(Absenteeism_Data$Abs_Day))
Absenteeism_Data$Abs_Season <- as.factor(as.character(Absenteeism_Data$Abs_Season))
Absenteeism_Data$Disciplinary_Failure <- as.factor(as.character(Absenteeism_Data$Disciplinary_Failure))
Absenteeism_Data$Education <- as.factor(as.character(Absenteeism_Data$Education))
Absenteeism_Data$Num_of_Kids <- as.factor(as.character(Absenteeism_Data$Num_of_Kids))
Absenteeism_Data$Drinker <- as.factor(as.character(Absenteeism_Data$Drinker))
Absenteeism_Data$Smoker <- as.factor(as.character(Absenteeism_Data$Smoker))
Absenteeism_Data$Num_of_Pets <- as.factor(as.character(Absenteeism_Data$Num_of_Pets))

###-------------------------------------MISSING VALUE ANALYSIS-----------------------------------------###
#Creating New DataSet with Missing Values Info
Missing_Data = data.frame(apply(Absenteeism_Data,2,function(x){sum(is.na(x))}))
Missing_Data$Columns = row.names(Missing_Data)

#Creating New Variable in Missing_Val DataSet
names(Missing_Data)[1] = "Missing_Percentage"
Missing_Data$Missing_Percentage = (Missing_Data$Missing_Percentage/nrow(Absenteeism_Data)) * 100

#Sorting in Descending Order
Missing_Data = Missing_Data[order(-Missing_Data$Missing_Percentage),]
row.names(Missing_Data) = NULL
Missing_Data = Missing_Data[,c(2,1)]
write.csv(Missing_Data, "Missing_Value_Analysis1.csv", row.names = F)

#Plotting a Bar-Graph for Missing Value Analysis
ggplot(data = Missing_Data[1:21,], aes(x=reorder(Columns, -Missing_Percentage),y = Missing_Percentage))+geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+ggtitle("Missing Data Percentage Analysis") + theme_bw()
  
# KNN Imputation
Absenteeism_Data = knnImputation(Absenteeism_Data, k = 3)

#Checking for any Missing Value in the data-set
sum(is.na(Absenteeism_Data))

#Data with no missing values
write.csv(Absenteeism_Data, 'DataFile1_PostKNN.csv', row.names = F)

###-----------------------------------------OUTLIER ANALYSIS-------------------------------------------###
#Identifying Continuous Variables
Cont = sapply(Absenteeism_Data,is.numeric)
Cont_Var = Absenteeism_Data[,Cont]

#Identifying Categorical Variables
Cat = sapply(Absenteeism_Data,is.factor)
Cat_Var = Absenteeism_Data[,Cat]

#Distribution of Continuos Variables using Box-Plots
for(i in 1:ncol(Cont_Var)) {
  assign(paste0("box",i), ggplot(data = Absenteeism_Data, aes_string(y = Cont_Var[,i])) +
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour = "red", fill = "grey", outlier.size = 1) +
           labs(y = colnames(Cont_Var[i])) +
           ggtitle(paste("Boxplot for : ",colnames(Cont_Var[i]))))
}

#Drawing Box-Plots for each Continuos Variable
gridExtra::grid.arrange(box1,box2,box3,box4,box5,box6,box7,box8,box9,box10,ncol=5)

#Remove outliers using boxplot method
Copy2 = Absenteeism_Data
Absenteeism_Data = Copy2
for(i in 1:10){
  print(i)
  val = Absenteeism_Data[,i][Absenteeism_Data[,i] %in% boxplot.stats(Absenteeism_Data[,i])$out]
  print(length(val))
  Absenteeism_Data = Absenteeism_Data[which(!Absenteeism_Data[,i] %in% val),]
}

#Replace all outliers with NA and impute
for(i in 1:10){
val = Absenteeism_Data[,i][Absenteeism_Data[,i] %in% boxplot.stats(Absenteeism_Data[,i])$out]
print(length(val))
Absenteeism_Data[,i][Absenteeism_Data[,i] %in% val] = NA
}

#KNN Imputation
Absenteeism_Data = knnImputation(Absenteeism_Data, k = 3)
sum(is.na(Absenteeism_Data))

###----------------------------------------Feature Selection----------------------------------------------###
## Correlation Plot 
corrgram(Cont_Var, order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Exploring Relationship Between Independent Continous Variables and Dependent Variable('Absenteeism Hours') using scatter plot
Age <- aggregate(Absenteeism_Data$Abs_Hrs, by=list(Age=Absenteeism_Data$Emp_Age), FUN=sum)
plot(Age$Age, Age$x, main="Scatterplot Emp_Age Vs Absent_Hours", 
     xlab="Emp_Age ", ylab="Absent Hours", pch=19)

Expense <- aggregate(Absenteeism_Data$Abs_Hrs, by=list(Expense=Absenteeism_Data$Transport_Expense), FUN=sum)
plot(Expense$Expense, Expense$x, main="Scatterplot Transport_Expense Vs Absent_Hours", 
     xlab="Transport_Expense ", ylab="Absent Hours", pch=19)

ServiceT <- aggregate(Absenteeism_Data$Abs_Hrs, by=list(SerT=Absenteeism_Data$Service_Time), FUN=sum)
plot(ServiceT$SerT, ServiceT$x, main="Scatterplot Service_Time Vs Absent_Hours", 
     xlab="Service_Time ", ylab="Absent Hours", pch=19)

Distance <- aggregate(Absenteeism_Data$Abs_Hrs, by=list(SerT=Absenteeism_Data$Office_Distance), FUN=sum)
plot(ServiceT$SerT, ServiceT$x, main="Scatterplot Office_Distance Vs Absent_Hours", 
     xlab="Office_Distance ", ylab="Absent Hours", pch=19)

#Checking the Distribution of Dependent Variable('Absenteeism Hours') using Histogram with Normal Curve
x <- (Absenteeism_Data$Abs_Hrs)/1000
h<-hist(x, breaks=10, col="blue", xlab="Absent Hours", 
        main="Distribution of Absenteeism Hours") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="red", lwd=2)

#Exploring Distribution of Categorical variables with Dependent Variable('Absenteeism Hours')
for (i in 1:11){
  print(paste("Pie Distribution for", colnames(Cat_Var[i])))
  pie(table(Cat_Var[i]),main = paste("Pie Distribution for", colnames(Cat_Var[i])))
}

#checking the top reasons for absence as per the total numbers of absence
Reason <- aggregate(Absenteeism_Data$Abs_Hrs, by=list(Re=Absenteeism_Data$Abs_Reason), FUN=sum)
ggplot(data = Reason, aes(x= Reason$Re,y = Reason$x))+geom_bar(stat = "identity",fill = "lightblue")+xlab("Parameter")+ggtitle("Abs_Reason Analysis") + theme_bw()

#Analyzing absence dependency of no of kids
Kids <- aggregate(Absenteeism_Data$Abs_Hrs, by=list(Num_kids=Absenteeism_Data$Num_of_Kids), FUN=sum)
ggplot(data = Kids, aes(x= Kids$Num_kids,y = Kids$x))+geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+ggtitle("Num_OF Kids Analysis") + theme_bw()

#Analyzing absence dependency of month of year
Total_Hrs <- aggregate(Absenteeism_Data$Abs_Hrs, by=list(tot=Absenteeism_Data$Abs_Month), FUN=sum)
ggplot(data = Total_Hrs, aes(x= Total_Hrs$tot,y = Total_Hrs$x))+geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+ggtitle("Total Absent Hours Analysis Month-Wise") + theme_bw()
 
#Dimension Reduction for Second part of problem
Absenteeism_Data_1 = subset(Absenteeism_Data, 
                         select = -c(Abs_Reason,Abs_Day,Abs_Season,Hit_Target,Transport_Expense,Disciplinary_Failure,Education,Smoker,Num_of_Pets,Weight,Height,Emp_BMI))
###----------------------------PART 1 of PRoblem Ends Here------------------------------------------#####

###_________________________________________Part 2 Begins______________________________________________###

###--------------------------------FEATURE SCALING---------------------------------------------------###

#Identifying Continuous Variables
Cont1 = saapply(Absenteeism_Data_1,is.numeric)
Cont_Var1 = Absenteeism_Data_1[,Cont1]

#Removing Target Variable
num = names(Cont_Var1)p
num = num[-5]

#Identifying Categorical Variables
Cat1 = sapply(Absenteeism_Data_1,is.factor)
Cat_Var1 = Absenteeism_Data_1[,Cat1]

# #Standardisation
for(i in num){
  print(i)
  Absenteeism_Data_1[,i] = (Absenteeism_Data_1[,i] - mean(Absenteeism_Data_1[,i]))/
    sd(Absenteeism_Data_1[,i]) 
}
###-----------------------------------------MODEL DEVELOPMENT------------------------------------------###
#Generating Training and Test Data Set
set.seed(1)
train_index = sample(1:nrow(Absenteeism_Data_1), 0.8 * nrow(Absenteeism_Data_1))
train = Absenteeism_Data_1[train_index,]
test = Absenteeism_Data_1[-train_index,]

################Random Forest
#Train model using Training Data
RF_model = randomForest(Abs_Hrs ~ ., train, importance = TRUE, ntree = 100)

#Extract rules fromn random forest
#transform rf object to an inTrees' format
treeList = RF2List(RF_model)  

#Extract rules
exec = extractRules(treeList, train[,-9])  # R-executable conditions
 
# #Visualize some rules
exec[1:2,]

# #Make rules more readable:
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]  

# #Get rule metrics
ruleMetric = getRuleMetric(exec, train[,-9], train$Abs_Hrs)  # get rule metrics

# #evaulate few rules
ruleMetric[1:2,]

#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test[,-10])

#Create dataframe for actual and predicted values
rf_pred = data.frame("actual"=test[,-10], "rf_pred"=RF_Predictions)
head(rf_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = RF_Predictions, obs = test[,9]))

abs_pred2010 = Absenteeism_Data_1[,-c(1,8)]
abs_pred2010$Predicted <- (rf_pred$rf_pred)

Abs_Predict_2010 <- aggregate(abs_pred2010$Predicted , by=list(Re=abs_pred2010$Abs_Month), FUN=sum)

###---------------------------------------------Prediction--------------------------------------------###
#Sort Data by Absence Month and View Predicted Data
Abs_Predict <- aggregate(rf_pred$rf_pred , by=list(Re=rf_pred$actual.Abs_Month), FUN=sum)

#For 2011 Data
emp_2011 = Absenteeism_Data_1
emp_2011$Service_Time = Absenteeism_Data_1$Service_Time + 1
emp_2011$Emp_Age = Absenteeism_Data_1$Emp_Age + 1

#Exclude Emp_Id and Abs_hrs
emp_2011= emp_2011[,-c(1,8)]

# #Standardisation
for(i in num){
  print(i)
  emp_2011[,i] = (emp_2011[,i] - mean(emp_2011[,i]))/
    sd(emp_2011[,i]) 
}

predict_2011 = randomForest(Abs_Hrs ~ ., emp_2011, ntree = 500)
rf_predictions1 = predict(predict_2011, emp_2011)
rf_pred1 = data.frame("actual"=emp_2011, "rf_pred"=rf_predictions1)

abs_pred2011 = emp_2011
abs_pred2011$Predicted <- (rf_pred1$rf_pred)

Abs_Predict_2011 <- aggregate(abs_pred2011$Predicted , by=list(Re=abs_pred2011$Abs_Month), FUN=sum)

tot_Monthly_hours = 22*8*36

Abs_Predict_2011$monthly_loss_percentage = (Abs_Predict_2011$x/tot_Monthly_hours) * 100
