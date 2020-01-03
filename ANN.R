install.packages('neuralnet')
library(neuralnet)
# Importing Data
setwd('C:/Users/carlo/Desktop/Data science/ASDM/ANN')
creditrisk_data<-read.table("creditrisk.csv", sep = ",", header= TRUE)
# Inspecting dataset
names(creditrisk_data)
head(creditrisk_data)
tail(creditrisk_data)
summary(creditrisk_data)
str(creditrisk_data)
nrow(creditrisk_data)
ncol(creditrisk_data)
dim(creditrisk_data)

# Split dataset in 2 parts 60% for training and 40% for test.
set.seed(1234)
pd<-sample(2,nrow(creditrisk_data),replace=TRUE,prob=c(0.6,0.4))
trainingdata <-creditrisk_data[pd==1,]
testdata<-creditrisk_data[pd==2,]
dim(trainingdata) 
dim(testdata) 
#Build ANN 
creditnet <-neuralnet(default10yr ~ LTIR+age, trainingdata, hidden=3, lifesign="minimal", linear.output=FALSE, threshold=0.1)
#Ploting ANN
plot(creditnet, rep = "best")
temp_test <-subset(testdata, select = c("LTIR", "age"))
head(temp_test)
#Analizing results 
creditnet_results <-compute(creditnet, temp_test) 
names(creditnet_results)
str(creditnet_results)
results <-data.frame(actual = testdata$default10yr, prediction = creditnet_results$net.result)
head(results)
tail(results)
results[90:105, ]
results$prediction <-sapply( creditnet_results$net.result, round,digits=0)
results[90:105, ] 
#Confusing Matrix 
confusionmatrix<-table(testdata$default10yr,results$prediction) 
print(confusionmatrix)
#Classification accuracy
sum(diag(confusionmatrix))/sum(confusionmatrix) 
#Classification error
1-sum(diag(confusionmatrix))/sum(confusionmatrix)