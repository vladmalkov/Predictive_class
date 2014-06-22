dat1<-read.csv("pml-training.csv")

## Preserving flag for potential outliers
dat1$flag<-ifelse(dat1$kurtosis_roll_belt !="",1,0)
## Deleting variables with mostly missing values  
dat1<-dat1[,sapply(dat1, function(x) sum(x=="" | is.na(x)==1))/nrow(dat1)<0.8]

library(caret)
library(ggplot2)
## 70% to training set, 30% to testing
inTrain<-createDataPartition(y=dat1$classe, p=0.7,list=F)
train<-dat1[inTrain,]
test<-dat1[-inTrain,]

## Filtering variables based on ANOVA F-value 
Fval<-function(n){
  a<-lm(train[,n]~train$classe )
  x<-unlist(summary.aov(a)[[1]]["F value"])[1]
  names(x)<-names(dat1)[n]
  return(x)
}
Fval_var<-NA*numeric(60)
for (i in 7:59) Fval_var[i]<-Fval(i)
par(mfrow=c(1,1))
hist(Fval_var,breaks=c(0,30,50,70,100,150,200,300,400,500,800))
# Based on histogram, choose Fval>70 as cut-off

## Heatmap of correlation of variables where Fval>70
value<-1-abs(cor(train[,!is.na(Fval_var) & Fval_var>70 ]))
rownames(value)<-paste("F=",signif(Fval_var[!is.na(Fval_var) & Fval_var>70],2), names(train)[!is.na(Fval_var) & Fval_var>70])
colnames(value)<-names(train)[!is.na(Fval_var) & Fval_var>70]
heatmap(value,col = heat.colors(20),scale=c("none"))

## Predicting variables names to be used below
retain_var2=names(train)[!is.na(Fval_var) & Fval_var>70]

## Keeping only classe variable and predictive variables in the datasets
train<-train[,c(retain_var2,"classe")]
test<-test[,c(retain_var2,"classe")]

## Random Forest building prediction
modFit<-train(classe~.,method="rf",data=train,prox=T)
## Tabulation of prediction for the (30%) testing set
table(test$classe,predict(modFit,newdata=test))
(accuracy<-100*sum(test$classe==predict(modFit,newdata=test))/length(test$classe))
(errorrate<-100-accuracy)

## 20 new observations
val<-read.csv("pml-testing.csv")
val<-val[,retain_var2]
answers <-predict(modFit,newdata=val)

## Saving answers as files
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
pml_write_files(answers)