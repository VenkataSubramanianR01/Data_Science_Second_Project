rm(list = ls())
setwd("D:/Data Science/Second_Project")
#load the data
bike_data=read.csv("day.csv")
#######################finding missing values#########################
library("DMwR")
bike_data=knnImputation(bike_data,k=3,meth = "weighAvg") #No missing values found

numeric_data=sapply(bike_data, is.numeric)
numeric_values=bike_data[,numeric_data]
cnames=colnames(numeric_values)
#######################detecting outliers#########################
library("ggplot2")
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(bike_data))+ 
           stat_boxplot(geom = "boxplot", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "yellow" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of responded for",cnames[i])))
}
#plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn11,gn12,gn13,gn14,ncol=4)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)

plot(bike_data$registered,bike_data$cnt, pch = 16, cex = 1.3, col = "red", main = "Registered Users Plotted against COunt", xlab = "Count", ylab = "Casual Users")

##############Treating Outliers##########
Outlier_variables=c("hum","windspeed","casual")
for (i in Outlier_variables) {
  print(i)
  val=bike_data[,i][bike_data[,i] %in% boxplot.stats(bike_data[,i])$out]
  bike_data[,i][bike_data[,i] %in% val]=NA
}

bike_data=knnImputation(bike_data,k=5)
library("xlsx")
write.xlsx(bike_data,"Outliers_removed_data.xlsx")
####################Feature Selection##################################
catnames=c("season","yr","mnth","holiday","weekday","workingday","weathersit")
cnames=cnames[which(!cnames %in% catnames)]
###Converting Date to Numeric Date
bike_data$dteday=is.numeric.Date(bike_data$dteday)
######Correlation plot
library("corrgram")
qqnorm(bike_data$registered)
corrgram(bike_data[cnames],order = F,upper.panel = panel.pie,text.panel = panel.txt,
         main="Correlation plot")
#Dimension Reduction based on correlation plot
bike_data=subset(bike_data,select=-c(atemp,hum,instant))
####Feature Selection for Categorical Variables########################
for (i in catnames) {
  bike_data[,i]=as.factor(bike_data[,i])
}
factor_index=sapply(bike_data, is.factor)
factor_data=bike_data[,factor_index]
######Anova Testing
for (i in 1:length(catnames)) {
  print(names(factor_data)[i])
  aov_test=aov(cnt ~ factor_data[,i],data = bike_data)
  print(summary(aov_test))
}
####Dimension Reduction after Anova Testing
bike_data=subset(bike_data,select=-c(holiday,weekday,workingday))
#bike_data$cnt=is.numeric(bike_data$cnt)
#################Machine Learning#########################
library("randomForest")
RF_Model=randomForest(cnt ~.,data=bike_data,importance=TRUE,ntree=500)
importance(RF_Model,type = 1)
#Dimension Reduction after identifying important variables
bike_data=subset(bike_data,select=-c(dteday,windspeed))
dim(bike_data)
##### Checking for Collinearity using VIFCOR
for (i in colnames(bike_data)) {
  bike_data[,i]=as.numeric(bike_data[,i])
}
library("usdm")
vifcor(bike_data[,-8],th=0.8)
#Dimension Reduction after collinearity check
bike_data=subset(bike_data,select=-c(season))
##########Building Model###################
#sampling model
train_index=sample(1:nrow(bike_data),0.8*nrow(bike_data))
train_data=bike_data[train_index,]
test_data=bike_data[-train_index,]

################linear regression model###########################
lm_model=lm(cnt ~.,data=train_data)
summary(lm_model)
predict_model=predict(lm_model,test_data[,-7])
#names(test_data[9])
##########################decision tree###########
library("rpart")
library("MASS")
dec_train_index=sample(1:nrow(bike_data),0.8*nrow(bike_data))
dec_train_data=bike_data[dec_train_index,]
dec_test_data=bike_data[-dec_train_index,]
dec_model=rpart(cnt ~.,data = train_data,method = "anova")
summary(dec_model)
dec_test_model=predict(dec_model,dec_test_data[,-7])
library("rpart.plot")
rpart.plot(dec_model)
#############Error Metrics############
regr.eval(test_data[,7],predict_model,stats = c('mae','rmse','mape','mse'))
regr.eval(dec_test_data[,7],dec_test_model,stats = c('mae','rmse','mape','mse'))
