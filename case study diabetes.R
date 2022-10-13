df = read.csv(file.choose(),header=T)
df

anyNA(df)

#cleaning data
df$BloodPressure[df$BloodPressure == "0"] = NA
df$BloodPressure[df$BloodPressure == 0] = NA
df$BloodPressure = ifelse(is.na(df$BloodPressure),round(mean(df$BloodPressure,na.rm=T)),df$BloodPressure)

df[,4][df[,4] == 0] = NA
df[,4][df[,4] == "0"] = NA
df[,4] = ifelse(is.na(df[,4]),round(mean(df[,4],na.rm=T)),df[,4])

df[,5][df[,5] == 0] = NA
df[,5][df[,5] == "0"] = NA
df[,5] = ifelse(is.na(df[,5]),round(mean(df[,5],na.rm=T)),df[,5])

df[,6][df[,6] == 0] = NA
df[,6][df[,6] == "0"] = NA
df[,6] = ifelse(is.na(df[,6]),round(mean(df[,6],na.rm=T)),round(df[,6]))


#finding correlation
cordata = df[,1:8]
corr <- round(cor(cordata), 1)
corr
mean(corr) #checking mean correlation to determine features are dependent on each other or not

#training testing
library(caTools)
split = sample.split(df,0.7)
training = subset(df,split==T)
testing = subset(df,split==F)
training

#checking relationship b/w features for training and testing
cordata1 = testing[,1:8]
cordata = training[,1:8]
corr <- cor(cordata)
corr1 <- cor(cordata1)
corr
corr1
mean(corr) #checking mean correlation to determine features are dependent on each other or not
mean(corr1) #checking mean correlation to determine features are dependent on each other or not

#model
library(e1071)
model = naiveBayes(Outcome~.,training)
res = predict(model,testing)
cm = table(testing$Outcome,res)
acc = (cm[1,1] + cm[2,2]) / (cm[1,2] + cm[2,1] + cm[1,1] + cm[2,2])

#svm
model1 = svm(Outcome~., data=training, kernel="linear", type="C-classification")

#to find accuracy of the model
res1 = predict(model1,testing)
acc = mean(res1 == testing$Outcome)
acc
cm