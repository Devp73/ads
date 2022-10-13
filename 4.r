library(caTools)

df = read.csv("heartproblem (1).csv")
df

anyNA(df)

df = na.omit(df)

cordata = df[,1:15]
corr <- round(cor(cordata), 1)
corr
mean(corr) #checking mean correlation to determine features are dependent on each other or not

split = sample.split(df,0.7)
training = subset(df,split==T)
testing = subset(df,split==F)
training

cordata1 = testing[,1:15]
cordata = training[,1:15]
corr <- cor(cordata)
corr1 <- cor(cordata1)
corr
corr1
mean(corr) #checking mean correlation to determine features are dependent on each other or not
mean(corr1) #checking mean correlation to determine features are dependent on each other or not

model = naiveBayes(HeartProblem~.,training)
res = predict(model,testing)
cm = table(testing$HeartProblem,res)
acc = (cm[1,1] + cm[2,2]) / (cm[1,2] + cm[2,1] + cm[1,1] + cm[2,2])
acc

#svm
model1 = svm(HeartProblem~., data=training, kernel="linear", type="C-classification")

#to find accuracy of the model
res1 = predict(model1,testing)
acc1 = mean(res1 == testing$HeartProblem)
acc1

