library(caTools)
library(openxlsx)

df = read.xlsx("purchasedata (1).xlsx")
df

#remove user.id column
df = subset(df, select = -User.ID)

cordata = df[,2:3]
corr <- round(cor(cordata), 1)
corr
mean(corr) #checking mean correlation to determine features are dependent on each other or not

split = sample.split(df,0.7)
training = subset(df,split==T)
testing = subset(df,split==F)
training

cordata1 = testing[,1:3]
cordata = training[,1:3]
corr <- cor(cordata)
corr1 <- cor(cordata1)
corr
corr1
mean(corr) #checking mean correlation to determine features are dependent on each other or not
mean(corr1) #checking mean correlation to determine features are dependent on each other or not

model = naiveBayes(Purchased~.,training)
res = predict(model,testing)
cm = table(testing$Purchased,res)
acc = (cm[1,1] + cm[2,2]) / (cm[1,2] + cm[2,1] + cm[1,1] + cm[2,2])
acc

#svm
model1 = svm(Purchased~., data=training, kernel="linear", type="C-classification")

#to find accuracy of the model
res1 = predict(model1,testing)
acc1 = mean(res1 == testing$Purchased)
acc1
