library(caTools)
library(openxlsx)

df = read.xlsx("Placement Details (1).xlsx")
df

#remove student id column
df = subset(df, select = -Student.Id)

#making placement as categorical
df$Placement.Status = as.factor(df$Placement.Status)

#changing column name of 12 percentage
colnames(df)[2] <- "12 Percentage"


#computing value of NA mean
df$`10.Percentage`[is.na(df$`10.Percentage`)] = mean(df$`10.Percentage`,na.rm = T)
df$`12 Percentage`[is.na(df$`12 Percentage`)] = mean(df$`12 Percentage`,na.rm = T)
df$Graduate.Percentage[is.na(df$Graduate.Percentage)] = mean(df$Graduate.Percentage,na.rm = T)



#splitting into training and testing
split = sample.split(df,0.7)
training = subset(df,split==T)
testing = subset(df,split==F)
training


#naive bayes
model = naiveBayes(Placement.Status~.,training)
res = predict(model,testing)
cm = table(testing$Placement.Status,res)
acc = (cm[1,1] + cm[2,2]) / (cm[1,2] + cm[2,1] + cm[1,1] + cm[2,2])
acc

#svm
model1 = svm(Placement.Status~., data=training, kernel="linear", type="C-classification")

#to find accuracy of the model
res1 = predict(model1,testing)
acc1 = mean(res1 == testing$Placement.Status)
acc1
