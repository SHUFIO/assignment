#Research question1: whether customers will subscribe term deposit?
#set working direction
#load data
data.frame<-read.csv("002.csv", header=T)
#Check missing values
any(is.na(data.frame))
#showing no missing values
#Target leakage solution
data.frame[,9:12]<-list(NULL)
#Data Pre-pocessing:transform certain variables into categorical variables
data.frame$job<-as.factor(data.frame$job)
data.frame$marital<-as.factor(data.frame$marital)
data.frame$education<-as.factor(data.frame$education)
data.frame$default<-as.factor(data.frame$default)
data.frame$housing<-as.factor(data.frame$housing)
data.frame$loan<-as.factor(data.frame$loan)
data.frame$poutcome<-as.factor(data.frame$poutcome)
data.frame$y<-ifelse(data.frame$y=="yes",1,0)

#Split dataset into 80% trainset and 20% testset
library(caret)
inTrain<-createDataPartition(data.frame$y, p=0.8, list=F)
train_set<-data.frame[inTrain,]
test_set<-data.frame[-inTrain,]

#Build logistic model with train_set
mod1<-glm(y~.,data=train_set,family="binomial")
summary(mod1)
#As result, the p-value of age 0.85 greater than 0.05, showing no significance

#Evaluate the goodness of fit with trainset
library(rcompanion)
nagelkerke(mod1)
#nagelkerke compares model.none and model.full
#As result, pseudo r2的值越近1表示模型拟合得越好
library(ResourceSelection)
hoslem.test(train_set$y, fitted(mod1))
#As result, p>0.05表示模型拟合得好     

#Stepwise modeling to pick proper input variables
mod.none <- glm (y~1, data = train_set, family = "binomial")
mod.full<-glm(y~.,data=train_set, family = "binomial")
step(mod.none, scope=formula(mod.full),direction="both", trace=F)
#As result,y ~ poutcome + housing + job + campaign + loan + marital + education + balance + previous

#the p-value of jobblue-collar 0.000248 smaller than 0.05, showing significance
exp(mod1$coefficients[3])
#If customer is blue-collar, the probability of subscribing term deposit decrease 33%
#(1-0.77)*100%
#the p-value of entrepreneur shows significance.
exp(mod1$coefficients[4])
#if customer is entrepreneur, the probability of subscribing term deposit decrease 31%.
#the p-value of housemaid show significance.
exp(mod1$coefficients[5])
#if customer is housemaid,the probability decrease 32%.
exp(mod1$coefficients[7])
#if customer is retired, the probability of subscribing term deposit increase 63%.
exp(mod1$coefficients[9])
#if customer is servcies, the probability decrease 21%.
exp(mod1$coefficients[10])
#if customer is student, the probability increase 59%
#the p-value of married 0.001267 smaller than 0.05, showing significance.
exp(mod1$coefficients[14])
#If customer is married, the probability of subscribing term deposit decrease 17%
exp(mod1$coefficients[15])
#If customer is single, the probability increase 14.9%
exp(mod1$coefficients[16])
#If customer is secondary education, the probability increase 13.8%.
exp(mod1$coefficients[17])
#If customer is tertiary eductaion, the probability increase 42.2%.
exp(mod1$coefficients[22])
#If customer hold loan,the probability decrease 41%
#Predict with testset
library(stats)
library(pROC)
test.probs=predict(mod1,test_set,type="response")
#Set threshold
roc_curve<-roc(test_set$y,test.probs)
plot(roc_curve, main="ROC Curve", col="#39BEB1",lwd=2)
abline(h=0,v=1,col="#ACA4E2",lty=2,lwd=2)
#x-axis:specificity(true negative rates), and y-axis:sensitivity(true positive rate)
#In this case, we want to prioritize true negative rate to identify more unsubscribed customers
#In conclusion, the cut-off point should be 0.5
test.pred=rep('NO',9042)
test.pred[test.probs>0.5]="YES"

#Evaluate the performance of trained model on new,unseen data
#Method1:confusion matrix
conf_matrix<-table(test.pred, test_set$y)
print(conf_matrix)
#Method2:Accuracy
accuracy<-sum(diag(conf_matrix))/sum(conf_matrix)
print(accuracy)
#Method3:Precision(Positive Predictive Value)
precision<-conf_matrix[2,2]/sum(conf_matrix[,2])
print(precision)
#Method4:Recall(Sensitivity,True Positive Rate)
recall<-conf_matrix[2,2]/sum(conf_matrix[2,])
print(recall)
#Method4:F1Score
f1_score<-2*precision*recall/(precision+recall)
print(f1_score)



       


#Research question2: predict the customer's balance
#一般用数字预测数字
#age预测balance
data.frame<-read.csv("002.csv", header=T)
data.frame[,9:16]<-list(NULL)
#Data Pre-pocessing:transform certain variables into categorical variables
data.frame$job<-as.factor(data.frame$job)
data.frame$marital<-as.factor(data.frame$marital)
data.frame$education<-as.factor(data.frame$education)
data.frame$default<-as.factor(data.frame$default)
data.frame$housing<-as.factor(data.frame$housing)
data.frame$loan<-as.factor(data.frame$loan)
data.frame$y<-as.factor(data.frame$y)

#Full linear model
fit<-lm(balance~.,data=data.frame)
summary(fit)

#Split data
library(caret)
inTrain<-createDataPartition(data.frame$balance, p=0.8, list=F)
train_set2<-data.frame[inTrain,]
test_set2<-data.frame[-inTrain,]

#Create the full model on the trainset
fit<-lm(balance~.,data=train_set2)

#Reduce the model to the most important predicators
fitStep<-step(fit,k=log(nrow(train_set2)))

#Get Bayes Factors using Wagenmaker(2007) formula
exp(BIC(fit)-BIC(fitStep)/2)

#Evaluate how well the model is doing on new data
test.probs2<-predict.lm(fitStep,test_set2)

#Get R squared
cor(test.probs2,test_set2$balance)^2

#Draw the relationship between y hat and y
plot(test_set2$balance~test.probs2)
abline(lm(test_set2$balance~test.probs2),col="#E495A5")

