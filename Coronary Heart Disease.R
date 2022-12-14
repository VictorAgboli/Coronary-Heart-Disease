#Linear Stats Project

#import the data
setwd("/Users/victoragboli/Documents/Fall 22 Semester/Linear Statistical Analysis/Project")
df = read.csv("Heart Disease.csv")

#coding
df$Gender = as.factor(df$Gender)
df$Diabetes = as.factor(df$Diabetes)
df$TenYearCHD = as.factor(df$TenYearCHD)
df$Prevalent.Stroke = as.factor(df$Prevalent.Stroke)
df$Prevalent.Hypertension = as.factor(df$Prevalent.Hypertension)
df$Age = as.factor(df$Age)
df$CurrentSmoker = as.factor(df$CurrentSmoker)


#shows the imbalance 
table(df$TenYearCHD)
table(df$TenYearCHD)*100/sum(table(df$TenYearCHD))

#sampling the data to get the balance
library(ROSE)
data.rose <- ROSE(TenYearCHD ~ ., data = df, seed = 2)$data
table(data.rose$TenYearCHD)
table(data.rose$TenYearCHD)*100/sum(table(data.rose$TenYearCHD))




#Splitting the data into test and train
library(caret)
set.seed(20)
Train<-createDataPartition(data.rose$TenYearCHD,p=0.7,list=FALSE)
training<-data.rose[Train,]
testing<-data.rose[-Train,]


#Fitting The Full Model
fit = glm(TenYearCHD ~. ,data=training,family="binomial")
summary(fit)

#Stepwise Model Selection (including interaction terms)
full = glm(TenYearCHD ~ Age*Diabetes*CurrentSmoker*Gender*Prevalent.Hypertension,data=training,family="binomial")
null = glm(TenYearCHD ~ 1,data= training,family=binomial)
step(null,scope=list(lower=null,upper=full),direction="both")


#After creating this model, I looked at the summary to review significance.
fit.1 = glm(formula = TenYearCHD ~ Age + Prevalent.Hypertension + Gender + 
              Diabetes + CurrentSmoker + Prevalent.Hypertension:CurrentSmoker + 
              Age:CurrentSmoker, family = binomial, data = training)

summary(fit.1)

#I removed the insignificant interaction term to create this model:
fit2 = glm(formula = TenYearCHD ~ Age + Prevalent.Hypertension + Gender + 
             Diabetes + CurrentSmoker + Prevalent.Hypertension:CurrentSmoker 
             , family = binomial, data = training)

summary(fit2)

#To convert the coefficients to odds-ratios:
exp(coef(fit2))

#To create a confidence interval of odds-ratios:
exp(cbind(OR=coef(fit2),confint(fit2)))

#Anova Test to Determine Goodness of Fit:
anova(fit2,test="Chisq")

#Analyzing Cook’s Distance:
cooks.distance<-cooks.distance(fit2)
which(cooks.distance>1)
plot(fit2, 4)

#Wald Test to determine if predictors are significant:
library(survey)
regTermTest(fit2,"Gender")
regTermTest(fit2,"Prevalent.Hypertension")
regTermTest(fit2,"Diabetes")
regTermTest(fit2,"Age") 
regTermTest(fit2,"CurrentSmoker") 


#Hoslem-Lemeshow Goodness of Fit Test:
library(ResourceSelection)
hoslem.test(fit2$y,fitted(fit2),g=10)

#Looking at VIF for Collinearity:
library(car)
vif(fit2)

#Determining the Pseudo-Rsq:
library(pscl)
pR2(fit2)


#Plotting the effects of getting CHD
library(effects)
plot(allEffects(fit2))


#confusion matrix
library(caret)
ctrl<-trainControl(method="repeatedcv",number=10,savePredictions=TRUE)
mod_fit<-caret::train(TenYearCHD ~ Age + Prevalent.Hypertension + Gender + 
                        Diabetes + CurrentSmoker + Prevalent.Hypertension:CurrentSmoker,
                      data=training,method="glm",family="binomial",trControl=ctrl,tuneLength=5)

pred<-predict(mod_fit,newdata=testing)
levels(pred) = c("No", "Yes")
levels(testing$TenYearCHD) = c("No", "Yes")
confusionMatrix(data = pred, testing$TenYearCHD, positive = "Yes")

#cutoff
acc.perf = ROCR::performance(pr, measure = "acc")
plot(acc.perf)


#Variable of Importance
varImp(mod_fit)

#ROC Curve
library(ROCR)
p<-predict(fit2,newdata=subset(testing,select=c(1:7)),type="response")
pr<-ROCR::prediction(p,testing$TenYearCHD)
prf<-ROCR::performance(pr,measure="tpr",x.measure="fpr")
plot(prf, main = "ROC Curve")
abline(a=0, b = 1, col = "red")
auc<-ROCR::performance(pr,measure="auc")
auc<-auc@y.values[[1]]
auc


