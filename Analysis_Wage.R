#R studion code
#Name: Nikhila Mariam Biju'
#Matriculation Number: 26104
#Course Name; Data Analysis and Statistics
#Date: 31/08/2019


#----------------------------------------------------------
# Reset R's brain
#----------------------------------------------------------
rm(list=ls())

#----------------------------------------------------------
# Reset graphic device
# As long as there is any dev open (exept "null device") 
# close the active one!
# Caution: closes all open plots!!!!
#----------------------------------------------------------
while(!is.null(dev.list()))
{
  dev.off()
}


require(ISLR)
library(ISLR)
attach(Wage)
assessment_dataframe <- Wage[sample(nrow(Wage), 3000), ]

##installing the necessary packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("GGally")
install.packages("gam")
install.packages("funModeling")
install.packages("devtools")
install.packages("caret")
install.packages('knitr', dependencies = TRUE)
library(GGally) # ggpairs plot
library (gam)  # GAM
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(funModeling)
library(devtools)
library(knitr)
library(caret)
library(splines)
#creating a variable my_data and assigning only year, age, education, wage from the dataframe to my_data
my_data<- assessment_dataframe %>% select(year,age,education,wage)
#fininding the summary detail of my_data
summary(my_data)
glimpse(my_data)

data(my_data)
df_status(my_data)
freq(my_data)
plot_num(my_data)
profiling_num(my_data)

#scatterplot showing the distribution of year, wage, education, age in my_data
#for each year, distribution of wage(y-axis) corresponding to education(x-axis), with points showing the distribution of age groups
ggplot(my_data, aes(education, wage)) + geom_point()+geom_point(aes(color = age))+theme_bw() + labs(title="Scatterplot")+ facet_wrap( ~ year)

#finding the distribution of numeric variable(year,age,wage) in my_data
#numeric data
my_data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value,fill=key)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins=sqrt(nrow(OJ))) +
  theme(legend.position="none")
#finding the distribution of categorial variable in my_data
#categorial data
my_data %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value, fill=value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.85),legend.position="none")


fit.1= lm(wage~age + s(year,3) +education ,data=my_data)
fit.2= lm(wage~poly(age,2) + s(year,3) + education,data=my_data)
fit.3= lm(wage~poly(age,3) + s(year,3)  + education ,data=my_data)
fit.4= lm(wage~poly(age,4) + s(year,3) + education  ,data=my_data)
fit.5= lm(wage~poly(age,5) + s(year,3) + education ,data=my_data)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

plot(my_data$age,my_data$wage)

#----------------------------------------------------------
#Analysis of research question 1
#----------------------------------------------------------

#relationship of year with wage
#with the use of linear model
ggplot(my_data, aes(x=year, y=wage)) + 
  geom_point() +  
  geom_smooth(method="lm", formula = "y ~ x + I(x^2)", color="red")
#scatterplot showing the relationship of year with wage without the use of function lm()
ggplot(my_data,aes(x=year,y=wage)) + geom_point(color="dark blue")

#relationship of age with wage
#with the use of linear model
ggplot(my_data, aes(x=age, y=wage)) + 
  geom_point() +  
  geom_smooth(method="lm", formula = "y ~ x ", color="red")
#scatterplot showing the relationship of age with wage without the use of lm() function
ggplot(my_data,aes(x=age,y=wage)) + geom_point(color="dark red")

#relationship of education with wage
my_data[,indexes]%>%
  gather(-wage, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = wage, color = value)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free")+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.85),legend.position="none")

#----------------------------------------------------------
#Analysis of research question 2
#----------------------------------------------------------

#correlation

#correlation  coefficient of age with wage
cor(age, wage, method = c("pearson", "kendall", "spearman"))
#correlation coefficent of year with wage
cor(year, wage, method = c("pearson", "kendall", "spearman"))
#correlation coeffient of education with wage
cor(education, wage, method = c("pearson", "kendall", "spearman"))
cor.test(age, wage, method=c("pearson", "kendall", "spearman"))

#scatterplot showing correlation and p-value for year with wage
ggscatter(my_data, x = "year", y = "wage", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson"
)
#scatterplot showing correlation and p-value for age with wage
ggscatter(my_data, x = "age", y = "wage", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson"
)
#scatterplot showing correlation and p-value for education with wage
my_data$education<- as.numeric(my_data$education)
ggscatter(my_data, x = "education", y = "wage", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson"
)

#correlation matrix
cor(my_data[,1:2])
cor(my_data[,c(1,2,4)])
#strong,weak correlation
ggcorr(my_data)
#plot showing correlation of age,year,education with wage
ggcorr(my_data,label = TRUE)

#correclation with ggpairs
ggpairs(my_data, 
        columns = c("year", "age", "education","wage"), 
        upper = list(continuous = wrap("cor", 
                                       size = 10)), 
        lower = list(continuous = "smooth"))


#----------------------------------------------------------
#Analysis of research question 3
#----------------------------------------------------------

##testing and training data
set.seed(4)
trainset=sample(1:nrow(my_data),0.5*nrow(my_data))
wageDf.testset=my_data[-trainset,]
wageVar.testset=my_data$wage[-trainset]

##creating folders
install.packages("caret")
library(caret) # Showing Confusion Matrix Data
set.seed(1)
foldss=createFolds(my_data$wage[trainset], k=10)

# Set up a matrix which will have 1 row for every CV iteration and 1 column for each polynomial degree.
polyDf = 8
cvErrors = matrix(nrow=10,ncol=polyDf)

# Loop over degrees of polynomial 
for(polyDegFree in 1:polyDf){
  # Loop over folds of cv
  for(k in 1:10){
    fit=gam(wage~poly(age, polyDegFree)+ s(year,3) + education , data = my_data, subset = -foldss[[k]])
    preds=predict(fit,newdata=my_data[foldss[[k]],])
    cvErrors[k,polyDegFree]=mean((preds-my_data[foldss[[k]],c("wage")])^2)
  }   
}

# Find which degree has lowest average MSE over all k folds:
meanCvErrorsPoly = apply(cvErrors,2,mean)
plot(meanCvErrorsPoly, type = 'b', xlab = "Polynomial Degree", ylab = "Squared Error")
bestPoly = which.min(meanCvErrorsPoly)

set.seed(4)
# Loop over degrees of polynomial 
degFreeToTry = 20
cvErrorsSmSp = matrix(nrow=10,ncol=degFreeToTry)
cvErrorsNaSp = matrix(nrow=10,ncol=degFreeToTry)
cvErrorsCut  = matrix(nrow=10,ncol=degFreeToTry+1)

for(degFree in 1:degFreeToTry){
  
  my_data$tmp =  cut(my_data$age,degFree+1)
  # Loop over folds of cv
  for(k in 1:10){
    ### Cut
    fit=gam(wage~ tmp + s(year,3) + education , data = my_data, subset = -foldss[[k]])
    preds=predict(fit,newdata=my_data[foldss[[k]],])
    cvErrorsCut[k,degFree+1]=mean((preds-my_data[foldss[[k]],c("wage")])^2)
    ### Smoothing spline
    fit=gam(wage~ s(age, degFree)+ s(year,3)  + education , data = my_data, subset = -foldss[[k]])
    preds=predict(fit,newdata=my_data[foldss[[k]],])
    cvErrorsSmSp[k,degFree]=mean((preds-my_data[foldss[[k]],c("wage")])^2)
    ### natural spline
    fit=gam(wage~ ns(age, degFree)+ s(year,3)  + education , data = my_data, subset = -foldss[[k]])
    preds=predict(fit,newdata=my_data[foldss[[k]],])
    cvErrorsNaSp[k,degFree]=mean((preds-my_data[foldss[[k]],c("wage")])^2)
  }   
}
### Local Regression
spanNums = seq(0.1, 0.7, 0.06)
cvErrorsLocReg  = matrix(nrow=10,ncol=length(spanNums))
currIdx = 0
for(spanNum in spanNums){
  # Loop over folds of cv
  currIdx = currIdx + 1
  for(k in 1:10){
    #  local regression
    fit=gam(wage~ lo(age, span = spanNum)+ s(year,3) + education , data = my_data, subset = -foldss[[k]])
    preds=predict(fit,newdata=my_data[foldss[[k]],])
    cvErrorsLocReg[k,currIdx]=mean((preds-my_data[foldss[[k]],c("wage")])^2)
  }   
}
# Find which degree has lowest average MSE over all k folds:
par(mfrow= c(2,2))
meanCvErrorsSmSp = apply(cvErrorsSmSp,2,mean)
meanCvErrorsNaSp = apply(cvErrorsNaSp,2,mean)
meanCvErrorsCut = apply(cvErrorsCut,2,mean)
meanCvErrorsLocReg = apply(cvErrorsLocReg,2,mean)
plot(meanCvErrorsSmSp, type = 'b', xlab = "Smooth Spline Degrees", ylab = "Squared Error")
plot(meanCvErrorsNaSp, type = 'b', xlab = "Natural Spline Degrees", ylab = "Squared Error")
plot(meanCvErrorsCut, type = 'b', xlab = "Number of Cuts", ylab = "Squared Error")
plot(spanNums, meanCvErrorsLocReg, type = 'b', xlab = "Span (Local regression)", ylab = "Squared Error")

bestSmSpDegFr = which.min(meanCvErrorsSmSp)
bestNaSpDegFr = which.min(meanCvErrorsNaSp)
bestCut = which.min(meanCvErrorsCut)
bestLocRegSpan = spanNums[which.min(meanCvErrorsLocReg)]

### Cut
my_data$tmp =  cut(my_data$age,bestCut)
fit=gam(wage~ tmp + s(year,3)  + education , data = my_data, subset = train)
preds=predict(fit,newdata=my_data[-train,])
cutMSE = mean((preds-wageVar.testset)^2)
cutTrainingMse= sum(fit$residuals^2)/fit$df.residual

### Smoothing spline
fit=gam(wage~ s(age, bestSmSpDegFr)+ s(year,3) + education , data = my_data, subset = train)
preds=predict(fit,newdata=wageDf.testset)
smthSplMSE = mean((preds-wageVar.testset)^2)
smthSplTrainingMse= sum(fit$residuals^2)/fit$df.residual

### natural spline
fit=gam(wage~ ns(age, bestNaSpDegFr)+ s(year,3) + education , data = my_data, subset = train)
preds=predict(fit,newdata=wageDf.testset)
naturSplMSE = mean((preds-wageVar.testset)^2)
natSplTrainingMse= sum(fit$residuals^2)/fit$df.residual

### Local Regression
fit=gam(wage~ lo(age, span = bestLocRegSpan)+ s(year,3) + education , data = my_data, subset = train)
preds=predict(fit,newdata=wageDf.testset)
locRegMSE = mean((preds-wageVar.testset)^2)
locRegTrainingMse= sum(fit$residuals^2)/fit$df.residual

### Poly
fit=gam(wage~poly(age, bestPoly)+ s(year,3) + education , data = my_data, subset = -foldss[[k]])
preds=predict(fit,newdata=wageDf.testset)
polyMSE = mean((preds-wageVar.testset)^2)
polyTrainingMse= sum(fit$residuals^2)/fit$df.residual

## Display Results in Table Format
rowLabels = c("CV mean MSE (for Tuning the Parameters)",
              "Training Data MSE","Test Data MSE",
              "Tuning Value (DoF, Cuts, Span, Deg)")

# Agrregate desored data
var1 = c(meanCvErrorsSmSp[bestSmSpDegFr],smthSplTrainingMse, smthSplMSE, bestSmSpDegFr)
var2 = c(meanCvErrorsNaSp[bestNaSpDegFr],natSplTrainingMse, naturSplMSE, bestNaSpDegFr)
var3 = c(meanCvErrorsCut[bestCut],cutTrainingMse, cutMSE, bestCut)
var4 = c(meanCvErrorsLocReg[which.min(meanCvErrorsLocReg)],locRegTrainingMse, locRegMSE, bestLocRegSpan)
var5 = c(meanCvErrorsPoly[bestPoly],polyTrainingMse,polyMSE, bestPoly)

# Create dataframe and display it in table format
df = data.frame(var1, var2, var3,var4,var5, row.names = rowLabels)
colnames(df) = c("Smooth Spline", "Natural Spline", "Cut", "Local Regression", "Polynomial")
kable(df, format = 'markdown')

par(mfrow = c(2,3))
### Smoothing spline
fit=gam(wage~ poly(age, 2)+ s(year,3) + education , data = my_data, subset = train)
#ploting the prediction for age/year/education with wage
plot(fit, se=TRUE ,col ="red")

#----------------------------------------------------------
#Analysis of research question 4
#----------------------------------------------------------


#RSE value, p-value, F value for education with wage
lm.edu=lm(wage~education,data=my_data)
summary(lm.edu)
#RSE value, p-value, F value for age with wage
lm.age=lm(wage~age,data=my_data)
summary(lm.age)
#RSE value, p-value, F value for year with wage
lm.year=lm(wage~year,data=my_data)
summary(lm.year)
#year+age+education with wage, RSE, p-vale, F-value
my_data$education<-as.factor(my_data$education)
lm.fit2=lm(wage ~ (age+education+year),data=my_data)
summary(lm.fit2)

#----------------------------------------------------------
#Analysis of reserach question 5
#----------------------------------------------------------


#calculates the multiple R squared error and p value for year,age, education with wage to check the linear model fit the data or not
#year with wage
summary(lm.year)
ggplot(my_data, aes(x=year, y=wage)) + 
  geom_point() +  
  geom_smooth(method="lm", formula = "y ~ x + I(x^2)", color="red")
#age with wage
summary(lm.age)
ggplot(my_data, aes(x=age, y=wage)) + 
  geom_point() +  
  geom_smooth(method="lm", formula = "y ~ x + I(x^2)", color="red")
#education with wage
summary(lm.edu)

#----------------------------------------------------------
#Analysis of research question 6
#----------------------------------------------------------


#interaction effect

##interaction effect
install.packages("interplot")
library(interplot)
lm.intercation=lm(wage~(age+education),data=my_data)
anova(lm.intercation)
lm.intercation1=lm(wage~(age*education),data=my_data)
anova(lm.intercation1)
#interaction showing education coefficient with age
interplot(m = lm.intercation1, var1 = "education", var2 = "age")
#interaction showing age coefficient with age
interplot(m = lm.intercation1, var1 = "age", var2 = "education")

