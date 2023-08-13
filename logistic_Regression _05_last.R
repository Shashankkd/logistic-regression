library(readxl)
heartdisease <- read_excel("C:/Users/shash/Downloads/heartdisease.xls")
View(heartdisease)
attach(heartdisease)
hd=heartdisease
str(hd)
# dependent is 'target'
summary(heartdisease)
library(ggplot2)
hd=heartdisease
ggplot(heartdisease,aes(x=factor(0),y=trestbps))+geom_boxplot()
ggplot(heartdisease,aes(x=factor(0),y=age))+geom_boxplot() 
ggplot(heartdisease,aes(x=factor(0),y=chol))+geom_boxplot()
ggplot(heartdisease,aes(x=factor(0),y=thalach))+geom_boxplot()
ggplot(heartdisease,aes(x=factor(0),y=oldpeak))+geom_boxplot()

# remove outliers
data <-hd$trestbps
length(data)
quartiles <- quantile(data, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR
data_no_outlier <- subset(data, data > Lower & data < Upper)
length(data_no_outlier)
boxplot(data_no_outlier)
boxplot(trestbps)

#(i)
lrm=glm(target~age+sex+cp+trestbps+chol+fbs+
        restecg+thalach+exang+oldpeak+
          slope+ca+thal,family =gaussian ,hd)
summary(lrm)
step(lrm,direction = 'forward')
step(lrm,direction = 'backward')
lrm1=step(lrm,direction = 'both');lrm1
summary(lrm1)
#(ii)
#[ii]
set.seed(200)
n=floor(0.8*nrow(hd));n
t1=sample(seq_len(nrow(hd)),size = n,replace = F)
#train data set 80%
tr_s=hd[t1,];tr_s
#Test data set 20%
te_s=hd[-t1,];te_s

#model with train data set

##--------------------------------------------
##--------------------------------------------
##(4)
#### Or using the stepwise model selection
attach(tr_s)
flm=glm(target~age+sex+cp+trestbps+chol+fbs+
        restecg+thalach+exang+oldpeak+
          slope+ca+thal,data=tr_s,family = gaussian)
lm=step(flm,direction = 'both');lm

#
#lm=glm(target~sex+cp+trestbps+chol+
     #  restecg+thalach+exang+oldpeak+
        # slope+ca+thal,family =gaussian ,data=tr_s)
te_s$p=predict(lm ,newdata=te_s, type = 'response')
te_s$p1=ifelse(te_s$p>0.5,1,0)
table1=table(te_s$target,te_s$p1);
dimnames(table1)=list(Observed=c('Yes','No'),
                      Predicted=c('Yes','No'))
table1

# Accuracy (TP/TN)/total
accuracy=(table1[1]+table1[4])/sum(table1);accuracy


#
#Insatll required packages
install.packages('caret')

#Import required library
library(caret)

#Creates vectors having data points
Observed_value <-factor(te_s$target)
predicted_value <- factor(te_s$p1)

#Creating confusion matrix
example <- confusionMatrix(data=predicted_value, reference = Observed_value)


