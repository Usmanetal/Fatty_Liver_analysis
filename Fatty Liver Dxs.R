### Data analysis of Fatty Liver Disease
library(tidyverse)

### Control data
library(readxl)
control_PI <- read_excel("C:/Users/Usman Ola/Downloads/Philip Ibinaiye PHD Raw Data (1).xlsx", 
                         sheet = "Control Data")
View(control_PI)
control_PI$group<- 0

### Cases data
cases_PI <- read_excel("C:/Users/Usman Ola/Downloads/Philip Ibinaiye PHD Raw Data (1).xlsx", 
                       sheet = "Cases Data")
View(cases_PI)
cases_PI$group<- 1


### Merge data for case-control data

new_data<- rbind(control_PI,cases_PI)
### to capture the factor levels against numeric or continous data
new_data[,c(3:24,26,30,34,46)]<- sapply(new_data[,c(3:24,26,30,34,46)], as.factor)

### Univariate Logistic regression
Eureka<- function(x){
y=glm(factor(group)~x,data = new_data,family = binomial)
z=summary(y)%>%coefficients()%>%
  cbind(confint(y))%>%
  subset(select=c("Estimate","Pr(>|z|)","2.5 %","97.5 %"))%>%
  data.frame()%>%slice(-1)%>%mutate(Estimate=exp(Estimate))
return(z)

}

#### Multivariate Logistic Regression

prof=glm(factor(group)~Religion+
           Physical_Activity+
           Junk_foods+
           Meat+
           Sun_exposure+
           Fura_da_nunu+
           Sleep_duration+
           Body_weight+
           BMI+Waist+Hip+
           Systolic_blood+
           Diastolic_blood,
         data = new_data,family = "binomial")
### Reg table
summary(prof)%>%coefficients()%>%cbind(confint(prof))
