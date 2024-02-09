setwd("C:Users/MAGNIN/Documents/Stages/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp")
emergence_apple <- read.csv("~/working/Article_Osmia/emergence_apple_censored.csv", sep=";")
View(emergence_apple)
attach(emergence_apple)
names(emergence_apple)

library(ggplot2)
library(survival)
library(plyr)
library(rms)
library("survminer")


 #### Data exploration 

plot(survfit(Surv(Emergence,Status)~1))

########################################## Apple emergence data analysis

Graph1<-ggsurvplot(model,linetype = "strata", data=emergence_apple,ylab="Emergence", xlab="Day", fun = function(x) {1 - x},legend.labs = c("O.bicornis_Ctrl", "O.bicornis_Field","O.cornuta_Ctrl","O.cornuta_Field"))
Graph1$plot <- Graph1$plot +scale_linetype_manual(values = c(2,1,2,1)) + theme(legend.text = element_text(size = 12))
print(Graph1)

####Model de cox
model<-coxph(formula = Surv(Emergence, Status) ~ Species* strata(culture)) 
summary(model)

####Model de Kaplan meier (but not supposed to be used for multifactor analysis)

model<-survreg(formula = Surv(Emergence, Status) ~ Species* experiment, dist = "logistic")
model1<-survreg(formula = Surv(Emergence, Status) ~ strata(Species)* strata(experiment), dist = "logistic")
summary(model1)
anova(model1,model)

###############################################Pair-wise comparison with selecting data to analyse

emergence_apple_bicornis<-subset(emergence_apple,emergence_apple$Species%in%"O.bicornis") #data for bicornis
attach(emergence_apple_bicornis)
model<-survreg(formula = Surv(Emergence, Status) ~ experiment, dist = "exponential") #analyse filed/control
summary(model)

emergence_apple_cornuta<-subset(emergence_apple,emergence_apple$Species%in%"O.cornuta") #data for cornuta
attach(emergence_apple_cornuta)
model<-survreg(formula = Surv(Emergence, Status) ~ experiment, dist = "exponential")#analyse filed/control
summary(model)

emergence_apple_field<-subset(emergence_apple,emergence_apple$experiment%in%"field") #data of the field exp
attach(emergence_apple_field)
model<-survreg(formula = Surv(Emergence, Status) ~ Species, dist = "exponential") #analyse cornuta/bicornis
summary(model)

emergence_apple_control<-subset(emergence_apple,emergence_apple$experiment%in%"control") #data of the controls
attach(emergence_apple_control)
model<-survreg(formula = Surv(Emergence, Status) ~ Species, dist = "exponential") #analyse cornuta/bicornis
summary(model)

#################################################################################################################
######For cherry
##############################################################################################################

emergence_cherry <- read.csv("~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/emergence_cherry_censored.csv", sep=";")
View(emergence_cherry)
attach(emergence_cherry)
names(emergence_cherry)

library(ggplot2)
library(survival)
library(plyr)
library(rms)
library("survminer")

Statu<-1*(Emergence>0)


plot(survfit(Surv(Emergence,Status)~1))

model<-survfit(Surv(Emergence,Status)~ Species+experiment)
Summary(model)
Graph1<-ggsurvplot(model,linetype = "strata", data=emergence_cherry,ylab="Emergence", xlab="Day", fun = function(x) {1 - x},
                   legend.labs = c("O.bicornis_Ctrl", "O.bicornis_Field","O.cornuta_Ctrl","O.cornuta_Field"))

Graph1$plot <- Graph1$plot + scale_linetype_manual(values = c(2,1,2,1))+ theme(legend.text = element_text(size = 12))
print(Graph1)

model<-coxph(formula = Surv(Emergence, Status) ~ Species* experiment)
summary(model)



model<-survreg(formula = Surv(Emergence, Status) ~ strata(Species)* strata(experiment), dist = "logistic")
model1<-survreg(formula = Surv(Emergence, Status) ~ Species* experiment, dist = "logistic")
summary(model)
anova(model1,model)

###############################################Pair-wise comparison
emergence_cherry_bicornis<-subset(emergence_cherry,emergence_cherry$Species%in%"O.bicornis")
#View(emergence_cherry_bicornis)
attach(emergence_cherry_bicornis)
model<-survreg(formula = Surv(Emergence, Status) ~ experiment, dist = "exponential")
summary(model)

emergence_cherry_cornuta<-subset(emergence_cherry,emergence_cherry$Species%in%"O.cornuta") 
attach(emergence_cherry_cornuta)
model<-survreg(formula = Surv(Emergence, Status) ~ experiment, dist = "exponential")
#View(emergence_cherry_cornuta)
summary(model)

emergence_cherry_field<-subset(emergence_cherry,emergence_cherry$experiment%in%"field")
attach(emergence_cherry_field)
model<-survreg(formula = Surv(Emergence, Status) ~ Species, dist = "exponential")
summary(model)

emergence_cherry_control<-subset(emergence_cherry,emergence_cherry$experiment%in%"control")
attach(emergence_cherry_control)
model<-survreg(formula = Surv(Emergence, Status) ~ Species, dist = "exponential")
summary(model)
