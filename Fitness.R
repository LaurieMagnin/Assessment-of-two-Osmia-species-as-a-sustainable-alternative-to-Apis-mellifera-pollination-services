DATA_parasites <- read.csv("~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/DATA_parasites.csv", sep=";")

library(ggplot2)

######################### Fitness pop: O.cornuta vs. O.bicornis #############
#Bar plot
p<-ggplot(data = DATA_parasites,
          mapping = aes(x = Species, fill = Fitness))
p + geom_bar(position = "fill")
#Data table
library(dplyr)
Fitness_pop <- DATA_parasites %>% 
  group_by(Species, Fitness) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))
View(Fitness_pop)
#Stat 

Proportion_fitness <- read.csv("~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/Analysis_R/Proportion_fitness.csv", sep=";")
chisq.test(Proportion_fitness)
MyTest <- chisq.test(Proportion_fitness)
MyTest$observed
MyTest$expected

Proportion_parasits <- read.csv("~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/Analysis_R/Proportion_parasits.csv", sep=";")
fisher.test(Proportion_parasits)

Proportion_emergence <- read.csv("~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/Analysis_R/Proportion_emergence.csv", sep=";")
fisher.test(Proportion_emergence)
#############################
#####################################################################For apple orchards
#############################################################################################

DATA_parasites_apple<- read.csv("~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/DATA_parasites_apple.csv", sep=";")

library(ggplot2)

######################### Fitness pop: O.cornuta vs. O.bicornis #############
#Bar plot
p<-ggplot(data = DATA_parasites_apple,
          mapping = aes(x = Species, fill = Fitness))
p + geom_bar(position = "fill")
#Data table
library(dplyr)
Fitness_pop <- DATA_parasites_apple %>% 
  group_by(Species, Fitness) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))
View(Fitness_pop)

######### control vs field
DATA_cornuta<-subset(DATA_parasites_apple, DATA_parasites_apple$Species%in%"O.cornuta")
p<-ggplot(data = DATA_cornuta,
          mapping = aes(x = experiment, fill = Fitness))
p + geom_bar(position = "fill")
Fitness_pop_cornuta <- DATA_cornuta %>% 
  group_by(experiment, Fitness) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))
View(Fitness_pop_cornuta)

DATA_bicornis<-subset(DATA_parasites_apple, DATA_parasites_apple$Species%in%"O.bicornis")
View(DATA_bicornis)
p<-ggplot(data = DATA_bicornis,
          mapping = aes(x = experiment, fill = Fitness))
p + geom_bar(position = "fill")
Fitness_pop <- DATA_bicornis %>% 
  group_by(experiment, Fitness) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))
View(Fitness_pop)


#############################################
###############################################cherry
##########################################################

DATA_parasites_cherry <- read.csv("~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/DATA_parasites_cherry.csv", sep=";")

library(ggplot2)

######################### Fitness pop: O.cornuta vs. O.bicornis #############
#Bar plot
p<-ggplot(data = DATA_parasites_cherry,
          mapping = aes(x = Species , fill = Fitness))
p + geom_bar(position = "fill")
#Data table
library(dplyr)
Fitness_pop <- DATA_parasites_apple %>% 
  group_by(Species, Fitness) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))
View(Fitness_pop)

######### control vs field
DATA_cornuta<-subset(DATA_parasites_apple, DATA_parasites_apple$Species%in%"O.cornuta")
p<-ggplot(data = DATA_cornuta,
          mapping = aes(x = experiment, fill = Fitness))
p + geom_bar(position = "fill")
Fitness_pop <- DATA_cornuta %>% 
  group_by(experiment, Fitness) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))
View(Fitness_pop)

DATA_bicornis<-subset(DATA_parasites_apple, DATA_parasites_apple$Species%in%"O.bicornis")
View(DATA_bicornis)
p<-ggplot(data = DATA_bicornis,
          mapping = aes(x = experiment, fill = Fitness))
p + geom_bar(position = "fill")
Fitness_pop <- DATA_bicornis %>% 
  group_by(experiment, Fitness) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))
View(Fitness_pop)
