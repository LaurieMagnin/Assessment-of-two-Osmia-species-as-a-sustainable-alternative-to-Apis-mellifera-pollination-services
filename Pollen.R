pollen_collection <- read.csv("~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/pollen_collection.csv", sep=";")
  View(pollen_collection)
Pollensamples <- read.csv("C:/Users/MAGNIN/Downloads/data/Pollensamples.csv", sep=";")

data_pollen<-merge(pollen_collection,Pollensamples, by.x = "Code", by.y = "X")
View(data_pollen)
write.table(data_pollen, "~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/data_pollen.csv", sep=";") 
library(xlsx)
write.csv2(data_pollen, "~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/data_pollen.csv") 
library(ggplot2)

Data_pollen <- read.csv("~/working/Article_Osmia/data_pollen_select.csv", sep=";")
View(Data_pollen)
#####boxe plot
ggplot(Data_pollen, aes(x=Species, y=Rosacea)) + geom_boxplot() 

Data_pollen_cherry<-subset(Data_pollen,Data_pollen$Tree%in%"cherry")
ggplot(Data_pollen_cherry, aes(x=Species, y=Rosacea)) + geom_boxplot()

Data_pollen_apple<-subset(Data_pollen,Data_pollen$Tree%in%"apple")
ggplot(Data_pollen_apple, aes(x=Species, y=Rosacea)) + geom_boxplot()

#### proportions
p<-ggplot(data = Data_pollen,
          mapping = aes(x = Species, fill = Visited.plant))
p + geom_bar(position = "fill")

p<-ggplot(data = Data_pollen_cherry,
          mapping = aes(x = Species, fill = Visited.plant))
p + geom_bar(position = "fill")

library(dplyr)
Visit_plants <- Data_pollen_cherry %>% 
  group_by(Species, Visited.plant) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))
View(Visit_plants)

p<-ggplot(data = Data_pollen_apple,
          mapping = aes(x = Species, fill = Visited.plant))
p + geom_bar(position = "fill")

Visit_plants <- Data_pollen_apple %>% 
  group_by(Species, Visited.plant) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))
View(Visit_plants)
###Analyse for bicornis and cornuta from the same nest
Data_pollen_nest<-subset(Data_pollen_apple,Data_pollen_apple$Introduced_bee%in%"O. bicornis")
View(Data_pollen_nest)
p<-ggplot(data = Data_pollen_nest,
          mapping = aes(x = Bee, fill = Visited.plant))
p + geom_bar(position = "fill")


Data_pollen <- read.csv("~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/data_pollen%.csv", sep=";")

library(ggplot2)
Data_pollen_cherry<-subset(Data_pollen,Data_pollen$Tree%in%"cherry")
ggplot(Data_pollen_cherry, aes(x=Bee, y=Rosacea)) + geom_boxplot()

Data_pollen_apple<-subset(Data_pollen,Data_pollen$Tree%in%"apple")
ggplot(Data_pollen_apple, aes(x=Bee, y=Rosacea)) + geom_boxplot()


# Analysis by period 

p<-ggplot(data = Data_pollen_cherry,
          mapping = aes(x = Bee, fill = Visited.plant))
p + facet_grid(. ~ Period)+ geom_bar(position = "fill")

p<-ggplot(data = Data_pollen_apple,
          mapping = aes(x = Bee, fill = Visited.plant))
p + facet_grid(. ~ Period)+ geom_bar(position = "fill")

# Analysis by management practices

p<-ggplot(data = Data_pollen_apple,
          mapping = aes(x = Bee, fill = Visited.plant))
p + facet_grid(. ~ Management)+ geom_bar(position = "fill")

###################### STAT ANALYSIS

Proportion_pollen <- read.csv("~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/Analysis_R/Proportion_pollen.csv", sep=";")
chisq.test(Proportion_pollen)
MyTest <- chisq.test(Proportion_pollen)
MyTest$observed
MyTest$expected

Proportion_rosacea <- read.csv("~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/Analysis_R/Proportion_rosacea.csv", sep=";")
fisher.test(Proportion_rosacea)

#Méthode avec package ggstatplot

library ('rstatix')
library(ggstatsplot)
library(dplyr)

library(ggplot2)
library(ggpubr)

#https://www.google.com/search?q=chisq.posthoc.test+output+explenation&oq=chisq.posthoc.test+output+explenation&aqs=edge..69i57.6667j0j4&sourceid=chrome&ie=UTF-8#fpstate=ive&vld=cid:66b12639,vid:8Tj0-yMPO64
Data_pollen_bicornis<-subset(Data_pollen_apple,Data_pollen_apple$Introduced_bee%in%"O. bicornis")
Data_pollen_cornuta<-subset(Data_pollen_apple,Data_pollen_apple$Introduced_bee%in%"O. cornuta")

cherry <- ggbarstats(
  data = Data_pollen_cherry,
  x = Visited.plant,
  y = Species)

apple_bicornis <- ggbarstats(
  data = Data_pollen_bicornis,
  x = Visited.plant,
  y = Species)

apple_cornuta <- ggbarstats(
  data = Data_pollen_cornuta,
  x = Visited.plant,
  y = Species)

ggarrange(cherry, apple_bicornis, apple_cornuta, ncol = 2, nrow = 2)

COLORS_larval_size_L1L3<-subset(COLORS_larval_size, COLORS_larval_size$instar!="L2")

contingency_table<-table(COLORS_larval_size_L1L3$variety,COLORS_larval_size_L1L3$instar)
contingency_table
posthoc_2<-pairwise_prop_test(contingency_table)

grouped_ggbarstats(
  # arguments relevant for `ggbarstats()`
  data = Data_pollen_apple,
  x = Visited.plant,
  y = Species,
  grouping.var = Introduced_bee,
  perc.k = 1,
  package = "ggsci",
  palette = "category10_d3",
  # arguments relevant for `combine_plots()`
  title.text = "Passenger survival on the Titanic by gender and age",
  caption.text = "Asterisks denote results from proportion tests; \n***: p < 0.001, ns: non-significant",
  plotgrid.args = list(ncol = 2)
  )
