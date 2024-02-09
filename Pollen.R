library (ggplot)
library(dplyr)
library(ggstatsplot)

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

###################### STAT ANALYSIS

Proportion_pollen <- read.csv("~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/Analysis_R/Proportion_pollen.csv", sep=";")
chisq.test(Proportion_pollen)
MyTest <- chisq.test(Proportion_pollen)
MyTest$observed
MyTest$expected

Proportion_rosacea <- read.csv("~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/Analysis_R/Proportion_rosacea.csv", sep=";")
fisher.test(Proportion_rosacea)


