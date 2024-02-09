Nesting_apple <- read.csv("~/working/Article_Osmia/Nesting_appleD.csv", sep=";")
attach(Nesting_apple)
names(Nesting_apple)

library(ggplot2)

GG<-ggplot(Nesting_apple, aes(x=reorder(Day, Cumulated_nesting.bee), y=Cumulated_nesting.bee, fill=Introduced_bee))+ 
  geom_boxplot(position=position_dodge(width=1))+ scale_y_continuous(name="Cumulated number of nesting bees")+ xlab("day after introduction")+ 
  theme(panel.border = element_blank(),panel.background = element_blank(),axis.line.y = element_line(colour = "grey"), panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',colour = "grey"))
GG+ scale_fill_manual(values=c("green4","purple"),name="Species of bee")



GS<-ggplot(Nesting_apple, aes(x=reorder(Day,Cumulated_spilled_bee), y=Cumulated_spilled_bee, colour=Introduced_bee))+ 
  geom_boxplot(position=position_dodge(width=1))+ scale_y_continuous(name="Cumulated number of nesting bees",limits=c(0,400))+ xlab("day after introduction")+ 
  theme(panel.border = element_blank(),panel.background = element_blank(),axis.line.y = element_blank(), panel.grid.major.y = element_blank())
GS+ scale_color_manual(values=c("purple","green4"),name="Spillover of bee") 

Nesting_apple <- gather(Nesting_apple, "bee", "cumulated_nesting", 8|10) 

Nesting_apple_average <-Nesting_apple %>%                                 # Group data
  group_by(Introduced_bee, Day, bee) %>%
  dplyr::summarize(average = mean(cumulated_nesting), sd = sd(cumulated_nesting), n = n(),
                   se = sd / sqrt(n)) %>% 
  as.data.frame()

Nesting_apple_average_bicornis <- subset(Nesting_apple_average, Nesting_apple_average$Introduced_bee == "bicornis")


plot1<-ggplot(Nesting_apple_average_bicornis, aes(x=reorder(Day,average), y=average, fill=bee)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=average-se, ymax=average+se), width=.2,
                position=position_dodge(.9)) + ylim(0,300)


Bicornis <- plot1+labs(x="Day after introduction", y = "Cumulated number of nesting bees") + 
  theme_classic() +
  scale_fill_manual(values=c('#999999','#69b3a2' ))

Bicornis

Nesting_apple_average_cornuta <- subset(Nesting_apple_average, Nesting_apple_average$Introduced_bee == "cornuta")

plot2<-ggplot(Nesting_apple_average_cornuta, aes(x=reorder(Day,average), y=average, fill=bee)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=average-se, ymax=average+se), width=.2,
                position=position_dodge(.9)) 


Cornuta <- plot2+labs(x="Day after introduction", y = "Cumulated number of nesting bees") + 
  scale_y_continuous(breaks=seq(0, 300, 50))+
  theme_classic() +
  scale_fill_manual(values=c('#999999','#69b3a2' ))

library(ggpubr)
ggarrange(Bicornis, Cornuta, ncol = 2)

##############################################Analyse stat###############################################
##############ANOVA assumptiom check
Nesting_apple_bicornis<-subset(Nesting_apple,Nesting_apple$Introduced_bee%in%"bicornis")
attach(Nesting_apple_bicornis)
View(Nesting_apple_bicornis)
shapiro.test(Cumulated_nesting.bee[Day=="day 2"])
shapiro.test(Cumulated_nesting.bee[Day=="day 4"]) ###Not good for this one but almost 0 so ANOVA possible?
shapiro.test(Cumulated_nesting.bee[Day=="day 6"])
shapiro.test(Cumulated_nesting.bee[Day=="day 8"])
shapiro.test(Cumulated_nesting.bee[Day=="day 10"])
shapiro.test(Cumulated_nesting.bee[Day=="day 13"])
shapiro.test(Cumulated_nesting.bee[Day=="day 18"])


Nesting_apple_cornuta<-subset(Nesting_apple,Nesting_apple$Introduced_bee%in%"cornuta")
attach(Nesting_apple_cornuta)
View(Nesting_apple_cornuta)
shapiro.test(Cumulated_nesting.bee[Day=="day 2"])
shapiro.test(Cumulated_nesting.bee[Day=="day 4"])
shapiro.test(Cumulated_nesting.bee[Day=="day 6"])
shapiro.test(Cumulated_nesting.bee[Day=="day 8"])
shapiro.test(Cumulated_nesting.bee[Day=="day 10"])
shapiro.test(Cumulated_nesting.bee[Day=="day 13"])
shapiro.test(Cumulated_nesting.bee[Day=="day 18"])

Nesting_apple_18<-subset(Nesting_apple,Nesting_apple$Day%in%"day 18")
View(Nesting_apple_18)
attach(Nesting_apple_18)
library(car)
leveneTest(Cumulated_nesting.bee ~ Introduced_bee, data = Nesting_apple_18)

##########################Non-parametric test : Wilkoxon rank sum test#####################
#################Comparison for each day 
Nesting_apple_18<-subset(Nesting_apple,Nesting_apple$Day%in%"day 18")
wilcox.test(Cumulated_nesting.bee~Introduced_bee, data=Nesting_apple_18, exact = FALSE, alternative = "less",p.adjust.method="fdr")

Nesting_apple_13<-subset(Nesting_apple,Nesting_apple$Day%in%"day 13")
View(Nesting_apple_13)
wilcox.test(Cumulated_nesting.bee~Introduced_bee, data=Nesting_apple_13, exact = FALSE, alternative = "less")

Nesting_apple_10<-subset(Nesting_apple,Nesting_apple$Day%in%"day 10")
View(Nesting_apple_10)
wilcox.test(Cumulated_nesting.bee~Introduced_bee, data=Nesting_apple_10, exact = FALSE, alternative = "less")

Nesting_apple_8<-subset(Nesting_apple,Nesting_apple$Day%in%"day 8")
wilcox.test(Cumulated_nesting.bee~Introduced_bee, data=Nesting_apple_8, exact = FALSE, alternative = "less")

Nesting_apple_6<-subset(Nesting_apple,Nesting_apple$Day%in%"day 6")
wilcox.test(Cumulated_nesting.bee~Introduced_bee, data=Nesting_apple_6, exact = FALSE, alternative = "less")

Nesting_apple_4<-subset(Nesting_apple,Nesting_apple$Day%in%"day 4")
wilcox.test(Cumulated_nesting.bee~Introduced_bee, data=Nesting_apple_4, exact = FALSE, alternative = "less")

Nesting_apple_2<-subset(Nesting_apple,Nesting_apple$Day%in%"day 2")
wilcox.test(Cumulated_nesting.bee~Introduced_bee, data=Nesting_apple_2, exact = FALSE, alternative = "less")

#####################################GLM - Poisson

model1<-glm(Cumulated_nesting.bee~Introduced_bee+Day+Introduced_bee*Day, family="poisson", data=Nesting_apple)
summary(model1)



