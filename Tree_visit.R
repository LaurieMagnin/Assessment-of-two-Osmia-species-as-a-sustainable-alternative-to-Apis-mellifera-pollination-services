tree_visit_management <- read.csv("~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/tree_visit_management1.csv", sep=";")
View(tree_visit_management)

apple_visit_management<-subset(tree_visit_management, tree_visit_management$Tree%in%"apple")
attach(apple_visit_management)
names(apple_visit_management)
library(ggplot2)
ggplot(apple_visit_management, aes(x=Species, y=Visits_nb, fill=Management))+ 
  geom_boxplot(position=position_dodge(width=0.8))+ scale_y_continuous(name="Individual visit on apple tree",limits=c(0,15))+ xlab("Species")+ 
  theme(panel.border = element_blank(),panel.background = element_blank(),axis.line.y = element_line(colour = "grey"), panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',colour = "grey"))

##############################################Analyse stat###############################################
##############ANOVA assumptiom check

apple_visit_organic<-subset(apple_visit_management,apple_visit_management$Management%in%"organic")
attach(apple_visit_organic)
View(apple_visit_organic)

shapiro.test(Visits_nb[Species=="A.mellifera"])
shapiro.test(Visits_nb[Species=="B.terrestris"])
shapiro.test(Visits_nb[Species=="Hoverfly"])
shapiro.test(Visits_nb[Species=="O.cornuta"])
shapiro.test(Visits_nb[Species=="Wildbee"])

apple_visit_conventional<-subset(apple_visit_management,apple_visit_management$Management%in%"conventional")
attach(apple_visit_conventional)
View(apple_visit_conventional)

shapiro.test(Visits_nb[Species=="A.mellifera"])
shapiro.test(Visits_nb[Species=="B.terrestris"])
shapiro.test(Visits_nb[Species=="Hoverfly"])
shapiro.test(Visits_nb[Species=="O.cornuta"])
shapiro.test(Visits_nb[Species=="Wildbee"])   #ANOVA assumption not met ==> non-parametric test 

############## Kruscal Wallis test 
#for the distributions in Oranic management 
attach(apple_visit_organic)
kruskal.test(Visits_nb~Species) #significant ==>pairwise comparison
pairwise.wilcox.test(Visits_nb,Species,p.adjust.method = "BH")
#for the distributions in conventional management 
attach(apple_visit_conventional)
kruskal.test(Visits_nb~Species) #significant ==>pairwise comparison
pairwise.wilcox.test(Visits_nb,Species,p.adjust.method = "BH")

#For the distibution by species
#A.mellifera
apple_visit_A.mellifera<-subset(apple_visit_management,apple_visit_management$Species%in%"A.mellifera")
attach(apple_visit_A.mellifera)
pairwise.wilcox.test(Visits_nb,Management,p.adjust.method = "BH")
wilcox.test(Visits_nb~Management, data=apple_visit_A.mellifera, exact = FALSE, alternative = "less")

#B.terrestris
apple_visit_B.terrestris<-subset(apple_visit_management,apple_visit_management$Species%in%"B.terrestris")
attach(apple_visit_B.terrestris)
pairwise.wilcox.test(Visits_nb,Management,p.adjust.method = "BH")
wilcox.test(Visits_nb~Management, data=apple_visit_B.terrestris, exact = FALSE, alternative = "less")

#Hoverfly
apple_visit_Hoverfly<-subset(apple_visit_management,apple_visit_management$Species%in%"Hoverfly")
attach(apple_visit_Hoverfly)
pairwise.wilcox.test(Visits_nb,Management,p.adjust.method = "BH")
wilcox.test(Visits_nb~Management, data=apple_visit_Hoverfly, exact = FALSE, alternative = "less")

#O.bicornis
apple_visit_O.bicornis<-subset(apple_visit_management,apple_visit_management$Species%in%"O.bicornis")
attach(apple_visit_O.bicornis)
pairwise.wilcox.test(Visits_nb,Management,p.adjust.method = "BH")
wilcox.test(Visits_nb~Management, data=apple_visit_O.bicornis, exact = FALSE, alternative = "greater")

#O.cornuta
apple_visit_O.cornuta<-subset(apple_visit_management,apple_visit_management$Species%in%"O.cornuta")
attach(apple_visit_O.cornuta)
pairwise.wilcox.test(Visits_nb,Management,p.adjust.method = "BH")
wilcox.test(Visits_nb~Management, data=apple_visit_O.cornuta, exact = FALSE, alternative = "less")

#Wildbee
apple_visit_Wildbee<-subset(apple_visit_management,apple_visit_management$Species%in%"Wildbee")
attach(apple_visit_Wildbee)
pairwise.wilcox.test(Visits_nb,Management,p.adjust.method = "BH")
wilcox.test(Visits_nb~Management, data=apple_visit_Wildbee, exact = FALSE, alternative = "less")
