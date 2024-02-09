visit_tree<- read.csv("~/working/Article_Osmia/visit_tree.csv", header=TRUE, sep=";")
View(visit_tree)
library(ggplot2)

#Exploring data
visit_tree$Average_time.flower <- as.numeric(visit_tree$Average_time.flower)

boxplot(Average_time.flower~Specie , data = visit_tree)

ggplot(visit_tree, aes(x=Specie, y=Average_time.flower)) + 
  geom_boxplot(fill="slateblue", alpha=1) + 
  xlab("Specie")


########################################### Plot the time/flower per species ###########################


apple_visit<-subset(visit_tree, visit_tree$Tree%in%"apple") #Selection of apple orchard observations
apple_visit_Selected<-subset(apple_visit,apple_visit$Specie%in% c("A.mellifera", "B.terrestris","O.bicornis","O.cornuta")) #Selection of species 
attach(apple_visit_Selected)
names(apple_visit_Selected)

P=ggplot(apple_visit_Selected, aes(x=Specie, y=Average_time.flower)) + 
  geom_boxplot() + scale_y_continuous(name="Time per flower visit (s)",limits=c(0,15))+ xlab("Species")+
  theme(panel.border = element_blank(),panel.background = element_blank(),axis.line.y = element_line(colour = "grey"), panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',colour = "grey"))


P+text( c(1:nlevels(apple_visit_Selected$Specie)) , P$stats[nrow(P$stats) , ]+0.5 , paste("n = ",table(apple_visit_Selected$Specie),sep=""))

##############################################Analyse stat###############################################
##############ANOVA assumptiom check

shapiro.test(Average_time.flower[Specie=="A.mellifera"])
shapiro.test(Average_time.flower[Specie=="O.cornutaF"])
shapiro.test(Average_time.flower[Specie=="Bombus"])
shapiro.test(Average_time.flower[Specie=="O.bicornisF"]) #ANOVA assumption not met ==> non-parametric test 

############## Kruscal Wallis test 

kruskal.test(Average_time.flower~Specie) #significant ==>pairwise comparison

pairwise.wilcox.test(Average_time.flower,Specie,p.adjust.method = "BH")
