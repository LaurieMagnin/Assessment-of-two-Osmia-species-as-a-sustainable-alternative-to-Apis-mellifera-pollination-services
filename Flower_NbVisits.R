visit_tree<- read.csv("~/Stage Wild Biene+ Partner/Wild Biene+Partner/Comparison exp/Data_R/visit_tree.csv", header=TRUE, sep=";")
names(visit_tree)

#Exploring data 

library(ggplot2)
boxplot(nb.of.flower , data = visit_tree)

ggplot(visit_tree, aes(x=Specie, y=nb.of.flower)) + 
  geom_boxplot(fill="slateblue", alpha=1) + 
  xlab("Specie")

########################################### Plot the number of flower per species ###########################

apple_visit<-subset(visit_tree, visit_tree$Tree%in%"apple")
apple_visit_Selected<-subset(apple_visit,apple_visit$Specie%in% c("A.mellifera", "B.terrestris","O.bicornis","O.cornuta"))
attach(apple_visit_Selected)
names(apple_visit_Selected)

P=ggplot(apple_visit_Selected, aes(x=Specie, y=nb.of.flower)) + 
  geom_boxplot(fill="slateblue", alpha=1) + scale_y_continuous(name="number of flower visited",limits=c(0,20))+ xlab("Species")+
  theme(panel.border = element_blank(),panel.background = element_blank(),axis.line.y = element_line(colour = "grey"), panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',colour = "grey"))
P+text( c(1:nlevels(apple_visit_Selected$Specie)) , P$stats[nrow(P$stats) , ]+0.5 , paste("n = ",table(apple_visit_Selected$Specie),sep=""))

############## Kruscal Wallis test 
kruskal.test(nb.of.flower~Specie) # NOT significant 

