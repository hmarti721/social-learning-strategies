
setwd("C:/Users/hanna/Documents/STRI 2019")
Age_Exp<- read.csv(file="Age_Experience_data20190904.csv", header=TRUE, sep=',')
head(Age_Exp)

library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(MuMIn)
library(emmeans)

#Calculate dependent variable, 
Age_Exp%>%
  select(1:9)%>%
  mutate(Prop.PT.age=case_when(Treated.leaf=="LS" ~ (LS/(LS+HH)),
                               Treated.leaf=="HH" ~ (HH/(LS+HH))))%>%
  mutate(Leaves_taken=LS+HH)%>%
  mutate(PT_taken=case_when(Treated.leaf=="LS" ~ LS,
                            Treated.leaf=="HH" ~ HH))%>%
  mutate(ln_LeavesTaken = log(Leaves_taken))->Age_Exp1
Age_Exp1%>%filter(Leaves_taken>0)->Age_Exp2
Age_Exp2$Hour<-as.numeric(Age_Exp2$Hour)

##### Collapse time points into first four hours and last four hours
Age_Exp2%>% 
  mutate(Half=case_when(Hour<4~"First",Hour>3~"Last"))%>%
  group_by(Treatment,Treated.leaf,Date,Colony,sub,Forager.age,Half)%>%
  summarise(LS_half=sum(LS),HH_half=sum(HH))%>%
  mutate(Prop.PT.age=case_when(Treated.leaf=="LS" ~ (LS_half/(LS_half+HH_half)),
                               Treated.leaf=="HH" ~ (HH_half/(LS_half+HH_half))))%>%
  mutate(Leaves_taken=LS_half+HH_half)%>%
  mutate(PT_taken=case_when(Treated.leaf=="LS" ~ LS_half,
                            Treated.leaf=="HH" ~ HH_half))%>%
  mutate(ln_LeavesTaken = log(Leaves_taken))->Age_Exp_Half

AEH1<-read.csv(file="AgeExperienceDataset.csv", header=TRUE, sep=',')

# Calculate the amount of work done by old vs young ants and experienced vs naive ants. 
## Numbers in these columns are the number of leaf discs picked up by each group over the course of a four hour time block.
AEH1%>%
  mutate(Old=as.numeric(Old),
         Young=as.numeric(Young),
         Naive=case_when(Treatment%in% c("I","III") ~ Old,
                         Treatment %in% c("II","IV") ~ Young),
         Experienced=case_when(Treatment%in% c("I","III") ~ Young,
                               Treatment %in% c("II","IV") ~ Old),
         Prop_For_Exp=Experienced/(Experienced+Naive),
         Prop_For_Old=Old/(Old+Young))->pfh


PropFor_Half<-merge(pfh,Age_Exp_Half)  

### Make a column for whether the forager group in question is experienced or naive
PropFor_Half%>%
  mutate(For_EorN=
           case_when(Treatment%in%c("I","III") & Forager.age=="Young"~ "Experienced",
                     Treatment %in% c("II","IV") & Forager.age=="Young" ~ "Naive",
                     Treatment%in%c("I","III") & Forager.age=="Old"~ "Naive",
                     Treatment %in% c("II","IV") & Forager.age=="Old" ~ "Experienced",))%>%
  mutate(Old_EorN=case_when(Treatment%in% c("I","III") ~ "Naive",
                            Treatment %in% c("II","IV") ~ "Experienced"),
         Young_EorN=case_when(Treatment%in% c("I","III") ~ "Experienced",
                              Treatment %in% c("II","IV") ~ "Naive"),
         Tiny_EorN=case_when(Treatment%in% c("IV","III") ~ "Experienced",
                             Treatment %in% c("II","I") ~ "Naive"))%>%
  mutate(Bin_Exp=case_when(Prop_For_Exp<0.25 ~ "0-25%",
                           Prop_For_Exp>0.24 & Prop_For_Exp<0.5 ~ "25%-50%",
                           Prop_For_Exp>0.49 & Prop_For_Exp<0.75 ~ "50%-75%",
                           Prop_For_Exp<0.74~ "75-100%"))%>%
  mutate(Forager.type=case_when(For_EorN=="Experienced"&Forager.age=="Old"~"Experienced, Old",
                                For_EorN=="Experienced"&Forager.age=="Young"~"Experienced, Young",
                                For_EorN=="Naive"&Forager.age=="Young"~"Naive, Young",
                                For_EorN=="Naive"&Forager.age=="Old"~"Naive, Old"))->PropFor_Half1



#Get the mean and median for the amount of PT leaf taken over all groups
summary(PropFor_Half1$PT_taken)

#### Model including naive and experienced ants
mod.all1 <- glmer.nb(PT_taken~
                       For_EorN+
                       Forager.age+
                       Tiny_EorN+
                       Half+
                       Treated.leaf+
                       For_EorN*Forager.age+
                       Prop_For_Exp+
                       offset(ln_LeavesTaken)+
                       (1| Colony), data=PropFor_Half1,na.action=na.fail)
summary(mod.all1)

emmeans(mod.all1, "For_EorN",type="response")
emmeans(mod.all1, "Forager.age",type="response")
emmeans(mod.all1, "Tiny_EorN",type="response")
emmeans(mod.all1, "Treated.leaf",type="response")
emmeans(mod.all1, "Half",type="response")
emmeans(mod.all1, "Prop_For_Exp", type="response", cov.reduce = range)
emmeans(mod.all1, "Forager.age", by= "For_EorN", type="response")





# Same model for Experienced ants
data.e<-subset(PropFor_Half1, For_EorN=="Experienced")
summary(data.e$PT_taken)
                  
 
## Best model that includes the variables I manipulated
mod.exp1 <- glmer.nb(PT_taken~
                       Forager.age+
                       Tiny_EorN+
                       Half+
                       Treated.leaf+
                       Prop_For_Exp+
                       Forager.age*Prop_For_Exp+
                       offset(ln_LeavesTaken)+
                       (1| Colony), data=data.e,na.action=na.fail)
summary(mod.exp1)

### effect sizes
emmeans(mod.exp1, "Forager.age", type="response")
emmeans(mod.exp1, "Tiny_EorN", type="response")
emmeans(mod.exp1, "Half", type="response")
emmeans(mod.exp1, "Treated.leaf", type="response")
emmeans(mod.exp1, "Prop_For_Exp", type="response", cov.reduce = range)
emmeans(mod.exp1, "Forager.age", by= "Prop_For_Exp", type="response", cov.reduce = range)

#Same process for Naive ants
data.n<-subset(PropFor_Half1, For_EorN=="Naive")


###Naive only model
mod.naive1 <- glmer.nb(PT_taken~
                         Forager.age+
                         Tiny_EorN+
                         Half+
                         Treated.leaf+
                         Forager.age*Prop_For_Exp+
                         offset(ln_LeavesTaken)+
                         (1| Colony), data=data.n,na.action=na.fail)
summary(mod.naive1)

emmeans(mod.naive1, "Forager.age", type="response")
emmeans(mod.naive1, "Prop_For_Exp",type="response", cov.reduce = range)emmeans(mod.naive1, "Treated.leaf", type="response")
emmeans(mod.naive1, "Forager.age", by="Prop_For_Exp",type="response", cov.reduce = range)
emmeans(mod.naive1, "Tiny_EorN", type="response")
emmeans(mod.naive1, "Half", type="response")





#########Figures 3-6

#Define functions to put sample sizes on plots
StatN <- ggproto("StatN", Stat,
                 required_aes = c("x", "y"), 
                 compute_group = function(data, scales) {
                   y <- data$y
                   y <- y[!is.na(y)]
                   n <- length(y)
                   data.frame(x = data$x[1], y = -0.1, label = n)
                 }
)

stat_n <- function(mapping = NULL, data = NULL, geom = "text", 
                   position = "identity", inherit.aes = TRUE, show.legend = NA, 
                   na.rm = FALSE, ...) {
  ggplot2::layer(stat = StatN, mapping = mapping, data = data, geom = geom, 
                 position = position, inherit.aes = inherit.aes, show.legend = show.legend, 
                 params = list(na.rm = na.rm, ...))
}



## Figure 3
ggplot(PropFor_Half1, aes(x=Forager.type, y= PT_taken/Leaves_taken))+
  geom_boxplot()+
  geom_point()+
  theme_bw(base_size=20)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Type of Forager")+
  ylab("Leaf Choice")+
  stat_n()


## Figure 4
ggplot(PropFor_Half1, aes(x=Tiny_EorN, y= PT_taken/Leaves_taken))+
  geom_boxplot()+
  geom_point()+
  facet_grid(~For_EorN)+
  theme_bw(base_size=20)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Tiny Ant Experience")+
  ylab("Leaf Choice")

# Figure 5
ggplot(PropFor_Half1, aes(x=Prop_For_Exp, y= PT_taken/Leaves_taken))+
  geom_smooth(method='lm',formula=y~x, se =FALSE, size=3, colour="black")+
  geom_point()+
  theme_bw(base_size=15)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  facet_grid(Forager.age~For_EorN)+
  xlab("Proportion of Work Done by Experienced Ants")+
  ylab("Leaf Choices of Old, Experienced Ants")+
  ylim(0,1.0)+
  xlim(0,1.0)

#Figure 6
ggplot(PropFor_Half1, aes(x=Prop_For_Exp, y= PT_taken/Leaves_taken))+
  geom_smooth(method='lm',formula=y~x, se =FALSE, size=3, colour="black")+
  geom_point()+
  theme_bw(base_size=15)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  #facet_grid(Forager.age~For_EorN)+
  xlab("Proportion of Work Done by Experienced Ants")+
  ylab("Leaf Choice")+
  ylim(0,1.0)+
  xlim(0,1.0)


ggplot(PropFor_Half1, aes(x=For_EorN, y= PT_taken/Leaves_taken))+
  geom_point()+
  geom_boxplot()+
  facet_wrap(~Treated.leaf)+
  xlab("Experienced or Naive to Fungicide Treatment")+
  ylab("Leaf Choice")+
  stat_n()




