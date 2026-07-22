##run glm to assess relationships between biogeographic factors, recreation, and species richness of vascular plants
model1<- glm(alpinespprich ~ connectivity + logarea + elevation + thruhikedistance 
             +hikecompleted+ (1|transect), 
             data=FieldRarefaction, family= 'poisson')

##create correlation matrix to assess relationships between individual variables
new_names <- c("Alpine Species Richness", "Log Area", "Connectivity", "Distance to Thru Hiking Trail",
               "AllTrails Hikes Completed")
colnames(numeric_alpine) <- new_names
alpinecor <- cor(numeric_alpine)
print(alpinecor)
corrplot(alpinecor, method = "circle", type = "lower", diag = FALSE,
         tl.cex = .5, tl.srt = 45, addCoef.col = "black")

##run multiple linear regression to assess relationships between variables
model2 <- lm(cbind(alpinespprich, hikecompleted) ~ logarea + connectivity + 
               thruhikedistance)

##recreate figures
hikefigure<-ggplot(FieldRarefaction)+
  geom_jitter(aes(hikecompleted, alpinespprich), colour= "olivedrab4")+
  geom_smooth(aes(hikecompleted, alpinespprich), colour= "darkseagreen4",
              method = "lm", se=TRUE, fill = "#C6CDF7")+
  theme_classic()+
  xlab("AllTrails Hikes Completed")+
  ylab("Alpine Specialist Species Richness")
hikefigure

connectivityfigure <- ggplot(FieldRarefaction)+
  geom_jitter(aes(connectivity, alpinespprich), colour= "olivedrab4")+
  geom_smooth(aes(connectivity, alpinespprich), colour= "darkseagreen4",
              method = "lm", se=TRUE, fill = "#C6CDF7")+
  theme_classic()+
  xlab("Connectivity")+
  ylab("Alpine Specialist Species Richness")
connectivityfigure

areafigure<-ggplot(FieldRarefaction)+
  geom_jitter(aes(logarea, alpinespprich), colour= "olivedrab4")+
  geom_smooth(aes(logarea, alpinespprich), colour= "darkseagreen4",
              method = "lm", se=TRUE, fill = "#C6CDF7")+
  theme_classic()+
  xlab("Log Area")+
  ylab("Alpine Specialist Species Richness")
areafigure

elevationfigure<-ggplot(FieldRarefaction)+
  geom_jitter(aes(elevation, alpinespprich), colour= "olivedrab4")+
  geom_smooth(aes(elevation, alpinespprich), colour= "darkseagreen4",
              method = "lm", se=TRUE, fill = "#C6CDF7")+
  theme_classic()+
  xlab("Elevation")+
  ylab("Alpine Specialist Species Richness")
elevationfigure

thruhikefigure<-ggplot(FieldRarefaction)+
  geom_jitter(aes(thruhikedistance, alpinespprich), colour= "olivedrab4")+
  geom_smooth(aes(thruhikedistance, alpinespprich), colour= "darkseagreen4",
              method = "lm", se=TRUE, fill = "#C6CDF7")+
  theme_classic()+
  xlab("Distance to Nearest Thru-Hiking Trail (km)")+
  ylab("Alpine Specialist Species Richness")
thruhikefigure

layout <- "AAABBB
CCCDDD
#EEEE#"
connectivityfigure  + hikefigure  + 
  elevationfigure  + areafigure +thruhikefigure +
  plot_layout(axis_titles = "collect" , heights = 5, design = layout)

