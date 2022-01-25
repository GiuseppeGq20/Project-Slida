
#tune Model

library(tidyverse)
library(latex2exp)
df<-read_csv("completeDf.csv")

R=150e-3

df_mod<-df %>% filter(Actuator!=20,Sensor!=20, AreaDiff > 0)
df_mod <- df_mod %>% mutate(Distance= Distance/(2*R) )
df_mod <- df_mod %>% filter(!(Actuator==21 & Sensor==49))



#adj mod
Radj=0
Radj_new=1
iter=0
df_temp=df_mod

# removed_point=data.frame(matrix(ncol = ncol(df_mod), nrow = 0))
removed_point=0
# colnames(removed_point)<-names(df_mod)

while(Radj_new>Radj ){
  
  mod_q=lm(Distance ~ AreaDiff + I(AreaDiff^2), data=df_temp)
  
  Radj=summary(mod_q)$adj.r.squared
  
  #store removed points
  temp<-df_temp %>% arrange(Distance,AreaDiff) %>% slice(1)
   
  removed_point<- add_row(temp)
  
  df_temp_q<-df_temp
  #df_temp<- df_temp %>% mutate(minimize= sqrt((Distance/max(Distance))^2 + (AreaDiff/max(AreaDiff))^2) )
  df_temp<- df_temp %>% mutate(minimize= Distance/max(Distance) + AreaDiff/max(AreaDiff) )
  df_temp = df_temp %>% arrange(minimize) %>% slice(-1)
  
  mod_q2=lm(Distance ~ AreaDiff + I(AreaDiff^2), data=df_temp)
  
  Radj_new=summary(mod_q2)$adj.r.squared
  
  iter=iter+1
  
  #control
  print(paste("iter=",toString(iter)," Radj=",toString(Radj)))
  
}
# plot(mod_q)

#plot model vs geom_smooth
df_mod %>%  ggplot()+
  geom_point(aes(y=Distance, x= AreaDiff, color=as.factor(Sensor)))+
  geom_smooth(aes(y=Distance, x= AreaDiff))+
  geom_line(data=df_temp_q, mapping=aes(x=AreaDiff, y= predict(mod_q)), color="red")



#polynomial model
df<-read_csv("completeDf.csv")
R=150e-3
df_mod<-df %>% filter(Actuator!=20,Sensor!=20, AreaDiff > 0)
df_mod <- df_mod %>% mutate(Distance= Distance/(2*R) )
df_mod <- df_mod %>% filter(!(Actuator==21 & Sensor==49))

#mod quadratico
mod_q1=lm(Distance ~ AreaDiff + I(AreaDiff^2), data=df_mod)
summary(mod_q1)

#linear model
mod_l=lm(Distance ~ AreaDiff, data=df_mod)
summary(mod_l)
#comparison model

plot<-df_mod %>%  ggplot()+
  geom_point(aes(y=Distance, x= AreaDiff, color=as.factor(Sensor)))+
  geom_smooth(aes(y=Distance, x= AreaDiff))+
  geom_line(data=df_temp_q, mapping=aes(x=AreaDiff, y= predict(mod_q)), color="red")+
  geom_line(data=df_mod, mapping=aes(x=AreaDiff, y= predict(mod_q1)), color="green")
plot +  labs(color="Sensors",x=TeX("$\\Delta A$"),y=TeX("$\\frac{d}{R}$"))


# plots for the presentation

labels=data.frame(labels=c("geom_smooth", "full model", "censored Model"))
#just points
plot<-df_mod %>%  ggplot()+
  geom_point(aes(y=Distance, x= AreaDiff, color=as.factor(Sensor)))

plot +  labs(color="Sensors",x=TeX("$\\Delta A$"),y=TeX("$\\frac{d}{R}$"))

#points + geom_smooth
plot<-df_mod %>%  ggplot()+
  geom_point(aes(y=Distance, x= AreaDiff, color=as.factor(Sensor)))+
  geom_smooth(aes(y=Distance, x= AreaDiff))

plot +  labs(color="Sensors",x=TeX("$\\Delta A$"),y=TeX("$\\frac{d}{R}$"))

#points + geom_smooth + full mod
plot<-df_mod %>%  ggplot()+
  geom_point(aes(y=Distance, x= AreaDiff, color=as.factor(Sensor)))+
  geom_line(data=df_mod, mapping=aes(x=AreaDiff, y= predict(mod_q1)),color="green", size=1.5)

plot +  labs(color="Sensors",x=TeX("$\\Delta A$"),y=TeX("$\\frac{d}{R}$"))

#points + geom_smooth + full mod + mod_adj
plot<-df_mod %>%  ggplot()+
  geom_point(aes(y=Distance, x= AreaDiff, color=as.factor(Sensor)))

plot<-plot+  
  geom_smooth(aes(y=Distance, x= AreaDiff, colour= "LOESS"))+
  geom_line(data=df_temp_q, mapping=aes(x=AreaDiff, y= predict(mod_q),colour="censored"))+
  geom_line(data=df_mod, mapping=aes(x=AreaDiff, y= predict(mod_q1), colour="full"))+
  scale_color_manual(name = "models", values = c("censored" = "red", "full" = "green", "LOESS"="blue"))

plot +  labs(color="Sensors",x=TeX("$\\Delta A$"),y=TeX("$\\frac{d}{R}$"))
