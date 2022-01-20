library(tidyverse)

df<-read_csv("completeDf.csv")

df_mod<-df %>% filter(Actuator!=20,Sensor!=20,Index==2, AreaDiff > 0)


df_mod <- df_mod %>% filter(!(Actuator==21 & Sensor==49))

df_mod<-df_mod %>% mutate(logAdiff=log(AreaDiff), logD=log(Distance))



# mod normale quadratico
mod_q=lm(Distance ~ AreaDiff + I(AreaDiff^2), data=df_mod)
summary(mod_q)
plot(mod_q)
df_mod<-df_mod%>% mutate(yhat=predict(mod_q))

df_mod %>%  ggplot()+
  geom_point(aes(y=Distance, x= AreaDiff, color=as.factor(Sensor)))+
  geom_smooth(aes(y=Distance, x= AreaDiff))+
  geom_line(aes(x=AreaDiff, y= yhat), color="red")


# inv distance
df_mod<-df_mod %>% mutate(invD=1/Distance)

mod_i=lm(invD~ AreaDiff, data=df_mod[-c(4,40),])
summary(mod_i) 
plot(mod_i)
df_mod<-df_mod[-c(4,40),]%>% mutate(yhat=predict(mod_i))

df_mod[-c(4,40),] %>%  ggplot()+
  geom_point(aes(y=invD, x= AreaDiff, color=as.factor(Sensor)))+
  geom_smooth(aes(y=invD, x= AreaDiff),method="lm")+
  geom_line(aes(x=AreaDiff, y= yhat), color="red")



# quadratic log model
mod= lm(Distance~ logAdiff + I(logAdiff^2), data = df_mod[-c(2,4,40),])
summary(mod)
plot(mod)

df_mod<-df_mod[-c(2,4,40),] %>% mutate(yhat=predict(mod))


#df %>% filter(Actuator==21,Sensor==20,Index==1)
  

#df_mod %>% ggplot()+
#  geom_point(aes(x=Distance, y= AreaDiff, color=as.factor(Sensor)))+
#  facet_wrap(~as.factor(Actuator))

df_mod_2<-df %>% filter(Actuator!=20,Sensor!=20,Index==3, AreaDiff > 0)

df_mod[-c(2,4,40),] %>%  ggplot()+
  geom_point(aes(y=Distance, x= logAdiff, color=as.factor(Sensor)))+
  geom_smooth(aes(y=Distance, x= logAdiff))+
  geom_line(aes(x=logAdiff, y= yhat), color="red")


# sigmoid model
df_mod <- df_mod %>%  mutate(Asigmoid=-AreaDiff/(1 -AreaDiff))

mod_sigmod<- lm(Distance ~ Asigmoid + I(Asigmoid^2) , data = df_mod )
summary(mod_sigmod)
plot(mod_sigmod)

df_mod<-df_mod %>% mutate(yhat=predict(mod_sigmod))

#plot
df_mod %>%  ggplot()+
  geom_point(aes(y=Distance, x= Asigmoid, color=as.factor(Sensor)))+
  geom_smooth(aes(y=Distance, x= Asigmoid))+
  geom_line(aes(x=Asigmoid, y= yhat), color="red")
