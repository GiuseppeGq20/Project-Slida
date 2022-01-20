
library(tidyverse)
df<-read_csv("completeDf.csv")

df_mod<-df %>% filter(Actuator!=20,Sensor!=20,Index==2, AreaDiff > 0)

df_mod <- df_mod %>% filter(!(Actuator==21 & Sensor==49))

df_predict<- df %>% filter(Actuator!=20,Sensor!=20,Index==7, AreaDiff > 0)

#mod quadratico
mod_q=lm(Distance ~ AreaDiff + I(AreaDiff^2), data=df_mod)
summary(mod_q)

#residui
y_hat= predict(mod_q,newdata = df_predict)

df_predict= df_predict %>% mutate(yhat=y_hat)

z_hat= df_predict$Distance - y_hat

plot(z_hat)

df_predict %>% arrange(yhat) %>% head(10)
