
library(tidyverse)
df<-read_csv("completeDf.csv")

df_mod<-df %>% filter(Actuator!=20,Sensor!=20,Index==2, AreaDiff > 0)


df_mod <- df_mod %>% mutate(Distance= Distance/(2*R) )

df_mod <- df_mod %>% filter(!(Actuator==21 & Sensor==49))


#mod quadratico
mod_q=lm(Distance ~ AreaDiff + I(AreaDiff^2), data=df_mod)
summary(mod_q)

#plot(mod_q)


#plot model vs geom_smooth
df_mod %>%  ggplot()+
  geom_point(aes(y=Distance, x= AreaDiff, color=as.factor(Sensor)))+
  geom_smooth(aes(y=Distance, x= AreaDiff))+
  geom_line(aes(x=AreaDiff, y= predict(mod_q)), color="red")


#residui su dati diversi
df_predict<- df %>%
  mutate(Distance= Distance/(2*R) ) %>% 
  filter(Actuator!=20,Sensor!=20,Index==7, AreaDiff > 0)

y_hat= predict(mod_q,newdata = df_predict)

df_predict= df_predict %>% mutate(yhat=y_hat)

z_hat= df_predict$Distance - y_hat

plot(y_hat,z_hat)

dlim=1.3*min(df_predict$yhat)

damaged_path <- df_predict %>% arrange(yhat) %>%  filter(yhat<dlim)

D_mean <- mean(df_mod$Distance)


###-------PLOT PATH------

pplotDistance <- function() {
  

damaged_path <- df_predict %>% arrange(yhat) %>%  filter(yhat<dlim)
## DATA ------------------------------------------------------------------------
R=150*10^-3;  # radius of the sensor circle [m]
dalpha=30;     # angular distance between sensors [deg]
xd=81*10^-3;  # x coordinate of the damage center [m]
yd=40*10^-3;  # y coordinate of the damage center [m]
sx=26*10^-3;  # x-span of the damage [m]
sy=25*10^-3;  # y-span of the damage [m]

## Compute positions coordinates of the sensor ---------------------------------
alpha=seq(from=0,to=330,by=dalpha)
alpha=alpha*pi/180      # angular coordinates of the sensors [rad]
x_sens=R*cos(alpha)     # x coordinates of the sensors [m]
y_sens=R*sin(alpha)     # y coordinates of the sensors [m]
x_sens=c(x_sens,0)
y_sens=c(y_sens,0)
plot(x_sens,y_sens,asp =1)
ordering=c(47:42,25,24,22,21,49,48,20)  # Vector that describe the sensors order

x=c(-R,R)
for (i in 1:nrow(damaged_path)){
  b=damaged_path$b_path[i]
  a=damaged_path$a_path[i]
  d=damaged_path$yhat[i]*(2*R)
  if (is.finite(a)) { 
    lines(x,a*x+ b )
    lines(x,a*x+ b + d,type = "l",lty=2)
    lines(x,a*x+ b - d,type = "l",lty=2)
  }else{
    
    lines(c(b,b),x)
    lines(c(b+d,b+d),x,type = "l",lty=2)
    lines(c(b-d,b-d),x,type = "l",lty=2)
  }
  
}
points(xd,yd, col="red")
}


pplotDistance()
