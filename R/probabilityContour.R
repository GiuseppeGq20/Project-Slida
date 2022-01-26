
library(tidyverse)

R=150e-3
#load df
df<-read_csv("completeDf.csv")
df_mod<-df %>% filter(Actuator!=20,Sensor!=20,Index==8, AreaDiff > 0)
df_mod <- df_mod %>% filter(!(Actuator==21 & Sensor==49))
df_mod <- df_mod %>% mutate(Distance= Distance/(2*R) )

#tune model
tuneModel <- function(formula,df_mod) {
  
  #adj mod
  Radj=0
  Radj_new=1
  iter=0
  df_temp=df_mod
  
  while(Radj_new>Radj ){
    
    mod_q=lm(formula, data=df_temp)
    
    Radj=summary(mod_q)$adj.r.squared
    
    #store removed points
    temp<-df_temp %>% arrange(Distance,AreaDiff) %>% slice(1)
    
    removed_point<- add_row(temp)
    
    df_temp_q<-df_temp
    #df_temp<- df_temp %>% mutate(minimize= sqrt((Distance/max(Distance))^2 + (AreaDiff/max(AreaDiff))^2) )
    df_temp<- df_temp %>% mutate(minimize= Distance/max(Distance) + AreaDiff/max(AreaDiff) )
    df_temp = df_temp %>% arrange(minimize) %>% slice(-1)
    
    mod_q2=lm(formula, data=df_temp)
    
    Radj_new=summary(mod_q2)$adj.r.squared
    
    iter=iter+1
    
    #control
    print(paste("iter=",toString(iter)," Radj=",toString(Radj)))
    
  }
  
  return(mod_q)
}
formula=formula(Distance ~ AreaDiff + I(AreaDiff^2))
mod_q=tuneModel(formula,df_mod)

#mod_q<-lm(formula,data = df_mod)

#test df
df_predict<-df %>% filter(Actuator!=20,Sensor!=20,Index==2, AreaDiff > 0)
df_predict <- df_mod %>% filter(!(Actuator==21 & Sensor==49))
df_predict <- df_mod %>% mutate(Distance= Distance/(2*R) )

y.predict <- predict(mod_q,newdata = df_predict, se=TRUE,interval="predict")

dof=y.predict$df
residual.scale=y.predict$residual.scale

y.predict<- data.frame(fit=y.predict$fit[,1],
                       fit.lwr=y.predict$fit[,2],
                       fit.upr=y.predict$fit[,3],
                       x=df_mod$AreaDiff, se=y.predict$se.fit)

# integral
integr2 <- function(f,x,y) {
  
  Nx=length(x)
  Ny=length(y)
  DA=(x[2]-x[1])*(y[2]-y[1])
  I=0
  for(i in 1:(Nx-1) ){
    for(j in 1:(Ny-1)){
      I=I + DA*(f[i,j]+f[i,j+1]+f[i+1,j]+f[i+1,j+1])/4
      #print(I)
    }
  }
  return(I)
}

#calc distance
distance<- function(df_mod,x0,y0){
  
  ##  Compute distances matrix D -------------------------------------------------
  # D : d_ij is the distance between the path ij and the parallel from the damage
  n=nrow(df_mod)
  D=matrix(0,nrow = n)
  
  for (i in 1:n) {
    
    path_i=df_mod[i,]
    a=path_i$a_path
    b=path_i$b_path
    
    if(is.finite(a)){
      # D[i]=abs(b-c)*cos(atan2(y_sens[j]-y_sens[i],x_sens[j]-x_sens[i]))
      D[i]=abs(-a*x0 + y0 - b)/(sqrt(a^2 + 1))
    }
    else{
      D[i]=abs(b-x0)
    }
  }
  
  return(D)
}
 
#plot sensor arrary
plotPoints <- function() {
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
  points(x_sens,y_sens,asp =1,pch = 19,
         xlab="x [m]",
         ylab="y [m]")
  
}

d= distance(df_mod = df_mod, x0=81e-3, y0=40e-3)
#d/(2*R) - df_mod$Distance #comparison with damage distances

# define meshgrid
R=150e-3
N=50
x=seq(from=-R,to=R,length.out=N)
y=x
plot(0,0,
     xlim = c(-R,R),
     ylim = c(-R,R))
prob=matrix(0,N,N)

#log density probability matrix
for (i in 1:N){
  for (j in 1:N) {
    
    d=distance(df_mod = df_mod, x0=x[i],y0=y[j])/(2*R)
    
    #probability density calculation
    f_point=dt((d -y.predict$fit)/sqrt(y.predict$se^2 + residual.scale^2), df= dof, log=TRUE)
    
    logf = sum(f_point)
    prob[i,j]=logf

  }
  #progress bar 
  print(paste("Complete : ",toString(i/(N)*100)," % "))
}

prob=exp(prob)

I=integr2(prob,x,y)

prob=prob/I #normalization

#plotting

#normalized density contour
plot3D::contour2D(prob,x,y,
                  xlab="x [m]", ylab="y [m]",
                  asp=1)
plotPoints()
points(x=81e-3,y=40e-3, pch=19, col="red")

#normalized density image
plot3D::image2D(prob,x,y,
                xlab="x [m]", ylab="y [m]",
                asp=1,col=hcl.colors(100, "Oslo"))
plotPoints()
points(x=81e-3,y=40e-3, pch=19, col="red")

# draw box

max(prob)
idx=which(prob==max(prob), arr.ind = TRUE )
idx_x=idx[1]
idx_y=idx[2]

bound=0
p_box=0
alpha=0.05
while (p_box<1-alpha) {
  
bound=bound+1

range_x=(idx_x-bound):(idx_x+bound)
range_y=(idx_y-bound):(idx_y+bound)

p_box=integr2(prob[range_x,range_y], x[range_x],y[range_y])

}

rect(xleft = x[idx_x-bound],
     xright = x[idx_x+bound],
     ybottom = y[idx_y-bound],
     ytop = y[idx_y+bound],
     border = "black")

print(paste("box prediction level = " ,toString((p_box)*100 ),"%"))
title(main = paste("box prediction level = " ,toString(round((p_box)*100,digits = 2) ),"%"))
