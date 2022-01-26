# full tuned model
library(tidyverse)

# Functions ----
plot_d <- function(damaged_path) {
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
  plot(x_sens,y_sens,asp =1,pch = 19,
       xlab="x [m]",
       ylab="y [m]")
  ordering=c(47:42,25,24,22,21,49,48,20)  # Vector that describe the sensors order
  
  pplotDistance <- function() {
    # damaged_path <- df_predict %>% arrange(yhat) %>%  filter(yhat<dlim)
    x=c(-R,R)
    for (i in 1:nrow(damaged_path)){
      b=damaged_path$b_path[i]
      a=damaged_path$a_path[i]
      d=damaged_path$yhat[i]*(2*R)
      if (is.finite(a)) { 
        lines(x,a*x+ b )
        # lines(x,a*x+ b + d,type = "l",lty=2)
        # lines(x,a*x+ b - d,type = "l",lty=2)
      }else{
        
        lines(c(b,b),x)
        #   lines(c(b+d,b+d),x,type = "l",lty=2)
        #   lines(c(b-d,b-d),x,type = "l",lty=2)
      }
      
    }
    points(xd,yd, col="red",pch = 19)
  }
  
  
  pplotDistance()
  
  
  # calc damage position
  N=nrow(damaged_path)
  x=matrix(NaN,N,N)
  colnames(x)=paste(damaged_path$Actuator, "-",damaged_path$Sensor)
  rownames(x)=paste(damaged_path$Actuator, "-",damaged_path$Sensor)
  
  y=matrix(NaN,N,N)
  colnames(y)=paste(damaged_path$Actuator, "-",damaged_path$Sensor)
  rownames(y)=paste(damaged_path$Actuator, "-",damaged_path$Sensor)
  
  w=matrix(NaN,N,N)
  colnames(w)=paste(damaged_path$Actuator, "-",damaged_path$Sensor)
  rownames(w)=paste(damaged_path$Actuator, "-",damaged_path$Sensor)
  
  for (i in 1:N){
    path_i=damaged_path[i,]
    
    a_i=path_i$a_path
    b_i=path_i$b_path
    d_i=path_i$yhat
    
    for (j in 1:N ){
      
      if(i!=j){
        
        path_j=damaged_path[j,]
        
        a_j=path_j$a_path
        b_j=path_j$b_path
        
        if((a_i != a_j) & is.finite(a_i) & is.finite(a_j) ){
          x[i,j]= (b_j-b_i)/(a_i-a_j)
          y[i,j]=a_j*x[i,j] + b_j
          
        }else if( !is.finite(a_i) & is.finite(a_j)){
          
          x[i,j]=b_i
          y[i,j]=a_j*x[i,j] + b_j
          
        }else if(is.finite(a_i) & !is.finite(a_j)){
          x[i,j]=b_j
          y[i,j]=a_i*x[i,j] + b_i
          
        }
        
        d_j=path_j$yhat
        
        w[i,j]= 2/(d_i + d_j)
        #w[i,j]= 1/sqrt(d_i^2 + d_j^2)
      }
    }
  }
  
  for (i in 1:N) {
    
    for (j in 1:N) {
      
      if(!(x[i,j]^2 + y[i,j]^2 < (0.99*R)^2) & (!is.nan(x[i,j]) || !is.nan(y[i,j]))){
        x[i,j]=NaN
        y[i,j]=NaN
      }
    }
  }
  
  # intersection position vector
  
  x_vec=x[upper.tri(x) & !is.nan(x)]
  y_vec=y[upper.tri(y) & !is.nan(y)]
  w_vec=w[upper.tri(x) & !is.nan(x)]
  
  # contour(x_vec,y_vec,w_vec)
  
  points(x_vec,y_vec, col="green")
  
  #normal mean
  # x_g1=mean(x_vec)
  # y_g1=mean(y_vec)
  # points(x_g1,y_g1,col="azure4",pch = 19)
  
  #weighted intersection
  xw=x*w
  yw=y*w
  x_vec=xw[upper.tri(xw) & !is.nan(xw)]
  y_vec=yw[upper.tri(yw) & !is.nan(yw)]
  
  x_g=sum(x_vec)/(sum(w_vec))
  y_g=sum(y_vec)/(sum(w_vec))
  
  points(x_g,y_g,col="blue",pch = 19)
  
  #legend
  legend(-0.33,0.12,legend=c("real damage", "damage estimate"),
         col=c("red", "blue"),pch=19, cex=0.8)
  
  # %position difference
  xd=81e-3
  yd=40e-3
  
  x_p=round(abs((x_g-xd)/(2*R)) * 100,digits = 2)
  y_p=round(abs((y_g-yd)/(2*R)) * 100,digits = 2)
  labels=paste("% error\n","x = ", toString(x_p),"%\ny = ",toString(y_p),"%")
  text(-0.3,-0.1,labels = labels)
}

tunemodel <- function(formula,df_temp) {
  Radj=0
  Radj_new=1
  iter=0
  while(Radj_new>Radj ){
    
    mod_q=lm(formula, data=df_temp)
    
    Radj=summary(mod_q)$adj.r.squared

    df_temp_q<-df_temp
    
    ##L2 norm
    #df_temp<- df_temp %>% mutate(minimize= sqrt((Distance/max(Distance))^2 + (AreaDiff/max(AreaDiff))^2) )
    
    ##L1 norm
    df_temp<- df_temp %>% mutate(minimize= Distance/max(Distance) + AreaDiff/max(AreaDiff) )
    
    # censor observartion
    df_temp = df_temp %>% arrange(minimize) %>% slice(-1)
    
    mod_q2=lm(formula, data=df_temp)
    
    Radj_new=summary(mod_q2)$adj.r.squared
    
    iter=iter+1
    
    #control
    print(paste("iter=",toString(iter)," Radj=",toString(Radj)))
    
  }
  
  return(mod_q)
}


# script ----

#read dataframe
R=150e-3
df<-read_csv("completeDf.csv")
df_mod<-df %>% filter(Actuator!=20,Sensor!=20, AreaDiff > 0)
df_mod <- df_mod %>% mutate(Distance= Distance/(2*R) )
df_mod <- df_mod %>% filter(!(Actuator==21 & Sensor==49))

#results
for (idx in 1:10) {
  
  df.train<-df_mod %>% filter(Index!=idx)
  df.test<-df_mod %>% filter(Index==idx)
  
  formula=formula=formula(Distance ~ AreaDiff + I(AreaDiff^2))
  mod= tunemodel(formula,df.train)
  
  y_hat= predict(mod,newdata = df.test)
  
  df.test= df.test %>% mutate(yhat=y_hat)

  dlim=3*min(df.test$yhat)
  df.test <- df.test %>% arrange(yhat) %>%  filter(yhat<dlim)
  
  plot_d(df.test)
}
