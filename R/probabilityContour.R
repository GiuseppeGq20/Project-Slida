#BROKEN

library(tidyverse)

#load model
load("mod.Rdata")

#load df
df<-read_csv("completeDf.csv")
df_mod<-df %>% filter(Actuator!=20,Sensor!=20,Index==2, AreaDiff > 0)
df_mod <- df_mod %>% filter(!(Actuator==21 & Sensor==49))

y.predict <-predict(mod_q,newdata = df_mod, se=TRUE)
dof=y.predict$df

y.predict<- data.frame(fit=y.predict$fit, se=y.predict$se.fit)


#function to calculate probability
probabilty <- function(y0,y.predict,dof,mod_q) {
  # y0 is distance vector from all path to a defined point
  
  logp = sum(dt((y0-y.predict$fit)/y.predict$se,df= dof, log=TRUE))
  
  return( exp(logp))
}



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
      c=y0 - a*x0
      # D[i]=abs(b-c)*cos(atan2(y_sens[j]-y_sens[i],x_sens[j]-x_sens[i]))
      D[i]=abs(-a*x0 + y0 - b)/(sqrt(a^2 + 1))
    }
    else{
      D[i]=abs(b-x0)
    }
  }
  
  return(D)
}
 
# d= distance(df_mod = df_mod, x0=81e-3, y0=40e-3)


# define meshgrid
R=150e-3
N=5
x=seq(from=-R,to=R,length.out=N)
y=x

plot(0,0,
     xlim = c(-R,R),
     ylim = c(-R,R))
prob=matrix(0,N,N)

Z=model.matrix(mod_q)
# inflation= t(df_mod$AreaDiff, df_mod$AreaDiff^2) %*% (t(Z) %*%Z) %*% df_mod$AreaDiff
sigma_hat= crossprod(mod_q$residuals)/mod_q$df.residual

for (i in 1:N){
  
  for (j in 1:N) {
    
    d=distance(df_mod = df_mod, x0=x[i],y0=y[j])
    logp = sum(dt((d/R -y.predict$fit)/sqrt(y.predict$se^2 + as.vector(sigma_hat)), df= dof, log=TRUE))
    print(exp(logp))
    # prob[i,j]=probabilty(y0=d,y.predict,dof,mod_q)  
  }
    # print(paste("Complete : ",toString(i/(N)*100)," % "))
}


contour(x,y,prob)
