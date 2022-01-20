##      Compute Distance
# In this script we want to compute the distance from the damage by all paths.
# Reference system: 
# origin in Sensor 20; x direction is 20-47; y direction is 20-44

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
plot(x_sens,y_sens)
ordering=c(47:42,25,24,22,21,49,48,20)  # Vector that describe the sensors order

## Compute A,B and c -----------------------------------------------------------
# A :  a_ij is the slope of the path that connect the i^th sensor to the j^th.
# B :  b_ij is the intercept of the path i-j.
# C :  c_ij is the intercept of the line passing from the damage, parallel to
#           the path ij.
N_sens=length(x_sens)           # Number of sensors
A=matrix(0,N_sens,N_sens)       # initialize matrices A,B and C
B=A
C=A
for(i in 1:N_sens){
  plot(x_sens,y_sens)
  points(xd,yd,col="red")
  for(j in 1:N_sens){
    if(j!=i){
      A[i,j]= (y_sens[j]-y_sens[i])/(x_sens[j]-x_sens[i])
      
      if(abs(A[i,j])<10^12){ # non vertical line
        B[i,j]= y_sens[i] - A[i,j]*x_sens[i]
        C[i,j]= yd - A[i,j]*xd
        # path plot
        x=c(x_sens[i],x_sens[j])
        y=A[i,j]*x + B[i,j]
        lines(x,y)
        # paralell plot
        #lines(x,A[i,j]*x+C[i,j],col="red")
      }
      else{
        A[i,j]=sign(A[i,j])/0
        lines(c(x_sens[i],x_sens[j]),c(y_sens[i],y_sens[j]))
      }
    }
  }
}

##  Compute distances matrix D -------------------------------------------------
# D : d_ij is the distance between the path ij and the parallel from the damage
D=matrix(0,N_sens,N_sens)
for(i in 1:N_sens){
  for(j in 1:N_sens){
    if(j!=i){
      if(is.finite(A[i,j])){
        D[i,j]=abs(B[i,j]-C[i,j])*cos(atan2(y_sens[j]-y_sens[i],x_sens[j]-x_sens[i]))
      }
      else{
        D[i,j]=xd-x_sens[i]
      }
    }
    else{
      D[i,j]=NaN
    }
    
  }
}

## Control plots ---------------------------------------------------------------

for(i in 1:N_sens){
  for(j in 1:N_sens){
    if(j!=i){
      plot(x_sens,y_sens,asp=1)     # plot sensors
      points(xd,yd,col="red")       # plot damage center
      if(is.finite(A[i,j])){        # non vertical paths 
        x=c(-R,R)
        lines(x,A[i,j]*x+B[i,j])              # path plot
        lines(x,A[i,j]*x+C[i,j],col="red")    # parallel from damage plot
        alp=atan2(y_sens[j]-y_sens[i],x_sens[j]-x_sens[i])
        x=c(xd,xd + sign(C[i,j]-B[i,j])*D[i,j]*sin(alp))
        y=c(yd,yd - sign(C[i,j]-B[i,j])*D[i,j]*cos(alp))
        lines(x,y,col="green")                # distance plot
      }
      else{                         # vertical paths
        lines(c(x_sens[i],x_sens[i]),c(y_sens[i],y_sens[j]))  # path plot
        lines(c(xd,xd),c(y_sens[i],y_sens[j]))                # parallel plot
        lines(c(xd,xd-D[i,j]),c(yd,yd),col="green")           # distance plot
        
      }
      
    }
  }
}
## OUTPUT ----------------------------------------------------------------------
# Write Distances matrix D in a dataframe and export in a csv file

Response=matrix(0,10*factorial(N_sens)/factorial(N_sens-2))
Attuatore=Response
Sensore=Response
Index = Response
nomefile_pre=Response
nomefile_post=Response
idx=0
for (i in 1:N_sens){
  for(j in 1:N_sens){
    if(j!=i){
      for(k in 1:10){
      idx=idx+1
      Response[idx]=abs(D[i,j])
      Sensore[idx]=ordering[j]
      Attuatore[idx]=ordering[i]
      Index[idx]=k
      nomefile_pre[idx]=paste("A",toString(Attuatore[idx])," PRE/A",toString(Attuatore[idx]),"_pre_",toString(k),".csv",sep="")
      nomefile_post[idx]=paste("A",toString(Attuatore[idx])," POST/A",toString(Attuatore[idx]),"_post_",toString(k),".csv",sep="")
    }
    }
  }
}
df=data.frame(cbind(Attuatore,Sensore,Response,Index,nomefile_pre,nomefile_post))
names(df)=c("Actuator","Sensor","Distance","Index","File_pre","File_post")

library(tidyverse)
df=df %>% mutate(Actuator=as.factor(Actuator), Sensor= as.factor(Sensor),Distance=as.numeric(Distance),
                 Index= as.factor(Index),File_pre= as.factor(File_pre)
                 ,File_post= as.factor(File_post))

df %>% ggplot()+
  geom_density(mapping=aes(x=Distance,fill=Sensor))#+
#  facet_wrap(~ Actuator)

write.csv(df, file = "df_Distance.csv")







