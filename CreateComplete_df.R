library("tidyverse")

####################    FUNCTION      ##########################################
#-------------------------------------------------------------------------------
#Creazione di una funzione che permette di trovare i primi k estremi relativi
k_max_rel <- function(vector,k) {
  indici_vec<-order(vector,decreasing=TRUE)
  
  k_indici_max_rel <- indici_vec[1]          #indice del primo massimo
  ik <- 1
  j=1
  
  while (ik < k){
    j=j+1
    if (sum( indici_vec[1:j-1]==indici_vec[j]+1 )==0  & sum( indici_vec[1:j-1]==indici_vec[j]-1 )==0 ){
      ik=ik+1
      k_indici_max_rel[ik] <- indici_vec[j] 
    }
    
  }
  return(k_indici_max_rel)
}
#-------------------------------------------------------------------------------

Trapz<- function(f,x){
  n <- length(x)
  dx=x[-1]-x[-n] # calcola il vettore di spacing (altezze)
  df=f[-1]+f[-n] # calcola il vettore delle basi
  I= 0.5*sum(df*dx)
  return (I)
}
#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------
DArea <- function(df,nrow){
  # df    dataframe output of ComputeDistance.R
  # nrow  Number of the rows
  
  Na=df$Actuator[nrow]              # Number of Actuator
  Ns=df$Sensor[nrow]                # Number of Sensor
  Nk=df$Index[nrow]                 # Index form 1 to 10
  filename= df$File_pre[nrow]
  sens_pre <- read.csv(filename)
  
  filename= df$File_post[nrow]
  sens_post <- read.csv(filename)
  names=c("times")
  colnames(sens_pre) <- names
  colnames(sens_post) <- names
  
  # Choose columns
  vec_ord=c(20:22,24,25,42:49)
  ordering=0
  idx=0
  for(i in 1:length(vec_ord)){
    if(vec_ord[i]!=Na){
      idx=idx+1
      ordering[idx]=vec_ord[i]
    }
  }
  idx=c(1:12)
  idx_col=idx[ordering==Ns]+2
  
  if(is_empty(idx_col)){ stop("Na and Ns not compatible")}
  
  
  # Shift
  
  sens_pre[,idx_col] <- sens_pre[,idx_col] - mean(as.matrix(sens_pre[sens_pre$times<0,idx_col]))
  sens_post[,idx_col] <- sens_post[,idx_col] - mean(as.matrix(sens_post[sens_post$times<0,idx_col]))
  
  #Filter & Rephase
  
  #Rephase
  
  prod <- sens_pre[,idx_col]*sens_post[,idx_col]
  if (sum(prod)<0) {
    sens_post[,idx_col]<- -sens_post[,idx_col]
    
  }

  #Filter
  
  FR<- 60000   #Frequency 60KHz

  id_tre_max <- k_max_rel(sens_pre[,idx_col]^2,3)            #prende gli indici dei tre massimi
  id_max <-  sort(id_tre_max)[2]                      #questo è l'indice del max centrale
  sens_pre [abs(sens_pre$times  -  sens_pre$times[id_max])>2.25/FR,idx_col]=0   #DOVE è VERO PONGO TUTTO A ZERO
  sens_post[abs(sens_post$times -  sens_post$times[id_max])>2.25/FR,idx_col]=0

  #Normalizziamo i segnali in modo tale da rimuovere l'effetto per cui ricevitori più lontani hanno
  #ampiezze minori in modo tale da calcolare la differenza delle aree 
  
  A<-max(abs(sens_pre[,idx_col]))
  sens_pre[,idx_col]<- sens_pre[,idx_col]/A
  sens_post[,idx_col]<- sens_post[,idx_col]/A

  # Compute Area
  AreaPRE  <- Trapz(sens_pre[,idx_col]^2,sens_pre$times)
  AreaPOST <- Trapz(sens_post[,idx_col]^2,sens_post$times)
  Delta <- AreaPRE-AreaPOST
  return(Delta)
  
}
#-------------------------------------------------------------------------------
###############################  MAIN   ########################################

filename="df_Distance.csv"
df=read.csv(filename)
D_Area=0
for(i in 1:nrow(df)){
  D_Area[i] <- DArea(df,i)
  print(paste("Complete : ",toString(i/nrow(df)*100)," % "))
}

df <- df %>% 
  mutate(AreaDiff=D_Area)









