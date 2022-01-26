library("tidyverse")

#for this script:
#Set DATI as Working Directory

filename= "A21 PRE/A21_pre_1.csv"
sens21_pre <- read.csv(filename)

filename= "A21 POST/A21_post_1.csv"
sens21_post <- read.csv(filename)

names= c("times","a21","r20","r22","r24","r25","r42","r43","r44","r45","r46","r47","r48","r49")
colnames(sens21_pre) <- names
colnames(sens21_post) <- names



#Visualizza dati 
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= a21)) +  
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r20), color='red')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r22), color= "green" )+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r24), color='blue')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r25), color= "azure4" )+  
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r42), color='bisque')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r43), color= "black" )+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r44), color='blueviolet')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r45), color= "brown" ) +
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r46), color='brown1')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r47), color= "darkgreen" )+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r48), color='darkred')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r49), color= "darkslateblue")+
  ylim(c(-0.04,0.04))



# Shift
for (i in 3:14) {
sens21_pre[,i] <- sens21_pre[,i] - mean(as.matrix(sens21_pre[sens21_pre$times<0,i]))
sens21_post[,i] <- sens21_post[,i] - mean(as.matrix(sens21_post[sens21_post$times<0,i]))
}

#Visualizza dati dopo lo Shift 
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= a21)) +  
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r20), color='red')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r22), color= "green" )+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r24), color='blue')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r25), color= "azure4" )+  
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r42), color='bisque')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r43), color= "black" )+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r44), color='blueviolet')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r45), color= "brown" ) +
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r46), color='brown1')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r47), color= "darkgreen" )+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r48), color='darkred')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r49), color= "darkslateblue")+
  ylim(c(-0.04,0.04))


#Filter & Rephase

#Rephase (Trovare i percorsi sfasati tra pre e post e rifasarli)

for (i in 3:14) {
  prod <- sens21_pre[,i]*sens21_post[,i]
  if (sum(prod)<0) {
    sens21_post[,i]<- -sens21_post[,i]
    
  }
}

#Possiamo vedere in questo esempio un caso di percorso fuori fase (nel caso uso il primo dataframe)
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r46), color= "green" ) + 
  geom_line(data= sens21_post, mapping = aes(x=times, y= r46), color= "yellow" )

#Filter (Prendere in considerazione solo i 4 seni e mezzo che provengono dall'attenuatore)



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

#Prova per vedere se la funzione funziona

A<- sens21_pre[,3]^2 #La terza colonna di sens21_pre (quella inerente a r20), ne faccio il quadrato
k_max_rel(A,3)   #Trovo gli indici relativi ai 3 massimi

#FUNZIONA !

#Filtro (In corrispondenza di intervalli maggiori di 2.25 seni a destra e 2.25 seni a sinistra pongo tutto 0)
FR<- 60000   #Frequenza 60KHz

for (i in 3:14) {
  id_tre_max<-  k_max_rel(sens21_pre[,i]^2,3)            #prende gli indici dei tre massimi
  id_max<-      sort(id_tre_max)[2]                      #questo ? l'indice del max centrale
  sens21_pre[abs(sens21_pre$times  -  sens21_pre$times[id_max])>2.25/FR,i]=0   #DOVE ? VERO PONGO TUTTO A ZERO
  sens21_post[abs(sens21_post$times  -  sens21_post$times[id_max])>2.25/FR,i]=0
}


#Prova con sens21_pre
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r20), color='red')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r22), color= "green" )+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r24), color='blue')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r25), color= "azure4" )+  
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r42), color='bisque')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r43), color= "black" )+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r44), color='blueviolet')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r45), color= "brown" ) +
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r46), color='brown1')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r47), color= "darkgreen" )+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r48), color='darkred')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r49), color= "darkslateblue")+
  ylim(c(-0.04,0.04))

#Prova con sens21_post
ggplot()+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r20), color='red')+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r22), color= "green" )+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r24), color='blue')+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r25), color= "azure4" )+  
  geom_line(data= sens21_post, mapping = aes(x=times, y= r42), color='bisque')+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r43), color= "black" )+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r44), color='blueviolet')+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r45), color= "brown" ) +
  geom_line(data= sens21_post, mapping = aes(x=times, y= r46), color='brown1')+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r47), color= "darkgreen" )+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r48), color='darkred')+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r49), color= "darkslateblue")+
  ylim(c(-0.04,0.04))


#Vedo graficamente alcuni percorsi come cambiano

#Il percorso 21-45 ? VICINO al danno
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r45), color='red')+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r45), color='blue')

#Il percorso 21-46 ? VICINO al danno
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r46), color= "red" ) + 
  geom_line(data= sens21_post, mapping = aes(x=times, y= r46), color= "blue" )
  
  
#Il percorso 21-20 ? LONTANO al danno
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r20), color= "red" ) + 
  geom_line(data= sens21_post, mapping = aes(x=times, y= r20), color= "blue" )

#Il percorso 21-22 ? LONTANO al danno
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r22), color='red')+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r22), color='blue')  

#Il percorso 21-24 ? LONTANO al danno
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r24), color= "red" ) + 
  geom_line(data= sens21_post, mapping = aes(x=times, y= r24), color= "blue" )

#Il percorso 21-25 ? LONTANO al danno
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r25), color='red')+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r25), color='blue')  

#Il percorso 21-42 ? LONTANO al danno
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r42), color='red')+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r42), color='blue')
#Il percorso 21-43 ? LONTANO al danno
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r43), color= "red" ) + 
  geom_line(data= sens21_post, mapping = aes(x=times, y= r43), color= "blue" )

#Il percorso 21-44 ? LONTANO al danno
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r44), color='red')+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r44), color='blue')  

#Il percorso 21-47 ? LONTANO al danno
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r47), color='red')+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r47), color='blue')

#Il percorso 21-48 ? LONTANO al danno
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r48), color= "red" ) + 
  geom_line(data= sens21_post, mapping = aes(x=times, y= r48), color= "blue" )

#Il percorso 21-49 ? LONTANO al danno
ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r49), color='red')+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r49), color='blue')  

#ATTENZIONE !!! VA TUTTO BENE, SOLO L'ULTIMO PERCORSO R49 DA PROBLEMI, ? SICURAMENTE UN OUTLIER DA ELIMINARE
#ATTENZIONE !!! IL PERCORSO R45 SEMBRA FUORI FASE !! 
#NB: LA COSA STRANA ? CHE QUESTI DUE PROBLEMI SI HANNO PER TUTTI E 10 I DATAFRAME DI SENS21



#Normalizziamo i segnali in modo tale da rimuovere l'effetto per cui ricevitori pi? lontani hanno
#ampiezze minori in modo tale da calcolare la differenza delle aree 

for (i in 3:14){
  A<-max(abs(sens21_pre[,i]))
  sens21_pre[,i]<- sens21_pre[,i]/A
  sens21_post[,i]<- sens21_post[,i]/A
}

#Dobbiamo scartare quelli che hanno un'area Post maggiore dell'area Pre in quanto non ? fisico




#Calcolo Area


# Eulero<- function(f,a,b){
#   
#   h=(b-a)/(length(f)-1);
#   I= sum(f*h)
#   return (I)
# }


Trapz<- function(f,x){
  n <- length(x)
  dx=x[-1]-x[-n] # calcola il vettore di spacing (altezze)
  df=f[-1]+f[-n] # calcola il vettore delle basi
  I= 0.5*sum(df*dx)
  return (I)
}

AreaPRE<- 0
AreaPOST<- 0

for (i in 3:14) {
AreaPRE[i-2] <- Trapz(sens21_pre[,i]^2,sens21_pre$times)
AreaPOST[i-2] <- Trapz(sens21_post[,i]^2,sens21_post$times)
}
#Facendo il quadrato non solo stiamo facendo tutto positivo, ma riduciamo la sensibilit? agli estremi

Delta <- AreaPRE-AreaPOST





