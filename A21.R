library("tidyverse")

filename= "AG2_ramp/DATI/A21 PRE/A21_pre_1.csv"
sens21_pre <- read_csv(filename)

filename= "AG2_ramp/DATI/A21 POST/A21_post_1.csv"
sens21_post <- read_csv(filename)

names= c("times","a21","r20","r22","r24","r25","r42","r43","r44","r45","r46","r47","r48","r49")
colnames(sens21_pre) <- names
colnames(sens21_post) <- names



#ATTUATORE 21 PRE

#Perche all'attuatore viene data una corrente a forma "sinusoidale" e non a gradino? 

ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= a21)) +  
  #Segnali che ricevono gli altri 12 sensori
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

# osservo che i ricevitori vicini al mio 21 (ossia 22 e 49) presentano un'onda piu ampia in quanto le onde meccaniche
# arrivano al ricevitore senza dispersione e ovviamente anche ad un tempo inferiore in quanto percorrerono meno spazio
# QUINDI SE PROVO A FARE SOLO LA DIFFERENZA PRE E POST, OSSERVERO' CHE I PERCORSI DANNEGGIATI MA LONTANI PRESENTERANNO
# UNA DIFFERENZA MINORE QUINDI DOVRO' STANDARDIZZARE I MIEI DATI (SOTTRAENDO LA MEDIA (0) E DIVIDENDO PER LA VARIANZA)
# Per quale Varianza?? Quella campionaria di A21_1 o una media delle varianze dei 10 campioni A21_1 A21_2 ... A21_10?


#ATTUATORE 21 POST

ggplot()+
  geom_line(data= sens21_post, mapping = aes(x=times, y= a21)) + 
  ylim(c(-0.15,0))
  #Segnali che ricevono gli altri 12 sensori
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


#confronto due percorsi SENZA delaminazione 21-20 21-43 pre e post ggplot()+

ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r20), color='red')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r43), color= "green" ) + 
  geom_line(data= sens21_post, mapping = aes(x=times, y= r20), color='blue')+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r43), color= "yellow" )

# IL RICEVITORE 20 è FUORI FASE, SE ANDASSI A CALCOLARE LA DIFFERENZA VERREBBE ENORME ANCHE SE LUNGO QUEL PERCORSO NON
# SONO PRESENTI DELAMINAZIONI. COME RISOLVERE ???
# SE VOGLIAMO FARE LA DIFFERENZA LA PRIMA COSA DA FARE è METTERLE IL FASE SHIFTANDO UNA DELLE DUE DI UNA QUANTITA' NOTA

#confronto due percorsi CON delaminazione 21-45 21-46 pre e post ggplot()+

ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r45), color='red')+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= r46), color= "green" ) + 
  geom_line(data= sens21_post, mapping = aes(x=times, y= r45), color='blue')+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r46), color= "yellow" )


