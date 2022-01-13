library("tidyverse")

#relative path from the location of this script to the file A20_pre_1.csv
filename= "AG2_ramp/DATI/A20 PRE/A20_pre_1.csv"
sens20_pre <- read_csv(filename)

filename= "AG2_ramp/DATI/A20 POST/A20_post_1.csv"
sens20_post <- read_csv(filename)


#rename columns
names= c("times","a20","r21","r22","r24","r25","r42","r43","r44","r45","r46","r47","r48","r49")
colnames(sens20_pre) <- names
colnames(sens20_post) <- names



#ATTUATORE 20 PRE, OSSERVO CHE I RICEVITORI RICEVONO ALLO STESSO TEMPO (PERCHE' IL 20 è AL CENTRO)
ggplot()+
  geom_line(data= sens20_pre, mapping = aes(x=times, y= a20)) +  #segnale inviato da 20 prima della delaminazione
  #Segnali che ricevono gli altri 12 sensori
  geom_line(data= sens20_pre, mapping = aes(x=times, y= r21), color='red')+
  geom_line(data= sens20_pre, mapping = aes(x=times, y= r22), color= "green" )+
  geom_line(data= sens20_pre, mapping = aes(x=times, y= r24), color='blue')+
  geom_line(data= sens20_pre, mapping = aes(x=times, y= r25), color= "azure4" )+  
  geom_line(data= sens20_pre, mapping = aes(x=times, y= r42), color='bisque')+
  geom_line(data= sens20_pre, mapping = aes(x=times, y= r43), color= "black" )+
  geom_line(data= sens20_pre, mapping = aes(x=times, y= r44), color='blueviolet')+
  geom_line(data= sens20_pre, mapping = aes(x=times, y= r45), color= "brown" ) +
  geom_line(data= sens20_pre, mapping = aes(x=times, y= r46), color='brown1')+
  geom_line(data= sens20_pre, mapping = aes(x=times, y= r47), color= "darkgreen" )+
  geom_line(data= sens20_pre, mapping = aes(x=times, y= r48), color='darkred')+
  geom_line(data= sens20_pre, mapping = aes(x=times, y= r49), color= "darkslateblue")+
  ylim(c(-0.1,0.1))
#Notiamo che il segnale dell'attuatore è diverso da 0 quando non c'è nessuna  


#visualizzo i segnali dell'attuatore prima e dopo la delaminazione, sono esattamente sovrapposti come è normale che sia
ggplot() +  
  geom_line(data= sens20_pre, mapping = aes(x=times, y= a20),color='green') +  
  geom_line(data = sens20_post, mapping = aes (x=times, y= a20),color='red')  

#ATTUATORE 20 POST
ggplot()+
  geom_line(data= sens20_post, mapping = aes(x=times, y= a20)) +  #segnale inviato da 20 prima della delaminazione
  #Segnali che ricevono gli altri 12 sensori
  geom_line(data= sens20_post, mapping = aes(x=times, y= r21), color='red')+
  geom_line(data= sens20_post, mapping = aes(x=times, y= r22), color= "green" )+
  geom_line(data= sens20_post, mapping = aes(x=times, y= r24), color='blue')+
  geom_line(data= sens20_post, mapping = aes(x=times, y= r25), color= "azure4" )+  
  geom_line(data= sens20_post, mapping = aes(x=times, y= r42), color='bisque')+
  geom_line(data= sens20_post, mapping = aes(x=times, y= r43), color= "black" )+
  geom_line(data= sens20_post, mapping = aes(x=times, y= r44), color='blueviolet')+
  geom_line(data= sens20_post, mapping = aes(x=times, y= r45), color= "brown" ) +
  geom_line(data= sens20_post, mapping = aes(x=times, y= r46), color='brown1')+
  geom_line(data= sens20_post, mapping = aes(x=times, y= r47), color= "darkgreen" )+
  geom_line(data= sens20_post, mapping = aes(x=times, y= r48), color='darkred')+
  geom_line(data = sens20_post, mapping = aes(x=times, y= r49), color= "darkslateblue" )+ 
  #Quest'ultimo (r49) è come se fosse traslato verso il basso di una certa quantità.Poichè abbiamo 10 campioni disponibili
  #usando dei metodi di clustering dovremmo arrivare alla conclusione che questo data è un outliers
  ylim(c(-0.1,0.1))

