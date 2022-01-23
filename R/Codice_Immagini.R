#SET DATI AS WORKING DIRECTORY
library("tidyverse")

filename= "A21 PRE/A21_pre_1.csv"
sens21_pre <- read_csv(filename)

filename= "A21 POST/A21_post_1.csv"
sens21_post <- read_csv(filename)

names= c("times","a21","r20","r22","r24","r25","r42","r43","r44","r45","r46","r47","r48","r49")
colnames(sens21_pre) <- names
colnames(sens21_post) <- names

#####################################
# Grafico di un attuatore (meglio fare linee più spesse di quelle di default in modo tale
# che siano ben visibili anche se visualizzate con un proiettore)

ggplot()+
  geom_line(data= sens21_pre, mapping = aes(x=times, y= a21),lwd=0.8) +  
  labs(title = "Actuator Signal")+
  xlab("Time (s)")+
  ylab("Voltage (V)")






#Grafico del segnale inviato dall'attuatore e di come viene ricevuto da un sensore pre e post
#FACCIAMO NOTARE CHE I DATI SONO MOLTO CONFUSIONARI E SONO DA CONTROLLARE:
#BISOGNA PROCEDERE A RIMUOVERE IL BIAS, RIFASARE E FILTRARE E NORMALIZZATI

colors<-c("A21-S46 PRE" = "red", "A21-S46 POST" = "blue")

ggplot()+
  geom_line(data= sens21_post, mapping = aes(x=times, y= r46,color= "A21-S46 PRE"),lwd=0.8 )+
  geom_line(data= sens21_pre , mapping = aes(x=times, y= r46,color= "A21-S46 POST"),lwd=0.8 ) +
  # ylim(c(-0.02,0.02))+
  labs(x = "Time (s)", y = "Voltage (V)", color = "Legend",title= "Before the data cleaning") +
  scale_color_manual(values = colors)







