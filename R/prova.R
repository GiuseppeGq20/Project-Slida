library("tidyverse")

#relative path from the location of this script to the file A20_pre_1.csv
filename= "./AG2_ramp/DATI/A48 PRE/A48_pre_1.csv"
sens_pre <- read_csv(filename)

filename= "./AG2_ramp/DATI/A48 POST/A48_post_1.csv"
sens_post <- read_csv(filename)

#rename columns
#names= c("times","a20","r21","r22","r24","r25","r42","r43","r44","r45","r46","r47","r48","r49")
names= c("times","a48","r20","r21","r22","r24","r25","r42","r43","r44","r45","r46","r47","r49")
colnames(sens_pre) <- names
colnames(sens_post) <- names

sens_post <- sens_post %>% 
  filter(times>=0)
sens_pre <-sens_pre %>% 
  filter(times>=0)


ggplot()+
  geom_line(data= sens_pre, mapping = aes(x=times, y= r44), color='red')+
  geom_line(data = sens_post, mapping = aes (x=times, y= r44), color= "blue")+
  geom_line(data= sens_pre, mapping = aes(x=times, y= a48))

ggplot()+
  geom_line(data= sens_pre, mapping = aes(x=times, y= r42), color='red')+
  geom_line(data = sens_post, mapping = aes (x=times, y= r42), color= "blue")


#y= a*|onda_pre - onda_post| + a2*