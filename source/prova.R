library("tidyverse")

#relative path from the location of this script to the file A20_pre_1.csv
filename= "./AG2_ramp/DATI/A20 PRE/A20_pre_1.csv"
sens20_pre <- read_csv(filename)

filename= "./AG2_ramp/DATI/A20 POST/A20_post_1.csv"
sens20_post <- read_csv(filename)

#rename columns
names= c("times","a20","r21","r22","r24","r25","r42","r43","r44","r45","r46","r47","r48","r49")
colnames(sens20_pre) <- names
colnames(sens20_post) <- names

sens20_post <- sens20_post %>% 
  filter(times>=0)
sens20_pre <-sens20_pre %>% 
  filter(times>=0)


ggplot()+
  geom_line(data= sens20_pre, mapping = aes(x=times, y= r21), color='red')+
  geom_line(data = sens20_post, mapping = aes (x=times, y= r21), color= "blue")+
  geom_line(data= sens20_pre, mapping = aes(x=times, y= a20))

