library("tidyverse")
library("cluster")
#data----
filename= "./AG2_ramp/DATI/A48 PRE/A48_pre_1.csv"
sens_pre <- read_csv(filename)

filename= "./AG2_ramp/DATI/A48 POST/A48_post_1.csv"
sens_post <- read_csv(filename)

#rename columns
#names= c("times","a20","r21","r22","r24","r25","r42","r43","r44","r45","r46","r47","r48","r49")
names= c("times","a48","r20","r21","r22","r24","r25","r42","r43","r44","r45","r46","r47","r49")
colnames(sens_pre) <- names
colnames(sens_post) <- names


sens_pre <- sens_pre %>%
  mutate(post=1)
sens_post <- sens_post %>%
  mutate(post=2)

#prova clustering gerarchico su dati originali----

#pre damage
mat <- sens_pre %>% 
  select(-c(times,a48,post)) %>% 
  t()

d= dist(mat)
hc_complete_pre <- hclust(d = d, method = "complete")
plot(hc_complete_pre)

#post damage
mat <- sens_post %>% 
  select(-c(times,a48,post)) %>% 
  t()

d= dist(mat)
hc_complete_post <- hclust(d = d, method = "complete")
plot(hc_complete_post)

# prova gerarchico e  K-means su differenze segnale punto per punto ----

diff<- abs((sens_post-sens_pre)/sens_pre) %>% na.omit()
diff<-diff %>% select(-c(times,a48,post))

#gerarchico
mat<- diff %>% t()
d= dist(mat)
hc_diff <- hclust(d = d, method = "complete")
plot(hc_diff)

#gerarchico matrice di correlazione
d <- as.dist(1 - cor(t(mat)))
hc_diff_corr <- hclust(d = d, method = "complete")
plot(hc_diff_corr)






