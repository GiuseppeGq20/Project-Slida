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

#matrice di differenze
diff<- abs((sens_post-sens_pre)/sens_pre) %>% na.omit()
diff<-diff %>% select(-c(times,a48,post))

#K-means----
#SEMBRA NON fUNZIONARE
diffm<-diff %>% as.matrix()
km_diff <- kmeans(diffm, centers = 2, nstart = 200)

#scelta K cluster
k_seq <- 1:10
silhouette_vec <- numeric(length(k_seq))
x=diffm
for (ii in seq_along(k_seq)) {
  kk <- k_seq[ii]
  km_out <- kmeans(x, centers = kk, nstart = 100)
  cluster_kk <- km_out$cluster
  sil <- silhouette(cluster_kk, dist = dist(x))
  silhouette_vec[ii] <- summary(sil)$avg.width
}
plot(k_seq, silhouette_vec, 
     type = "l",
     xlab = "K",
     ylab = "Silhouette (media)")
points(k_seq[which.max(silhouette_vec)], 
       max(silhouette_vec), 
       col = "red", 
       pch = 20)