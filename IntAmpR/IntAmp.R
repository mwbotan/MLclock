# Expression data
load("rpm_log2_mean_DRG")

# Sample information
load("sample_mean")

# PCA
pca <- prcomp(rpm_log2_mean_DRG[sample_mean$set!="weekly",],scale=F)

# Centroid for each season
pca_mean <- data.frame(PC1=0,PC2=0,PC3=0)
pca_mean[1,] <- colMeans(pca$x[sample_mean[sample_mean$set!="weekly","set"]=="VE2013",1:3])
pca_mean[2,] <- colMeans(pca$x[sample_mean[sample_mean$set!="weekly","set"]=="SS2013",1:3])
pca_mean[3,] <- colMeans(pca$x[sample_mean[sample_mean$set!="weekly","set"]=="AE2013",1:3])
pca_mean[4,] <- colMeans(pca$x[sample_mean[sample_mean$set!="weekly","set"]=="WS2013",1:3])

# Weekly data
w1 <- colSums(t(rpm_log2_mean_DRG[sample_mean$set=="weekly",])*pca$rotation[,1])
w2 <- colSums(t(rpm_log2_mean_DRG[sample_mean$set=="weekly",])*pca$rotation[,2])
w3 <- colSums(t(rpm_log2_mean_DRG[sample_mean$set=="weekly",])*pca$rotation[,3])

m1 = mean(colSums(t(rpm_log2_mean_DRG[sample_mean$set!="weekly",])*pca$rotation[,1]))
m2 = mean(colSums(t(rpm_log2_mean_DRG[sample_mean$set!="weekly",])*pca$rotation[,2]))
m3 = mean(colSums(t(rpm_log2_mean_DRG[sample_mean$set!="weekly",])*pca$rotation[,3]))

w1m <- w1-m1
w2m <- w2-m2
w3m <- w3-m3

# Calculate distance
v <- pca_mean[2,] - pca_mean[4,]
amp <- c()
for (i in 1:length(w1m)){
 target<- c(w1m[i],w2m[i],w3m[i])
 
 t = v*(target - pca_mean[4,])/sum(v^2)
 
 amp = c(amp,sqrt(sum(((pca_mean[4,]+t*v) - target)^2)))
}
plot(amp)

#plot
library(rgl)
plot3d(pca$x[sample_mean[sample_mean$set!="weekly","hour"]!=12,1:3],col=gray(0.5),xlab="",ylab="",zlab="",size=1)
#plot3d(pca$x[sample_mean[sample_mean$set!="weekly","hour"]!=12,1:3],col=gray(0.5),xlab="PC1",ylab="PC2",zlab="PC3",size=1)

lines3d(pca$x[sample_mean[sample_mean$set!="weekly","set"]=="VE2013",1:3],col="forestgreen",lwd=3)
lines3d(pca$x[sample_mean[sample_mean$set!="weekly","set"]=="SS2013",1:3],col="red",lwd=3)
lines3d(pca$x[sample_mean[sample_mean$set!="weekly","set"]=="AE2013",1:3],col="purple",lwd=3)
lines3d(pca$x[sample_mean[sample_mean$set!="weekly","set"]=="WS2013",1:3],col="dodgerblue",lwd=3)
text3d(pca$x[sample_mean[sample_mean$set!="weekly","hour"]==12,1:3],text="X",font=2, color="deeppink1")

clear3d(type="bboxdeco") 
axes3d(nticks=3,lwd=1)
clear3d(type="lights") 

points3d(pca_mean[c(2,4),],size=5,col="darkorange2")
lines3d(pca_mean[c(2,4),],lwd=3,col="darkorange2")

points3d(w1m,w2m,w3m,size=5)
