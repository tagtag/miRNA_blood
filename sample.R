# GSE31568_raw.txt should be downloaded from from 
#https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE31568
x <- read.csv("GSE31568_raw.txt",sep="\t")
y <- x[1,]
x <- x[-1,]
x <- data.frame(x[,1],apply(x[,2:dim(x)[2]],2,as.numeric))
x <- data.frame(x[,1],apply(x[,2:dim(x)[2]],2,scale)) #scaled
y <- apply(y,2,as.character)
y <- y[-1]
library(caTools)
library(MASS)
i<-1;j<-2;
index1 <- y==unique(y)[i]
index2 <- y==unique(y)[j]
Z <-x[,2:dim(x)[2]][,index1 | index2] #scaled 
pca1 <- prcomp(Z)
index <- rank(-rowSums(pca1$x[,1:2]^2)^0.5)<=10 #***
K<- sum(index)
pca <- prcomp(t(Z[index,]))
X <- pca$x
Perf<-NULL
for (K1 in 2:K)
{
LD <- lda(X[,1:K1],y[index1 | index2],CV=T,prior=rep(1/2,2));TABLE <- table(LD$class,y[index1 | index2]) 
Perf<- rbind(Perf,as.vector(TABLE))
}
Perf1<-NULL
index <- which.max(rowSums(Perf[,c(1,4)])/rowSums(Perf))
TABLE <- matrix(Perf[index,],ncol=2)
Perf1 <- rbind(Perf1,c(index+1,sum(diag(TABLE))/sum(TABLE),TABLE[1,1]/sum(TABLE[,1]),TABLE[2,2]/sum(TABLE[,2]),TABLE[2,2]/sum(TABLE[2,])))
Perf1 
