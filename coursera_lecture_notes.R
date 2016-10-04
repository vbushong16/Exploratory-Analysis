### Exploratory Analysis


### Week 1 ##

## Air pollution exploratory analysis
## need to download the data
pollution <- read.csv("data/avgpm25.csv", colClasses = c("numeric","character","factor","numeric","numeric"))
head(pollution)


### 1 dimensional summaries
## 5 number summary
summary(pollution$pm25)

## boxplot
boxplot(pollution$pm25, col = "blue")
abline(h=12)

## histogram
hist(pollution$pm25, col = "green",breaks = 100)
rug(pollution$pm25)
abline(v =12, lwd = 2)
abline( v = median(pollution$pm25), col = "magenta", lwd = 4 )

## barplot
barplot(table(pollution$region), col = "wheat", main = "Number of counties in each region")

### 2 dimensional summaries
## boxplot
boxplot(pm25~region, data=  pollution, col= "red")

##histogram
par(mfrow=c(2,1), mar = c(4,4,2,1))
hist(subset(pollution, region== "east")$pm25, col = "green")
hist(subset(pollution, region== "west")$pm25, col = "green")

## scatterplot
with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd =2 , lty =2)

par(mfrow = c(1,2), mar = c(5,4,2,1))
with(subset(pollution, region =="west"), plot(lattitude, pm25, main = "West"))
with(subset(pollution, region =="East"), plot(lattitude, pm25, main = "East"))

### >2 dimensional summaries

# use color/ highlight different variables

### base plot build as you
library(datasets)
data(cars)
with(cars, plot(speed, dist))

hist(airquality$Ozone) 
with(airquality, plot(Wind,Ozone))
title(main = "OZone and Wind in NYC")

with(airquality, plot(Wind, Ozone, main = "Ozone and wind in NYC",type = "n"))
with(subset(airquality, Month == 5), points(Wind,Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind,Ozone, col = "red"))
legend("topright",pch = 1, col = c("blue","red"), legend = c("May", "not May"))
model <- lm(Ozone~Wind, airquality)
abline(model, lwd = 2)


par(mfrow = c(1,3), mar=c(4,4,2,1), oma = c(0,0,2,0))
with(airquality,{
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
  plot(Temp, Ozone, main = "Ozone and Temperature")
  mtext("Ozone and Weather in NWC", outer = TRUE)
})

airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone~Month, airquality, xlab ="Month",ylab = "Ozone lvl")


x = rnorm(100)
hist(x)
y = rnorm(100)
par(mar =c(4,4,2,2))
plot(x,y,pch = 20,xlab = "weight",ylab = "height",main = "Scatterplot")
text(-2,-2,"Label")
legend("topr", legend = "data",pch=20)
fit = lm(x~y)
abline(fit,lwd=3, col = "blue")

z = rpois(100,2)
par(mfrow =c(2,2),mar=c(4,4,2,2))
plot(x,y,pch = 20)
plot(x,z,pch =19)
plot(z,x,pch =19)
plot(y,x,pch = 20)

x = rnorm(100)
y = x + rnorm(100)
g = gl(2,50,labels= c("Male","Female"))
plot(x,y,type = "n")
points(x[g == "Male"], y[g == "Male"], col ="green")
points(x[g == "Female"], y[g == "Female"], col ="blue",pch = 19)

### lattice plot build all at once
library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp~ Income| region, data = state, layout =c(4,1))

### ggplot2

library(ggplot2)
data(mpg)
qplot(displ,hwy, data = mpg)

### graphics Device

library(datasets)
pdf(file = "myplot.pdf")
with(faithful,plot(eruptions,waiting))
title(main="Graphics Device test")
dev.off()

### week 2 ##

### Lattice plotting system

# f and g are level variable
#xyplot(y~x| f*g, data)

library(lattice)
library(datasets)

xyplot(Ozone~Wind, data = airquality)

airquality = transform(airquality, Month =factor(Month))
xyplot(Ozone~Wind| Month, data = airquality, layout = c(5,1))

set.seed(10)
x = rnorm(100)
f = rep(0:1,each = 50)
y = x+f-f*x +rnorm(100,sd= .5)
f = factor(f,labels = c("Group 1","Group2"))

xyplot(x~y|f, panel = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(h = median(y), lty= 2)
})

xyplot(x~y|f, panel = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.lmline(x,y, col= 2)
})


### ggplot2 ##

library(ggplot2)
str(mpg)
##scatterplot
qplot(displ,hwy,data =mpg,color= drv,geom=c("point","smooth"))
##hist
qplot(hwy,data =mpg,fill= drv)
##facets
qplot(displ,hwy,data = mpg,facets=.~drv)
qplot(hwy,data = mpg,facets=drv~.,binwidth=2)
qplot(hwy,data = mpg,geom = "density",color = drv)

#basic plots
qplot(displ,hwy, data = mpg, facets = .~drv,geom = c("point","smooth"),method = "lm")

g = ggplot(mpg,aes(displ,hwy))
g + geom_point(color = "steelblue",size = 4, alpha = .5)+geom_smooth()
g + geom_point(aes(color = drv),size = 4, alpha = .5)+facet_grid(.~drv)+geom_smooth(method = "lm")
g + geom_point(aes(color =drv),size = 2,alpha = .5)+
  geom_smooth(size = 4,linetype = 3,method= "lm",se = FALSE) +
  labs(title = "Drive type") + labs(x = expression("log " *PM(2.4)), y ="HWY")+
  theme_bw(base_family = "Times")

### Axis limit ##

testdat = data.frame(x=1:100, y = rnorm(100))
testdat[50,2] = 100
plot(testdat$x,testdat$y, type = "l",ylim = c(-3,3))

g = ggplot(testdat,aes(x =x ,y=y))
g+geom_line()
g+geom_line()+ylim(-3,3)
g+geom_line()+coord_cartesian(ylim=c(-3,3))


### example

str(mpg)
cutpoints = quantile(mpg$displ, seq(0,1,length=4),na.rm=TRUE)
mpg$group_displ = cut(mpg$displ,cutpoints)
levels(mpg$group_displ)

g = ggplot(mpg,aes(displ,hwy))
g + geom_point(alpha =1/3)+
  facet_wrap(drv~group_displ,nrow =3, ncol =3)+
  geom_smooth(method = "lm",se= FALSE,col="steelblue")+
  theme_bw(base_size = 10)+
  labs(x = "displ")+
  labs(y = "hwy")+
  labs(tittle ="mpg analysis")


### Week 3 ##

#Hierarchical clustering
set.seed(1234)
par(mar = c(0,0,0,0))
x = rnorm(12,mean=rep(1:3,each =4),sd = 0.2)
y = rnorm(12,mean=rep(c(1,2,1),each =4),sd = 0.2)
plot(x,y,col="blue",pch=19,cex = 2)
text(x+0.05,y+0.05,labels = as.character(1:12))

dataFrame = data.frame(x=x,y=y)
distxy = dist(dataFrame)
hClustering = hclust(distxy)
plot(hClustering)

set.seed(143)
dataMatrix = as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)

#K means clustering (need to find out the number of cluster) (run the algorithm multiple times in case of data being unstable)

dataFrame = data.frame(x=x,y=y)
kmeansObj = kmeans(dataFrame, centers =3)
names(kmeansObj)
kmeansObj$cluster

par(mar=rep(.2,4))
plot(x,y,col = kmeansObj$cluster, pch =19, cex =2 )
points(kmeansObj$centers,col = 1:3, pch= 3, cex = 3,lwd=3)

set.seed(1234)
dataMatrix =as.matrix(dataFrame)[sample(1:12),]
kmeansObj2 = kmeans(dataMatrix, centers =3)
par(mfrow = c(1,2), mar = c(2,4,.1,.1))
image(t(dataMatrix)[,nrow(dataMatrix):1],yaxt = "n")
image(t(dataMatrix)[,order(kmeansObj2$cluster)],yaxt = "n")

#Dimension Reduction

set.seed(12345)
par(mar = rep(.2,4))
dataMatrix = matrix(rnorm(400),nrow = 40)
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])
heatmap(dataMatrix)

set.seed(678910)
for(i in 1:40){
  #flip a coin 
  coinFlip = rbinom(1, size =1 , prob =.5)
  #if coin is heads add a common pattern to that row
  if(coinFlip){
    dataMatrix[i,] = dataMatrix[i,] + rep(c(0,3),each = 5)
  }
}


par(mar =rep(.2,4))
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])
heatmap(dataMatrix)


hh =hclust(dist(dataMatrix))
dataMatrixOrdered = dataMatrix[hh$order,]
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered),40:1,xlab = "Row Means", ylab= "Row " , pch = 19)
plot(colMeans(dataMatrixOrdered),xlab = "Col", ylab= "Col Means " , pch = 19)

svd1 = svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd1$u[,1],40:1, xlab = "Rows", ylab = "First left singular vector",pch =19)
plot(svd1$v[,1],xlab = "Column",ylab = "First Right singular vector",pch= 19)

par(mfrow=c(1,2))
plot(svd1$d , xlab = "Column",ylab = "Singular value",pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column",ylab = "Prop. of variance explained")

svd1 = svd(scale(dataMatrixOrdered))
pca1 = prcomp(dataMatrixOrdered,scale =TRUE)
plot(pca1$rotation[,1],svd1$v[,1],pch=19,xlab = "Principal component1", ylab= "Right singular Vector 1")
abline(c(0,1))

constantMatrix = dataMatrixOrdered + 0
for( i in 1:dim(dataMatrixOrdered)[1]){  constantMatrix[i,]  =  rep(c(0,1),each =  5)}
svd1 = svd(constantMatrix)
par(mfrow =c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d , xlab = "Column",ylab = "Singular value",pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column",ylab = "Prop. of variance explained")

set.seed(678910)
for(i in 1:40){
  coinFlip1 = rbinom(1, size = 1,prob = .5)
  coinFlip2 = rbinom(1, size = 1,prob = .5)
  
  if(coinFlip1){
    dataMatrix[i,] = dataMatrix[i,]+rep(c(0,5),each = 5)
  }
  if(coinFlip2){
    dataMatrix[i,] = dataMatrix[i,]+rep(c(0,5),5)
  }
}

hh = hclust(dist(dataMatrix))
dataMatrixOrdered = dataMatrix[hh$order,]

svd2 = svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rep(c(0,1),each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1")
plot(rep(c(0,1),5), pch = 19, xlab = "Column", ylab = "Pattern 2")

svd2 = svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd2$v[,1], pch = 19, xlab = "Column", ylab = "Pattern 1")
plot(svd2$v[,2], pch = 19, xlab = "Column", ylab = "Pattern 2")

svd1 = svd(scale(dataMatrixOrdered))
par(mfrow = c(1,2))
plot(svd1$d , xlab = "Column",ylab = "Singular value",pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column",ylab = "Prop. of variance explained")

dataMatrix2 = dataMatrixOrdered
dataMatrix2[sample(1:100,size = 40, replace = FALSE)] = NA
svd1 = svd(scale(dataMatrix2))

# library(impute) #bioconductor lib
# dataMatrix2 = dataMatrixOrdered
# dataMatrix2[sample(1:100, size = 40, replace = FALSE)]= NA
# dataMatrix2 = impute.knn(dataMatrix2)$data
# svd1 = svd(scale(dataMatrixOrdered))
# svd2 = svd(scale(dataMatrix2))
# par(mfrow = c(1,2))
# plot(svd1$v[,1],pch =19)
# plot(svd2$v[,1],pch =19)


 










