
setwd("C:\\Users\\bushv003\\Documents\\Github\\Exploratory-Analysis")
source("myplclust.R")



### SETTING THE WORK DIRECTORY
wkdir = "~\\"
subdir = "Github\\Getting and Cleaning Data"

# create directory if it doesn't exist yet
dir.create(file.path(wkdir, subdir))
setwd(file.path(wkdir, subdir))
getwd()
#zip file download to director
url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "peer_review_data.zip")

#unzip file
unzip("peer_review_data.zip")

## LOAD LIBRARIES NEEDED
library(plyr) # FOR DATA TRANSFORMATION

#INPUT DIRECTORY PATH WHERE THE EXTRACTED FILES IS
dir = paste0(getwd(),"\\UCI HAR Dataset\\")

#FILES OF INTEREST PATH 
activity_label = paste0(dir,"activity_labels.txt")
features = paste0(dir,"features.txt")
subject_test = paste0(dir,"test\\subject_test.txt")
X_test = paste0(dir,"test\\X_test.txt")
Y_test = paste0(dir,"test\\Y_test.txt")
subject_train = paste0(dir,"train\\subject_train.txt")
X_train = paste0(dir,"train\\X_train.txt")
Y_train = paste0(dir,"train\\Y_train.txt") 

print("ALL DATA FILES LINKED")

#DESCRIPTIVE DATA LOAD
row_values_activities = read.table(activity_label)
print("activity_label.txt Loaded")

col_values_features = read.table(features)
print("features.txt Loaded")

row_values_activities[,2] = tolower(row_values_activities[,2])
row_values_activities[,2] = gsub("_"," ",row_values_activities[,2])
feature_labels = col_values_features[,2]

#TEST DATA LOAD
individual_id_test = read.table(subject_test)
print("subject_test.txt Loaded")
data_test = read.table(X_test)
print("X_test.txt Loaded")
row_id_test = read.table(Y_test)
print("Y_test.txt Loaded")
#TEST DATA FORMATTING WITH ACTIVITIES AND FEATURES
activity_id = row_values_activities[row_id_test[,1],2]
colnames(data_test) = feature_labels
full_data_test = cbind(activity_id,individual_id_test,data_test)
colnames(full_data_test)[1:2] = c("Activity","Individual ID")

#TRAIN DATA LOAD
individual_id_train = read.table(subject_train)
print("subject_train.txt Loaded")
data_train = read.table(X_train)
print("X_train.txt Loaded")
row_id_train = read.table(Y_train)
print("Ytrain.txt Loaded")

#TRAIN DATA FORMATTING WITH ACTIVITIES AND FEATURES
activity_id = row_values_activities[row_id_train[,1],2]
colnames(data_train) = feature_labels
full_data_train = cbind(activity_id,individual_id_train,data_train)
colnames(full_data_train)[1:2] = c("Activity","Individual ID")


#MERGING OF TRAIN AND TEST DATA SETS
full_data = rbind(full_data_train,full_data_test)
full_data[,"Individual ID"] = factor(full_data[,"Individual ID"])


data = full_data
colnames(data)
colnames(data)[2] = "subject"
par(mfrow=c(1,2),mar = c(5,4,1,1))
sub1 = subset(data,subject == 1)
plot(sub1[,3],col = sub1$Activity)
plot(sub1[,4],col = sub1$Activity)
legend("bottomright", legend = unique(sub1$Activity),col = unique(sub1$Activity),pch =1)

dmatrix = dist(sub1[,12:14])
hclustering = hclust(dmatrix)
myplclust(hclustering,lab.col = unclass(sub1$Activity))

svd1 = svd(scale(sub1[,3:563]))
plot(svd1$u[,1],col = sub1$Activity)
plot(svd1$u[,2],col = sub1$Activity)

m = apply(svd1$v,2,which.max)
names(data)[m[2]+2]
dmatrix = dist(sub1[,c(12:14,m[2]+2)])
hclustering = hclust(dmatrix)
myplclust(hclustering,lab.col = unclass(sub1$Activity))

plot(svd1$d)
plot((svd1$d^2/sum(svd1$d^2)))

d_vals = (svd1$d^2/sum(svd1$d^2))>.01
plot(svd1$u[,d_vals],col = sub1$Activity)


m = apply(svd1$v,2,which.max)
names(data)[m[d_vals]+2]
dmatrix = dist(sub1[,c(12:14,m[d_vals]+2)])
hclustering = hclust(dmatrix)
myplclust(hclustering,lab.col = unclass(sub1$Activity))

# a = 1
# b = 2
# a10 = svd1$u[,a:b] %*% diag(svd1$d[a:b]) %*% t(svd1$v[,a:b])
# heatmap(a10)



kClust = kmeans(sub1[,c(12:14,m[d_vals]+2)],centers = 6)
table(kClust$cluster,sub1$Activity)

kClust = kmeans(sub1[,3:563],centers = 6)
table(kClust$cluster,sub1$Activity)

plot(kClust$center[1,])
plot(kClust$center[2,])
plot(kClust$center[3,])
plot(kClust$center[4,])


data = read.csv("daily_88101_1999.csv")
colnames(data)
x0 = data[,"Arithmetic.Mean"]
summary(x0)

data1 = read.csv("daily_88101_2012.csv")
x01 = data1[,"Arithmetic.Mean"]
summary(x01)

boxplot(log10(x0),log10(x01))
par(mar = c(4,4,4,4))
negative = x01 <0
mean((negative))
dates = data1$Date.Local
dates = as.Date(as.character(dates),"%Y-%m-%d")
hist(dates[negative],"month")


site0 = unique(subset(data, State.Code == 36,c(County.Code,Site.Num)))
site1 = unique(subset(data1, State.Code == 36,c(County.Code,Site.Num)))

head(site0)

site0 = paste(site0[,1],site0[,2],sep = ".")
site1 = paste(site1[,1],site1[,2],sep = ".")

both = intersect(site0,site1)
both

data$county.site = with(data,paste(County.Code,Site.Num, sep = "."))
data1$county.site = with(data1,paste(County.Code,Site.Num, sep = "."))

cnt0 = subset(data, State.Code == 36 & county.site %in% both)
cnt1 = subset(data1, State.Code == 36 & county.site %in% both)

sapply(split(cnt0, cnt0$county.site),nrow)
sapply(split(cnt1, cnt1$county.site),nrow)


pm1_sub = subset(data1,State.Code == 36 & County.Code == 63 & Site.Num == 2008)
pm0_sub = subset(data,State.Code == 36 & County.Code == 63 & Site.Num == 2008)

date1 = pm1_sub$Date.Local 
x1sub = pm1_sub$Arithmetic.Mean
plot(date1,x1sub)

date = pm0_sub$Date.Local 
x0sub = pm0_sub$Arithmetic.Mean
plot(date,x0sub)












