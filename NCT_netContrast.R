#NCT
#Network Comparison test(NCT)
setwd("D:/R/DATA/DATA/feature") #SET WORK SPACE

library(qgraph)
library(ggplot2)
library(bootnet)
library(networktools)
library(magrittr)
library(NetworkComparisonTest)
library(dplyr)
#load data
data1 <- read.table(file = "HAHV_FEATURE_NODE_VER.csv",sep = ",", header = TRUE )#data2 load
data2 <- read.table(file = "HALV_FEATURE_NODE_VER.csv",sep = ",",header = TRUE)#data1 load
mydata <- read.csv(file = "HAHV_FEATURE_NODE_VER.csv")#case data load

colnames(myData) <- c("D1", "D2", "D3", "D4", "D5","D6",
                      "D7", "D8", "D9", "D10","D11",
                      "E1", "E2", "E3", "E4", "E5","E6","E7",
                      "E8", "E9", "E10", "E11", "E12",
                      "M1","M2","M3","M4","M5","M6",
                      "G1","G2","G3","G4","G5","G6","G7","G8","G9",
                      "R1","R2","R3","R4","R5","R6","R7","R8",
                      "T1","T2") #name your data column
colnames(mydata)<-myname
mydata.frame<-myname
colnames(data1)<-myname
mydata.frame<-myname
colnames(data2)<-myname
mydata.frame<-myname

#your groups
feature_group<-list(winr=1,	kill=2,	cash=3,	baron=4,	dragon=5,	turret=6,	blood=7,	fturret=7,	fdragon=7)
for (col_name in names(data1)) {
  if (is.numeric(data1[[col_name]])) {
    data1[[col_name]][is.na(data1[[col_name]])] <- 0
  }
}
for (col_name in names(data2)) {
  if (is.numeric(data2[[col_name]])) {
    data2[[col_name]][is.na(data2[[col_name]])] <- 0
  }
}
newdata1<-na.omit(data1)
newdata2<-na.omit(data2)     #NCT need data has completed and without NaN
standardize <- function(x) {
  return ((x - mean(x)) / sd(x))
}
newdata1 <- as.data.frame(lapply(newdata1, standardize))
newdata2 <- as.data.frame(lapply(newdata2, standardize))##zscore standardized
#estimate networks

mynetwork1<-estimateNetwork(newdata1,default = "EBICglasso",corMethod ="cor",threshold = TRUE,)
mynetwork2<-estimateNetwork(newdata2,default = "EBICglasso",corMethod ="cor",threshold = TRUE,)
#mynetwork2[["labels"]]<-myname
#mynetwork1[["labels"]]<-myname
#Run NCT
myNCT1<-NCT(mynetwork1,mynetwork2,it=500,weighted=TRUE,progressbar = F,test.edges=TRUE,edges="ALL",paired=FALSE, abs=FALSE)
           # ,test.centrality=TRUE,centrality=c("strength","expectedInfluence"),nodes="all",)


#Get results and save results
write.csv(myNCT1[["einv.pvals"]],file = "einv_pval_hv-lv.csv",row.names = TRUE)

save(myNCT1, file = "./-NCT.RData")
