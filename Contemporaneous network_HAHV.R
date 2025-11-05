setwd("D:/R/DATA/DATA/feature") #setting your working space
myData <- read.table(file = "HAHV_FEATURE_NODE_VER.csv",sep = ",", header = TRUE)#dataload

library(qgraph)
library(ggplot2)
library(bootnet)
library(networktools)
library(magrittr)
library(NetworkComparisonTest)
library(dplyr)
library(Matrix)

#myData <- myData[complete.cases(myData)] #DELETE NaN
nrow(myData)
ncol(myData)
colnames(myData) <- c("D1", "D2", "D3", "D4", "D5","D6",
"D7", "D8", "D9", "D10","D11",
"E1", "E2", "E3", "E4", "E5","E6","E7",
"E8", "E9", "E10", "E11", "E12",
"M1","M2","M3","M4","M5","M6",
"G1","G2","G3","G4","G5","G6","G7","G8","G9",
"R1","R2","R3","R4","R5","R6","R7","R8",
"T1","T2") #name your data column
labels <- c("D1", "D2", "D3", "D4", "D5","D6",
            "D7", "D8", "D9", "D10","D11",
            "E1", "E2", "E3", "E4", "E5","E6","E7",
            "E8", "E9", "E10", "E11", "E12",
            "M1","M2","M3","M4","M5","M6",
            "G1","G2","G3","G4","G5","G6","G7","G8","G9",
            "R1","R2","R3","R4","R5","R6","R7","R8",
            "T1","T2")#以疲乏、发热、头痛、感到孤单、悲伤 5 个症状作为例子，进行命名，命名信息保存在 labels 变量中
groups <- c(
  rep("1HRV", 11),   # D1~D11：hrv
  rep("2EEG", 12),   # E1~E12：eeg
  rep("3EMG", 6),    # M1~M6：emg
  rep("4GSR", 9),    # G1~G9：gsr
  rep("5RSP", 8),    # R1~R8：rsp
  rep("6TMP", 2)     # T1~T2：tmp
)
#your group
dev.new()
MYdata<-cor(myData, method = ("spearman"))


#Correct negative eigenvalues of the correlation matrix (ensure positive semi - definiteness)
eigen_result <- eigen(MYdata)
eigen_values <- eigen_result$values
eigen_vectors <- eigen_result$vectors

epsilon <- 1e-6
eigen_values[eigen_values < 0] <- epsilon

MYdata<- eigen_vectors %*% diag(eigen_values) %*% t(eigen_vectors)

#craft the EBICglasso graph
g <- qgraph(MYdata, layout="spring", labels=labels, groups=groups, label.scale=FALSE, label.cex=0.6, node.width=0.8, 
            graph = "EBICglasso", sampleSize = nrow(myData),tuning = 0.7,threshold = 0.01,
            color=c("#8FB4DC", "#70CDBE","#EB7E60","#FFDD8E","#EFF9A7","#F5AA61"),
            edge.color=c("#70A5D9","#F0868C"))
cen<-centrality(g) #centrality analysis
corr <- cor(myData)
write.csv(corr,file = "hahv_cor_result.csv",row.names = TRUE)
# save the centrality results
for (i in 2:5) {

  current_list <- cen[[i]]
  

  if (all(sapply(current_list, is.vector))) {

    df <- do.call(rbind, lapply(current_list, as.data.frame))

    filename <- paste0("list_hahv", i, ".csv")
    write.csv(df, file = filename, row.names = TRUE)
  } else {

    message("List_hahv ", i, " cannot be directly converted to a data frame.")
  }
}


dev.new()
centralityPlot(g, include=c("Strength","Closeness","Betweenness")) #visualize your centrality result
b <- bridge(MYdata, communities= groups, directed=FALSE) #bridge centrality
dev.new()
BPLOT <- plot(b, include=c("Bridge Expected Influence (1-step)", "Bridge Strength", "Bridge Closeness"),
theme_bw=FALSE, raw0 = TRUE, signed=TRUE) #visualize your bridge centrality result
# save the bridge centrality results
for (i in 1:5) {

  current_list <- b[[i]]
  

  if (all(sapply(current_list, is.vector))) {

    df <- do.call(rbind, lapply(current_list, as.data.frame))
    

    filename <- paste0("list_hahv_bridge", i, ".csv")
    write.csv(df, file = filename, row.names = TRUE)
  } else {

    message("List_hahv ", i, " cannot be directly converted to a data frame.")
  }
}

#edge robust test
nonParametricBoot <- bootnet(myData, nBoots = 500, 
 default = "EBICglasso",
 corMethod = "spearman", threshold = FALSE,
 type="nonparametric", 
 statistics="all")
dev.new()
plot(nonParametricBoot, labels= FALSE, order="sample",sampleColor =
       "#F0868C", samplelwd = 0.6, meanColor = "#70CDBE", meanlwd
     = 0.6, bootColor = "#EB7E60",bootlwd = 1.5)
dev.new()
plot(nonParametricBoot, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
summary(nonParametricBoot) %>% ungroup %>% filter(type == "edge") %>% arrange(-sample) %>% View
windows()
plot(nonParametricBoot, statistics="Strength", plot="difference",order = "mean")

#centrality robust test
dev.new()
caseDroppingBoot <- bootnet(myData, nBoots = 100,corMethod = "spearman", threshold = FALSE, default = "EBICglasso", type="case",
                            , communities= groups, useCommunities = "all",directed=FALSE,statistics="all")
corStability(caseDroppingBoot)
windows()
plot(caseDroppingBoot, statistics="all")
dev.new()
plot(caseDroppingBoot, statistics = c("Strength","ExpectedInfluence","Betweenness","bridgeStrength","bridgeBetweenness","bridgeExpectedInfluence"),
     cex.axis = 10.5,
     cex.lab = 10.5,
     las = 5)


