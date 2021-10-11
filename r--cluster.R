install.packages("factoextra")
library(dplyr)       
library(ggplot2)     
library(cluster)     
library(factoextra)  


ames_scale %
select_if(is.numeric) %>%  # 选择数据类型为数字
  select(-Sale_Price) %>%    # 去除特定的数据
  mutate_all(as.double) %>%  # 强制转换类型
  scale()                    # 中心化标准化数据


#层次聚类算法可以分成两种：

#1. 聚集聚类(Agglomerative clustering)，通常叫 AGNES，理解成从个体到整体的聚类方式；
#2. 分裂层次聚类(Divisive hierarchical clustering)，通常叫 DIANA，理解成从整体到个体的扩散。

library(colorspace)
# 显示数据集的结构
str(data)
# 系统聚类
# 聚类的一些必要的函数
library(cluster)
library(rattle)
#系统聚类函数在包amap中
require(amap, quietly=TRUE)
#聚类结果有包fpc提供
require(fpc, quietly=TRUE)
#绘图 需cba包
require(cba, quietly=TRUE) 
#method曼哈顿距离，ward离差平方和
chcluster <- hclusterpar(na.omit(data[,c(1:2)]), method="manhattan", link="ward", nbproc=2)
chcluster 
# 聚类中心
centers.hclust(na.omit(data[,c(1:2)]), chcluster, 3) 
#产生树形图 用矩形显示聚类结果 
par(bg="grey")
plot(chcluster, main="", sub="", xlab="", labels=FALSE, hang=0)
rect.hclust(chcluster, k=3)
#加标题
title(main="HCluster_Dendrogram_data", sub=paste("R", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"])) 
#类与类之间的相关性 
par(bg="yellow")
plotcluster(na.omit(data[,c(1:2)]),  cutree(chcluster, 3))
title(main="Discriminant Coordinates data", sub=paste("R", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"])) 
#数据集的聚类效果图
plot(data[,c(1:2)], col=cutree(chcluster, 3))
title(main="", sub=paste("R", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"])) 
#验证聚类结果的基本统计信息
cluster.stats(dist(na.omit(data[,c(1:2)])), cutree(chcluster, 3)) 
#输出结果
result<-cbind(data,cutree(chcluster,3))
write.csv(result,"cengciresult.csv")


