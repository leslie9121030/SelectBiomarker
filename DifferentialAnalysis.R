setwd("~/Rnewspace")
df0=as.data.frame(t(data.table::fread("second_magic0.csv",header=T)))[-1,]
df1=as.data.frame(t(data.table::fread("second_magic1.csv",header=T)))[-1,]
df2=as.data.frame(t(data.table::fread("second_magic2.csv",header=T)))[-1,]
df3=as.data.frame(t(data.table::fread("second_magic3.csv",header=T)))[-1,]
df4=as.data.frame(t(data.table::fread("second_magic4.csv",header=T)))[-1,]
df5=as.data.frame(t(data.table::fread("second_magic5.csv",header=T)))[-1,]
df6=as.data.frame(t(data.table::fread("second_magic6.csv",header=T)))[-1,]


#比较T6
y0=cbind(df2,df4,df5)
y=cbind(y0,df6)
group_list = c(rep("other",dim(y0)[2]), rep("Regulator",dim(df6)[2]))#对应sample的分组列表



#比较T2
y0=cbind(df5,df6,df4)
y=cbind(y0,df2)
group_list = c(rep("other",dim(y0)[2]), rep("Helper",dim(df2)[2]))#对应sample的分组列表



#比较T5
y0=cbind(df2,df3,df6)
y=cbind(y0,df5)
group_list = c(rep("other",dim(y0)[2]), rep("Naive",dim(df5)[2]))#对应sample的分组列表




#比较T4
y0=cbind(df2,df4,df5)
y=cbind(y0,df6)
group_list = c(rep("other",dim(y0)[2]), rep("Regulator",dim(df4)[2]))#对应sample的分组列表



#比较T3
y0=cbind(df4,df5,df2)
y=cbind(y0,df3)
group_list = c(rep("other",dim(y0)[2]), rep("Regulator",dim(df3)[2]))#对应sample的分组列表





#比较T0和T1
y0=df0
y=df1
group_list = c(rep("other",dim(y0)[2]), rep("Regulator",dim(df1)[2]))#对应sample的分组列表





library(limma)
# 1.构建实验设计矩阵
design <- model.matrix(~0+factor(group_list))

design

colnames(design)=levels(factor(group_list))

rownames(design)=colnames(y)

#给design矩阵加行名，行名为表达谱矩阵的列名，即sample

design

# 实验设计矩阵的每一行对应一个样品的编码，

# 每一列对应样品的一个特征。这里只考虑了一个因素两个水平，

# 如果是多因素和多水平的实验设计，会产生更多的特征，需要参考文档设计。

# 3.构建对比模型（对比矩阵），比较两个实验条件下表达数据

contrast.matrix<-makeContrasts(paste0(unique(group_list),collapse = "-"),levels = design)

#contrast.matrix<-makeContrasts(paste0(unique(group_list),collapse = "-"),levels = design)

contrast.matrix ##这个矩阵声明，我们要把G3组跟con组进行差异分析比较

##### 差异分析

##4.  step1 线性模型拟合

fit <- lmFit(y,design)

##    step2 根据对比模型进行差值计算

fit2 <- contrasts.fit(fit, contrast.matrix)

##5.  step3 贝叶斯检验

fit2 <- eBayes(fit2)

##6.  step4 生成所有基因的检验结果报告

tempOutput = topTable(fit2, coef=1, n=Inf)

##step5 用P.Value进行筛选，得到全部差异表达基因

dif <- tempOutput[tempOutput[, "P.Value"]<0.01,]

# 显示一部分报告结果

head(dif)




write.csv(dif,"T6_DiffAnalysis.csv") 
