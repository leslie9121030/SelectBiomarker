library(ggplot2)
library(magrittr)
library(ggpubr)

a=read.csv("grouplist_ITGAX_IDO1_ACTA2.csv",header=T,sep=',')

#取因子的level
my_comparisons=list(levels(a[,4]))
my_comparisons
grouplist=as.character(a[,4])

#ITGAX     IDO1    ACTA2  
#data.frame()函数不是转换数据框函数，而是组合数据框的函数，
#指明了每一列的组成，dat数据框有三列，group、values 2列
dat1=data.frame(group=grouplist,
               values= as.numeric(a[,'ITGAX']))

#画'ITGAX'基因的表达箱型图
ggboxplot(
  dat1, x = "group", y = "values",
  color = "group",
  add = "jitter",
  title = 'ITGAX'
)+
  stat_compare_means(comparisons = my_comparisons, 
                     method = "t.test"
  )+
  theme(axis.text.x=element_text(angle=60, hjust=1))

dat2=data.frame(group=grouplist,
                values= as.numeric(a[,'IDO1']))
#画'IDO1'基因的表达箱型图
ggboxplot(
  dat2, x = "group", y = "values",
  color = "group",
  add = "jitter",
  title = 'IDO1'
)+
  stat_compare_means(comparisons = my_comparisons, 
                     method = "t.test"
  )+
  theme(axis.text.x=element_text(angle=60, hjust=1))



dat3=data.frame(group=grouplist,
                values= as.numeric(a[,'ACTA2']))
#画'ACTA2'基因的表达箱型图
ggboxplot(
  dat3, x = "group", y = "values",
  color = "group",
  add = "jitter",
  title = 'ACTA2',
  font.label = list(size =
                      100, color = "black")
)+
  stat_compare_means(comparisons = my_comparisons, 
                     method = "t.test"
  )+
  theme(axis.text.x=element_text(angle=60, hjust=1))



library(ggplot2)
#sample(10)函数从1-10随机采10个整数，不放回抽样
#sample(m,n)：m>n,从1~m中随机抽取n个数字
#[1]  6  3  9  7  2  8  1 10  4  5
sample1=diamonds[sample(nrow(diamonds),100),]
#diamonds[1:5,1:5]
## A tibble: 5 x 5
#carat cut     color clarity depth
#<dbl> <ord>   <ord> <ord>   <dbl>
#  1 0.23  Ideal   E     SI2      61.5
#2 0.21  Premium E     SI1      59.8
#3 0.23  Good    E     VS1      56.9
#4 0.290 Premium I     VS2      62.4
#5 0.31  Good    J     SI2      63.3
dim(diamonds)
#[1] 53940    10
sample1[1:5,1:10]
#   carat cut     color clarity depth
#1  0.42 Premium E     VS2      61.9
#2  0.42 Ideal   H     VVS2     62.4
#3  0.34 Premium D     SI1      61.2
#4  0.97 Ideal   J     VS1      61.3
#5  1.5  Premium H     SI2      62.2
dim(sample1)
#100  10
attach(sample1)#连接数据框sample1
#当x为分类变量时,plot函数可自动绘制出箱线图



#当一个变量起作用（一个grouplist)
#法一：
plot(cut, price)
#plot可以用来直接画boxplot，
#横坐标为grouplist的分组，纵坐标为对应的值




#法二：
#在boxplot函数中,可以使用公式来实现。
boxplot(price ~ cut, data = sample1,col=rainbow(7))
#boxplot(value ~ grouplist, data=data,col=rainbow(7))
#value和grouplist都是来自于data,是data里的列名



#法三：
#也可以用ggplot2中的函数实现，下面两行代码运行的结果相同。

#ggplot2方式一：
qplot(cut, price, data=sample1, geom="boxplot")
#qplot(grouplist, value, data=data, geom="boxplot")

#ggplot2方式二：
ggplot(sample1, aes(x=cut, y=price)) + geom_boxplot()
#ggplot(data, aes(x=grouplist, y=value)) + geom_boxplot()
#value和grouplist都是来自于data,是data里的列名,不用加双引号









#当两个x变量同时作用(两个grouplist)

#unique(cut)-----5个group
#[1] Premium   Ideal     Good      Very Good Fair     
#Levels: Fair < Good < Very Good < Premium < Ideal

#unique(color)-----7个group
#[1] E H D J G F I
#Levels: D < E < F < G < H < I < J

#第一组5个level,第二组7个level，
#所以两组同时作用，共5*7=35个level/group

#法一：boxplot的实现方法：
boxplot(price ~ cut+color, data = sample1,col=rainbow(7))
#boxplot(value ~ grouplist1 + grouplist2, data = data,col=rainbow(7))
#value和grouplist都是来自于data,是data里的列名,不用加双引号



#法二：ggplot2的实现方法：

#ggplot2方式一：
qplot(interaction(cut, color), price, data=sample1, geom="boxplot")

#ggplot2方式二：
ggplot(sample1, aes(x=interaction(cut, color), y=price)) + geom_boxplot()

