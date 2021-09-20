library(ggplot2)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(dplyr)
#Ggplot2：载入ggplot函数，用于作图
#Hmisc：载入rcorr函数，计算相关性矩阵，计算对应的显著性水平
#Corrplot：载入corrplot函数，进行可视化处理
#PerformanceAnalytics：载入chart.Correlation函数，进行可视化处理
#Tidyverse：用于制作散点图
#RColorBrewer：用于制作散点图
#Ggthemes：用于制作散点图
#Dplyr：用于数据挖掘


#载入数据
df <- read.table("C:/Users/20325/Desktop/R语言期末作业/85-20data-new.txt",header=TRUE,sep=",")


#问题一探究三分球出手数与得分、胜率、总出手数、防守篮板、进攻篮板、抢断与篮板的关系
df.x3 <- df[,which(names(df)%in% #等号+并列
                     c("X3.S","PTS","WR","ALL.S","ORB","DRB","STL","BLK"))]
df.x3 <- df.x3[,c("X3.S","PTS","WR","ALL.S","ORB","DRB","STL","BLK")]
cor_x3 = cor(df.x3)
cor_x3
rcorr(as.matrix(df.x3))#r：第一个矩阵为相关性矩阵 n：处理数据的总记录数 P：显著性水平矩阵(越小说明越显著)

#看起来相似系数并不是很好,让我们尝试从跑轰战术正式进入赛场的05-06赛季开始进行分析
syear.RG <- c(2006:2020)
df.RG <- df[df$Season %in% syear.RG,]
df.x3_RG <- df.RG[,which(names(df.RG)%in%c("X3.S","PTS","WR","ALL.S","ORB","DRB","STL","BLK"))]
df.x3_RG <- df.x3_RG[,c("X3.S","PTS","WR","ALL.S","ORB","DRB","STL","BLK")]
cor_x3_RG = cor(df.x3_RG)
cor_x3_RG
rcorr(as.matrix(df.x3_RG))

#可视化处理
#symnum可视化处理，但是效果还是一般
symnum(cor_x3)
symnum(cor_x3_RG)
#在输出的最后一行说明了符号的意义：如空格表示【0,0.3）


#corrplot函数可视化处理上
corrplot(cor_x3,type="upper",order="original",tl.col = "black",tl.srt = 45)#用不同颜色表示相关性的强度，根据最右边的颜色来看，越接近蓝色说明相关性越高，其中圆形的大小表示行惯性的大小。
corrplot(cor_x3_RG,type="upper",order="original",tl.col = "black",tl.srt = 45)

#用PerformanceAnalytics进行可视化处理
chart.Correlation(df.x3,histogram = TRUE, pch=19)
#对角线给出了变量自身的分布
#下三角形（对角线的左下方，给出了两个属性的散点图
#上三角形（对角线的右上方），数字表示连个属性的相关性值，型号表示显著程度（星星越多表明越显著）
chart.Correlation(df.x3_RG,histogram = TRUE,pch=19)


#用heatmap可视化
col=colorRampPalette(c("blue","white","red"))(20)
heatmap(x=cor_x3,col=col,symm = TRUE )
heatmap(x=cor_x3_RG,col=col,symm=TRUE)


#plot1
palette<-brewer.pal(11,"Spectral")
#添加拟合线、图序号
plot1 <- ggplot(df.x3,aes(x=X3.S,y=PTS))+
  stat_bin_2d(binwidth = c(0.4,0.4))+
  scale_fill_gradientn(colours = rev(palette),limits=c(0,12), breaks=c(0,3,6,9,12),
                       labels=c("0","3",'6','9','>12'))+
  #添加拟合线
  geom_smooth(formula = "y ~ x",aes(x=X3.S,y=PTS),method = "loess",size=1)+
  #添加上误差线
  geom_abline(slope = 1.15,intercept = .05,linetype = "dashed",size=1) +
  #绘制下误差线
  geom_abline(slope = .85,intercept = -.05,linetype = "dashed",size=1) +
  #绘制对角线
  geom_abline(slope = 1,intercept = 0,color='black',linetype = "dashed",size=1)+
  #修改坐标做
  scale_x_continuous(limits = c(0,50),breaks = seq(0,50,10)) +
  scale_y_continuous(limits = c(80,130),breaks = seq(0,130,50))+
  guides(fill = guide_colorbar(title = "Counts", title.position = "top",title.hjust = .5,ticks = T))+
  labs(
    x ="X3.S",y = "PTS",
    title = "The scatter chart_pir of Train data and Test data",
    subtitle = "scatter R-ggplot2 Exercise(no color)",
    caption = 'Visualization by DataCharm') 

plot1


#plot2
palette<-brewer.pal(11,"Spectral")
#添加拟合线、图序号
plot2 <- ggplot(df.x3_RG,aes(x=X3.S,y=PTS))+
  stat_bin_2d(binwidth = c(0.4,0.4))+
  scale_fill_gradientn(colours = rev(palette),limits=c(0,12), breaks=c(0,3,6,9,12),
                       labels=c("0","3",'6','9','>12'))+
  #绘制拟合线
  geom_smooth(formula = "y ~ x",aes(x=X3.S,y=PTS),method = "loess",size=1)+
  #添加上误差线
  geom_abline(slope = 1.15,intercept = .05,linetype = "dashed",size=1) +
  #绘制下误差线
  geom_abline(slope = .85,intercept = -.05,linetype = "dashed",size=1) +
  #绘制对角线
  geom_abline(slope = 1,intercept = 0,color='black',linetype = "dashed",size=1)+
  #修改坐标做
  scale_x_continuous(limits = c(0,50),breaks = seq(0,50,10)) +
  scale_y_continuous(limits = c(80,130),breaks = seq(0,130,50))+
  guides(fill = guide_colorbar(title = "Counts", title.position = "top",title.hjust = .5,ticks = T))+
  labs(
    x ="X3.S",y = "PTS",
    title = "The scatter chart_pir of Train data and Test data",
    subtitle = "scatter R-ggplot2 Exercise(no color)",
    caption = 'Visualization by DataCharm') 

plot2

#问题二寻找投三分球前16的球队进入进入季后赛的次数

syear <- unique(df$Season)
both_X3.S_Playoff <- data.frame( Season=rep(NA,35),
                             both_count=rep(NA,35))
  
rela_X3.S_Playoff <- function(S){
  data_need_1 <- df[df$Season==S,]
  #print(S)
  #查看该赛季得分前5
  X3.S16 <- data_need_1[order(-data_need_1$X3.S),]
  X3.S16 <- X3.S16[1:16,]
  both_count<-0
  for(i in 1:nrow(X3.S16)){
    if(X3.S16[i,'Playoffs']==1){
      both_count <- both_count+1
    }
  }
  return (both_count)
}

for (z in 1:length(syear)){
  #print(z)
  Season <- syear[z]
  #print(Season)
  both_X3.S_Playoff[z,'both_count'] <- rela_X3.S_Playoff(Season)
  both_X3.S_Playoff[z, 'Season'] <- Season
  #print(both_pts5_wr5[z,'both_count'])
}

#???
plot(both_X3.S_Playoff$Season,both_X3.S_Playoff$both_count,pch=15,col="DarkTurquoise",xlim = c(0,35),ylim=c(0,16),xaxt="n")
lines(both_X3.S_Playoff$both_count,col="DarkTurquoise",lty=1)#lty=1表示用实线连起来
axis(1,both_X3.S_Playoff$Season,at=1:35,las=3)



#问题三
#投三分球前5的球队对应的胜率统计，平均排名
#首先提取出单个赛季，再提取赛季前五的球队，再得到赛季前五的球队在该赛季的排名，最终计算平均排名，再可视化处理
syear <- unique(df$Season)
summary_rank <- data.frame( Season=rep(NA,35),
                                 rank=rep(NA,35))

rela_X3.S_rank <- function(S){
  data_need_4 <- df[df$Season==S,]
  data_need_4 <- data_need_4[order(data_need_4$WR, decreasing = T), ]
  data_need_4$rank <- seq(1, nrow(data_need_4))#添加排名列
  
  X3.S5 <- data_need_4[order(-data_need_4$X3.S),]
  X3.S5 <- X3.S5[1:5,]
  mean.rank <- mean(X3.S5$rank)
  return (mean.rank)
}

for (z in 1:length(syear)){
  Season <- syear[z]
  summary_rank[z,'rank'] <- rela_X3.S_rank(Season)
  summary_rank[z, 'Season'] <- Season
} 

summary_rank
Season <- syear[1]

plot(summary_rank$Season,summary_rank$rank,pch=15,col="DarkTurquoise",xlim = c(0,35),ylim=c(0,16),xaxt="n")
lines(summary_rank$rank,col="DarkTurquoise",lty=1)#lty=1表示用实线连起来
axis(1,summary_rank$Season,at=1:35,las=3)

#由于14-15排名明显升高，因此对14-15数据进行分析
nba_shots<-read.csv("C:/Users/20325/Desktop/R语言期末作业/shot_logs.csv")

#14-15年球员投篮出手距离分布
ggplot(nba_shots,aes(SHOT_DIST))+geom_histogram(binwidth = 1)

#14-15投篮时命中投篮数与未命中投篮数与投篮位置的关系，其中15英寸代表罚球线；22英寸代表三分线
ggplot(nba_shots,aes(SHOT_DIST, CLOSE_DEF_DIST))+
  geom_point(aes(color=factor(SHOT_RESULT)))+ 
  geom_vline(xintercept=c(15,22),color="blue")




#问题四将所有球队投三分球的前8名的球队标记为smallball，将所有球队投三分球后8名的球队标记为bigball,把其余的球队标记为originalball
df.small_ball <- data.frame( Season=rep(NA,35),
                             rank=rep(NA,35),
                             PTS=rep(NA,35),
                             WR=rep(NA,35),
                             ALL=rep(NA,35),
                             ORB=rep(NA,35),
                             DRB=rep(NA,35),
                             STL=rep(NA,35),
                             BLK=rep(NA,35))

df.original_ball <- data.frame( Season=rep(NA,35),
                             rank=rep(NA,35),
                             PTS=rep(NA,35),
                             WR=rep(NA,35),
                             ALL=rep(NA,35),
                             ORB=rep(NA,35),
                             DRB=rep(NA,35),
                             STL=rep(NA,35),
                             BLK=rep(NA,35))

df.big_ball <- data.frame( Season=rep(NA,35),
                           rank=rep(NA,35),
                           PTS=rep(NA,35),
                           WR=rep(NA,35),
                           ALL=rep(NA,35),
                           ORB=rep(NA,35),
                           DRB=rep(NA,35),
                           STL=rep(NA,35),
                           BLK=rep(NA,35))

syear <- unique(df$Season)
for (z in 1:length(syear)){
  Season <- syear[z]
  
  data_test <- df[df$Season==Season,]
  
  data_test <- data_test[order(data_test$WR, decreasing = T), ]
  data_test$rank <- seq(1, nrow(data_test))#添加胜率排名列
  
  data_test <- data_test[order(data_test$X3.S, decreasing = T), ]
  data_test$rank_3 <- seq(1, nrow(data_test))#添加三分球出手排名列
  
  data_test$mark <- factor(data_test$rank_3 <9 ,levels = c(TRUE,FALSE),labels= c("small ball","big ball") )
  data_small <- data_test[data_test$mark == "small ball",]#构建单赛季small ball数据框
  
  data_big <- data_test[data_test$mark == "big ball",]
  data_big$mark <- factor(data_big$rank_3 <nrow(data_test)-7 ,levels = c(TRUE,FALSE),labels= c("original ball","big ball") )
  data_original <- data_big[data_big$mark == "original ball",]#构建单赛季original ball数据框
  
  data_big <- data_big[data_big$mark == "big ball",]#构建单赛季big ball数据框

  
  df.small_ball[z,'BLK'] <- mean(data_small$BLK)
  df.small_ball[z,'STL'] <- mean(data_small$STL)
  df.small_ball[z,'DRB'] <- mean(data_small$ORB)
  df.small_ball[z,'ORB'] <- mean(data_small$DRB)
  df.small_ball[z,'ALL'] <- mean(data_small$ALL.S)
  df.small_ball[z,'WR'] <- mean(data_small$WR)
  df.small_ball[z,'PTS'] <- mean(data_small$PTS)
  df.small_ball[z,'rank'] <- mean(data_small$rank)
  df.small_ball[z, 'Season'] <- Season
  
  df.original_ball[z,'BLK'] <- mean(data_original$BLK)
  df.original_ball[z,'STL'] <- mean(data_original$STL)
  df.original_ball[z,'DRB'] <- mean(data_original$ORB)
  df.original_ball[z,'ORB'] <- mean(data_original$DRB)
  df.original_ball[z,'ALL'] <- mean(data_original$ALL.S)
  df.original_ball[z,'WR'] <- mean(data_original$WR)
  df.original_ball[z,'rank'] <- mean(data_original$rank)
  df.original_ball[z,'PTS'] <- mean(data_original$PTS)
  df.original_ball[z, 'Season'] <- Season
  
  df.big_ball[z,'BLK'] <- mean(data_big$BLK)
  df.big_ball[z,'STL'] <- mean(data_big$STL)
  df.big_ball[z,'DRB'] <- mean(data_big$ORB)
  df.big_ball[z,'ORB'] <- mean(data_big$DRB)
  df.big_ball[z,'ALL'] <- mean(data_big$ALL.S)
  df.big_ball[z,'WR'] <- mean(data_big$WR)
  df.big_ball[z,'PTS'] <- mean(data_big$PTS)
  df.big_ball[z,'rank'] <- mean(data_big$rank)
  df.big_ball[z, 'Season'] <- Season
  
}

df.small_ball
df.original_ball
df.big_ball

class(df.small_ball$rank)
class(df.small_ball$Season)

plot(df.small_ball$Season,df.small_ball$rank,pch=16,col="DeepPink",
     xlim=c(1985,2020),ylim=c(6,24), xaxt='n',
     xlab = "season",ylab= "两分球/三分球出手数",
     cex=1, type = "b")
axis(1, at=1985:2020,las=3)    #设置X轴坐标刻度
points(df.original_ball$Season,df.original_ball$rank,pch=17,col="DarkTurquoise",cex=1, type = "b")
points(df.big_ball$Season,df.big_ball$rank,pch=18,col="RosyBrown",cex=1, type = "b")
#图例说明
legend(1990,24,c("small ball","original ball","big ball"),col=c("DeepPink","DarkTurquoise","RosyBrown"),text.col=c("DeepPink","DarkTurquoise","RosyBrown"),pch=c(16,17,18),xpd = TRUE)


#问题五检测需要赢得多少场胜率才能进季后赛
table(df$Playoffs,df$W)

#由于有些赛季未进行82场比赛，因此排除未进行完82场比赛的赛季，对所有进行完整赛季的数据进行分析
df.82 <- df[df$NOG == "82",]
table(df$Playoffs,df$W)

ggplot(data=df.82, aes(x=W,y=TEAM,col=Playoffs)) + 
  geom_point(aes(colour=Playoffs))



