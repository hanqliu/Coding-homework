#对于两种甲虫总体，总体均值和总体协差阵均未知，考虑基于马氏距离的最小距离判别准则
#先读取数据
deracea<-data.frame(
  x1=c(189,192,217,221,171,192,213,192,170,201,195,205,180,192,200,192,200,181,192),
  x2=c(245,260,276,299,239,262,278,255,244,276,242,263,252,283,294,277,287,255,287),
  x3=c(137,132,141,142,128,147,136,128,128,146,128,147,121,138,138,159,136,146,141),
  x4=c(163,217,192,213,158,173,201,185,129,186,192,192,167,183,188,177,173,183,198)
)
carduorum<-data.frame(
  x1=c(181,158,184,171,181,181,177,198,180,177,176,192,176,169,164,181,192,181,175,197),
  x2=c(305,237,300,273,297,297,301,308,286,299,317,312,285,287,265,308,276,278,271,303),
  x3=c(184,133,166,162,160,163,166,142,146,171,166,166,141,162,147,157,154,149,140,170),
  x4=c(209,231,231,213,223,224,221,197,214,192,213,209,200,214,192,204,209,235,192,205)
)
discriminant.distance <- function(Train1,Train2,TstX=NULL,var.equal=FALSE){
  TstX<-rbind(Train1,Train2)
  nx <- nrow(TstX)    # 需要用以预测的集合的大小，或者说测试集的大小
  # 生成长度为nx的0向量，用以存储预测的标签
  blong <- matrix(rep(0,nx),nrow=1,byrow=TRUE,dimnames=list("blong",1:nx))
  # 两个群体的均值向量
  mu1 <- colMeans(Train1); mu2 <- colMeans(Train2) 
  # 两群体同方差
  if (var.equal == TRUE || var.equal == T){
    # 计算混合样本方差
    S <- var(rbind(Train1,Train2))
    # 到第二群体的马氏距离减去到第一群体的马氏距离——>判别函数W(x)
    W <- mahalanobis(TstX, mu2, S) - mahalanobis(TstX, mu1, S)
  }
  # 两群体异方差
  else{
    S1 <- var(Train1); S2 <- var(Train2)
    W <- mahalanobis(TstX, mu2, S2) - mahalanobis(TstX, mu1, S1)
  }  
  for (i in 1:nx){
    if (W[i] > 0)
      blong[i] <- 1
    else
      blong[i] <- 2
  }
  blong
}
#建立两总体同方差时的w
S <- var(rbind(deracea,carduorum))
mu1 <- colMeans(deracea); mu2 <- colMeans(carduorum)
TstX<-rbind(deracea,carduorum)
W <- mahalanobis(TstX, mu2, S) - mahalanobis(TstX, mu1, S)
print(W)
#建立两总体方差不同时的w
S1 <- var(Train1); S2 <- var(Train2)
W <- mahalanobis(TstX, mu2, S2) - mahalanobis(TstX, mu1, S1)
print(W)
#判别结果
discriminant.distance(carduorum,deracea)
discriminant.distance(carduorum,deracea,var.equal = TRUE)
#最小距离法误判率为0

#bayes判别
#Bayes判别区域
lossrate=1
rate=(2/3)/(1/3)
#总体协方差相同时
beta <- log(1/rate)+log(lossrate)
beta
#总体协方差不同时
beta <- log(1/rate) +log(lossrate)+ 0.5*log(det(S1)/det(S2))
beta
discriminant.bayes <- function(TrnX1,TrnX2,rate=1,lossrate=1,TstX=NULL,var.equal=FALSE){
  TstX<-rbind(TrnX1,TrnX2)
  nx<-nrow(TstX)
  blong <- matrix(rep(0,nx), nrow=1, byrow=TRUE, dimnames = list("blong",1:nx))
  mu1 <- colMeans(TrnX1); mu2 <- colMeans(TrnX2)
  if (var.equal == TRUE || var.equal == T){
    S <- var(rbind(TrnX1,TrnX2))
    beta <- log(1/rate)+log(lossrate)   # W前面的1/2乘到beta上面去了
    W <- 0.5*(mahalanobis(TstX,mu2,S) - mahalanobis(TstX,mu1,S))
  }else{
    S1 <- var(TrnX1); S2 <- var(TrnX2)
    beta <- log(1/rate) +log(lossrate)+ 0.5*log(det(S1)/det(S2))
    W <- 0.5*(mahalanobis(TstX, mu2, S2) - mahalanobis(TstX, mu1, S1))
  }
  
  for (i in 1:nx){
    if (W[i] > beta)
      blong[i] <- 1
    else
      blong[i] <- 2
  }
  blong
}
#总体协方差不同
discriminant.bayes(deracea,carduorum,rate=2,lossrate=1)
#总体协方差相同
discriminant.bayes(deracea,carduorum,rate=2,lossrate=1,var.equal = TRUE)

d<-read.csv('D:/讲义笔记/7题.csv')
#用样本相关矩阵
pcacor<-princomp(d,cor=TRUE)
summary(pcacor,loading=TRUE)
#用样本协方差阵
pcacov<-princomp(d,cor=FALSE)
summary(pcacov,loading=TRUE)
