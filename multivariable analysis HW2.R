#�������ּ׳����壬�����ֵ������Э�����δ֪�����ǻ������Ͼ������С�����б�׼��
#�ȶ�ȡ����
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
  nx <- nrow(TstX)    # ��Ҫ����Ԥ��ļ��ϵĴ�С������˵���Լ��Ĵ�С
  # ���ɳ���Ϊnx��0���������Դ洢Ԥ��ı�ǩ
  blong <- matrix(rep(0,nx),nrow=1,byrow=TRUE,dimnames=list("blong",1:nx))
  # ����Ⱥ��ľ�ֵ����
  mu1 <- colMeans(Train1); mu2 <- colMeans(Train2) 
  # ��Ⱥ��ͬ����
  if (var.equal == TRUE || var.equal == T){
    # ��������������
    S <- var(rbind(Train1,Train2))
    # ���ڶ�Ⱥ������Ͼ����ȥ����һȺ������Ͼ��롪��>�б���W(x)
    W <- mahalanobis(TstX, mu2, S) - mahalanobis(TstX, mu1, S)
  }
  # ��Ⱥ���췽��
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
#����������ͬ����ʱ��w
S <- var(rbind(deracea,carduorum))
mu1 <- colMeans(deracea); mu2 <- colMeans(carduorum)
TstX<-rbind(deracea,carduorum)
W <- mahalanobis(TstX, mu2, S) - mahalanobis(TstX, mu1, S)
print(W)
#���������巽�ͬʱ��w
S1 <- var(Train1); S2 <- var(Train2)
W <- mahalanobis(TstX, mu2, S2) - mahalanobis(TstX, mu1, S1)
print(W)
#�б���
discriminant.distance(carduorum,deracea)
discriminant.distance(carduorum,deracea,var.equal = TRUE)
#��С���뷨������Ϊ0

#bayes�б�
#Bayes�б�����
lossrate=1
rate=(2/3)/(1/3)
#����Э������ͬʱ
beta <- log(1/rate)+log(lossrate)
beta
#����Э���ͬʱ
beta <- log(1/rate) +log(lossrate)+ 0.5*log(det(S1)/det(S2))
beta
discriminant.bayes <- function(TrnX1,TrnX2,rate=1,lossrate=1,TstX=NULL,var.equal=FALSE){
  TstX<-rbind(TrnX1,TrnX2)
  nx<-nrow(TstX)
  blong <- matrix(rep(0,nx), nrow=1, byrow=TRUE, dimnames = list("blong",1:nx))
  mu1 <- colMeans(TrnX1); mu2 <- colMeans(TrnX2)
  if (var.equal == TRUE || var.equal == T){
    S <- var(rbind(TrnX1,TrnX2))
    beta <- log(1/rate)+log(lossrate)   # Wǰ���1/2�˵�beta����ȥ��
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
#����Э���ͬ
discriminant.bayes(deracea,carduorum,rate=2,lossrate=1)
#����Э������ͬ
discriminant.bayes(deracea,carduorum,rate=2,lossrate=1,var.equal = TRUE)

d<-read.csv('D:/����ʼ�/7��.csv')
#��������ؾ���
pcacor<-princomp(d,cor=TRUE)
summary(pcacor,loading=TRUE)
#������Э������
pcacov<-princomp(d,cor=FALSE)
summary(pcacov,loading=TRUE)