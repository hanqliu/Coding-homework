data<-read.csv('D:/Ӧ��ͳ��ר��/��ҵ/tvdata.csv')
summary(data)
options(repr.plot.width=20, repr.plot.height=8)
library('ggplot2')
library('car')
data[518,'����Ƭ��']=0
hist(as.numeric(data$����Ƭ��),xlab='����Ƭ��/min',main='')
len<-as.numeric(data$����Ƭ��)
len[which(len<25)]=0
len[which(len>=25&len<45)]=1
len[which(len>=45)]=2
data$��ʱ��<-as.numeric(data$����Ƭ��)*as.numeric(data$����)
hist(data$��ʱ��,xlab='�缯��ʱ��',main='')
options(repr.plot.width=20, repr.plot.height=8)
hist(data$��������,xlab='��������',main='')
summary(data$��������)
hist(data$��������[which(data$��������<13183)],xlab='��������(����13183)',main='')
da<-data[which(data$��������<13183),]
library('ggplot2')
d<-data.frame(data$��������,data$��ʱ��,data$��������)
which(is.na(d))
km<-kmeans(d,4)
#ԭ�����й���Ϊ4��
k=t(km[1])
d$kmeans<-as.factor(unlist(k))
dt<-data.frame(d,data$����)
ggplot(data=dt, aes(data.��������,data.��ʱ��,color=data.����,shape=kmeans))+geom_point(size=2)+
  labs(x='��������',y='�ܳ���')
dtt=dt[which((dt$data.��������>6)&(dt$data.��ʱ��<1500)),]
ggplot(data=dtt, aes(data.��������,data.��ʱ��,color=data.����,shape=kmeans))+geom_point(size=3)+
  labs(x='��������',y='��ʱ��')
summary(as.factor(data$����))
summary(as.factor(unlist(k)))
ac1<-(sum(dt[which(dt$data.����=='ϲ��'),'kmeans']==1)+sum(dt[which(dt$data.����=='��¼'),'kmeans']==2)+sum(dt[which(dt$data.����=='����'),'kmeans']==3)+sum(dt[which(dt$data.����=='����'),'kmeans']==4))/801
ac2<-(sum(dt[which(dt$data.����=='����'),'kmeans']==1)+sum(dt[which(dt$data.����=='��¼'),'kmeans']==2)+sum(dt[which(dt$data.����=='ϲ��'),'kmeans']==3)+sum(dt[which(dt$data.����=='����'),'kmeans']==4))/801
print(max(ac1,ac2))
da<-data[which(data$��������<13183),]
l<-lm(da$��������~da$��������)
summary(l)
plot(l)
m<-lm(da$��������*100~da$��ʱ��)
summary(m)
plot(m)
le<-lm(da$��������~as.factor(da$����))
summary(le)
ggplot(da, aes(x=����, y=��������)) + geom_boxplot()
ll<-le<-lm(da$��������~as.factor(da$����))
summary(ll)
ggplot(da, aes(x=����, y=��������)) + geom_boxplot()
fit<-lm(da$��������~as.factor(da$����)+da$��������+as.factor(da$����)+da$��ʱ��)
summary(fit)
Anova(fit,type='III')
lm.aic=step(fit,trace=F)
summary(lm.aic)
lm.bic=step(fit,k=log(100),trace=F)
summary(lm.bic)
plot(fit)