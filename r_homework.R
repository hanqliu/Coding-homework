data<-read.csv('D:/应用统计专题/作业/tvdata.csv')
summary(data)
options(repr.plot.width=20, repr.plot.height=8)
library('ggplot2')
library('car')
data[518,'单集片长']=0
hist(as.numeric(data$单集片长),xlab='单集片长/min',main='')
len<-as.numeric(data$单集片长)
len[which(len<25)]=0
len[which(len>=25&len<45)]=1
len[which(len>=45)]=2
data$总时长<-as.numeric(data$单集片长)*as.numeric(data$集数)
hist(data$总时长,xlab='剧集总时长',main='')
options(repr.plot.width=20, repr.plot.height=8)
hist(data$评价人数,xlab='评价人数',main='')
summary(data$评价人数)
hist(data$评价人数[which(data$评价人数<13183)],xlab='评价人数(少于13183)',main='')
da<-data[which(data$评价人数<13183),]
library('ggplot2')
d<-data.frame(data$豆瓣评分,data$总时长,data$评价人数)
which(is.na(d))
km<-kmeans(d,4)
#原数据中共分为4类
k=t(km[1])
d$kmeans<-as.factor(unlist(k))
dt<-data.frame(d,data$类型)
ggplot(data=dt, aes(data.豆瓣评分,data.总时长,color=data.类型,shape=kmeans))+geom_point(size=2)+
  labs(x='豆瓣评分',y='总长度')
dtt=dt[which((dt$data.豆瓣评分>6)&(dt$data.总时长<1500)),]
ggplot(data=dtt, aes(data.豆瓣评分,data.总时长,color=data.类型,shape=kmeans))+geom_point(size=3)+
  labs(x='豆瓣评分',y='总时长')
summary(as.factor(data$类型))
summary(as.factor(unlist(k)))
ac1<-(sum(dt[which(dt$data.类型=='喜剧'),'kmeans']==1)+sum(dt[which(dt$data.类型=='纪录'),'kmeans']==2)+sum(dt[which(dt$data.类型=='其他'),'kmeans']==3)+sum(dt[which(dt$data.类型=='剧情'),'kmeans']==4))/801
ac2<-(sum(dt[which(dt$data.类型=='其他'),'kmeans']==1)+sum(dt[which(dt$data.类型=='纪录'),'kmeans']==2)+sum(dt[which(dt$data.类型=='喜剧'),'kmeans']==3)+sum(dt[which(dt$data.类型=='剧情'),'kmeans']==4))/801
print(max(ac1,ac2))
da<-data[which(data$评价人数<13183),]
l<-lm(da$豆瓣评分~da$评价人数)
summary(l)
plot(l)
m<-lm(da$豆瓣评分*100~da$总时长)
summary(m)
plot(m)
le<-lm(da$豆瓣评分~as.factor(da$类型))
summary(le)
ggplot(da, aes(x=类型, y=豆瓣评分)) + geom_boxplot()
ll<-le<-lm(da$豆瓣评分~as.factor(da$语言))
summary(ll)
ggplot(da, aes(x=语言, y=豆瓣评分)) + geom_boxplot()
fit<-lm(da$豆瓣评分~as.factor(da$类型)+da$评价人数+as.factor(da$语言)+da$总时长)
summary(fit)
Anova(fit,type='III')
lm.aic=step(fit,trace=F)
summary(lm.aic)
lm.bic=step(fit,k=log(100),trace=F)
summary(lm.bic)
plot(fit)
