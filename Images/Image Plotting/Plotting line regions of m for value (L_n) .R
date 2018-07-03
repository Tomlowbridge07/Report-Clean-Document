library(ggplot2)
library(tikzDevice)

options(tz="UK")

n=10
extend=2

#forming region 1: i.e m>2(n-1)
m1=c(seq(2*(n-1)+1,2*(n-1)+extend,1))
GameValue1=c(rep(1,extend))
mRegion1=matrix(rbind(m1,GameValue1),nrow=extend,ncol=2,byrow=TRUE)

#forming region 2: n-1<m<=2(n-1)
m2=c(seq(n,2*(n-1),1))
GameValue2=m2*(1/(2*(n-1)))
mRegion2=matrix(rbind(m2,GameValue2),nrow=n-1,ncol=2,byrow=TRUE)

mRegion=rbind(mRegion1,mRegion2)

#forming region 3: i.e m=2 and n>=3
if(n>=3)
{
  m3=c(2)
  GameValue3=c(1/ceiling(n/2))
  mRegion3=matrix(rbind(m3,GameValue3),nrow=1,ncol=2,byrow=TRUE)
  
  mRegion=rbind(mRegion,mRegion3)
}

#forming region 4: i.e m=n-1 or m=n-2 and even
#m=n-1
m4a=c(n-1)
GameValue4a=c(1/2)
mRegion4a=matrix(rbind(m4a,GameValue4a),nrow=1,ncol=2,byrow=TRUE)

mRegion=rbind(mRegion,mRegion4a)

#m=n-2 and even
if((n-2) %% 2==0 && (n-2)>=3)
{
  print("This code is run")
  m4b=c(n-2)
  GameValue4b=c(1/2)
  mRegion4b=matrix(rbind(m4b,GameValue4b),nrow=1,ncol=2,byrow=TRUE)
  
  mRegion=rbind(mRegion,mRegion4b)
}

#forming region 5: i.e m<=n-3 or m=n-2 and odd
#m=n-2 and odd
if((n-2) %% 2==1 && n-2>=3)
{
  m5a=c(n-2)
  GameValue5a=m5a*(1/(m5a+n-1))
  mRegion5a=matrix(rbind(m5a,GameValue5a),nrow=1,ncol=2,byrow=TRUE)
  
  mRegion=rbind(mRegion,mRegion5a)
  
}
#m<=n-3
m5b=c(seq(3,n-3))
GameValue5b=m5b*(1/(m5b+n-1))
mRegion5b=matrix(rbind(m5b,GameValue5b),nrow=n-5,ncol=2,byrow=TRUE)

mRegion=rbind(mRegion,mRegion5b)

#forming region for m=1
m5c=c(1)
GameValue5c=c(1/(n))
mRegion5c=matrix(rbind(m5c,GameValue5c),nrow=1,ncol=2,byrow=TRUE)

mRegion=rbind(mRegion,mRegion5c)

FinalRegion=mRegion

size=nrow(mRegion)
mAltRegion1=rbind(mRegion1,matrix(rep(NaN,2*(size-nrow(mRegion1))),nrow=size-nrow(mRegion1),ncol=2,byrow=TRUE))

#Plotting graph
df<-as.data.frame(mRegion)
df1<-as.data.frame(mRegion1)

tikz(file=paste("/maths/pg/pmxtol/Documents/Git Repositories/2nd-Year-Report/Images/LineRegions(L_",toString(n),").tex",sep=""),width=4.8,height=3)

valueplot<-ggplot(df,show.legend='True')+
  geom_point(aes(x = mRegion[,1], y = mRegion[,2]))+
  geom_line(aes(x = mRegion[,1], y = mRegion[,2]))+
  #geom_point(aes(x = mAltRegion1[,1], y = mAltRegion1[,2]),color='red')+
  #geom_line(aes(x = mAltRegion1[,1], y = mAltRegion1[,2]),color='red')+
  annotate("rect",xmin=18.55,xmax=20.45,ymin=0.00,ymax=1.05,color='yellow',fill='yellow',alpha=.2)+
  annotate("rect",xmin=9.55,xmax=18.45,ymin=0.00,ymax=1.05,color='red',fill='red',alpha=.2)+
  annotate("rect",xmin=7.55,xmax=9.45,ymin=0.00,ymax=1.05,color='purple',fill='purple',alpha=.2)+
  annotate("rect",xmin=2.55,xmax=7.45,ymin=0.00,ymax=1.05,color='blue',fill='blue',alpha=.2)+
  annotate("rect",xmin=1.55,xmax=2.45,ymin=0.00,ymax=1.05,color='green',fill='green',alpha=.2)+
  annotate("rect",xmin=0.55,xmax=1.45,ymin=0.00,ymax=1.05,color='blue',fill='blue',alpha=.2)+
  xlab("Attack time, $m$")+ylab(paste("Value, $V(L_{",toString(n),"})$"))
  #theme(axis.title = element_text(face="bold", size=20),axis.text= element_text(size=20))
  
print(valueplot)
dev.off()  
  
#ggsave(filename = "LineRegions(L_10).png",path="/local/pmxtol/Dropbox/R",plot=valueplot,dpi=600)
