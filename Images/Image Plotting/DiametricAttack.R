Value<-function(TimeHorizon,AttackTime,dbar)
{
  Start=AttackTime-dbar
  Middle=max(0,(AttackTime*(floor((TimeHorizon-2*AttackTime+1)/dbar)+1)))
  End=max(0,(TimeHorizon-(AttackTime-1+dbar*(floor((TimeHorizon-2*AttackTime+1)/dbar)+1))))+max(0,(TimeHorizon-(AttackTime-1+dbar*(floor((TimeHorizon-2*AttackTime+1)/dbar)+2))))
  Total=Start+Middle+End
  NumberOfGames=2*(TimeHorizon-AttackTime+1)
  min(1,Total/NumberOfGames)
}

StartHorizon=45
Attack=45
d=30

l=400

V=data.frame(nrow=l,ncol=2) #Value as predicted by start,middle,end
for(i in 1:l)
{
  V[i,1]=i+StartHorizon
  V[i,2]=Value(i+StartHorizon,Attack,d)
}

PV=data.frame(nrow=l,ncol=2) #Values Predicted as m/2d
for(i in 1:l)
{
  PV[i,1]=i+StartHorizon
  PV[i,2]=Attack/(2*d)
}

I=data.frame(nrow=l) #When are values the same store the time
IP=data.frame(nrow=l,ncol=2) #When they are the same store the point
for(i in 1:l)
{
  if(V[i,2]==PV[i,2])
  {
    I[i]=i+StartHorizon
    IP[i,1]=i+StartHorizon
    IP[i,2]=V[i,2]
  }
  else
  {
    I[i]=0
    IP[i,1]=NaN
    IP[i,2]=NaN
  }
}

M=data.frame(nrow=l) #When are values the maximum in the region
MP=data.frame(nrow=l,ncol=2) #When they are store the point
for(i in 1:l)
{
 if((i+StartHorizon-2*Attack+1) %% d==0)
 {
   M[i]=1
   MP[i,1]=i+StartHorizon
   MP[i,2]=V[i,2]
 }
 else
 {
   M[i]=0
   MP[i,1]=NaN
   MP[i,2]=NaN
 }   
}

library(tikzDevice)
library(ggplot2)

df<-data.frame(V,PV)

tikz(file=paste("/maths/pg/pmxtol/Documents/Git Repositories/2nd-Year-Report/Images/DiametricAttack(m_",toString(Attack),",d_",toString(d),").tex",sep=""),width=4.8,height=3)

plot1<-ggplot(df,show.legend='True')+
geom_point(aes(x = V[,1], y = V[,2]))+geom_line(aes(x = V[,1], y = V[,2]))+
geom_line(aes(x = PV[,1], y = PV[,2]))+geom_point(aes(x=IP[,1],y=IP[,2]),color='red')+
geom_point(aes(x=MP[,1],y=MP[,2]),colour='blue')+
geom_smooth(aes(x=MP[,1],y=MP[,2]),method = "lm", formula = y ~ splines::bs(x,10), se = FALSE)+
xlab("Time Horizon, $T$")+ylab("Upper Bound")
#theme(axis.title = element_text(face="bold", size=20),axis.text= element_text(size=20))

  print(plot1)
dev.off()
  
#ggsave(filename = "DiametricAttack(m_45,d_30).png",path="/local/pmxtol/Dropbox/R",plot=plot1,dpi=600)