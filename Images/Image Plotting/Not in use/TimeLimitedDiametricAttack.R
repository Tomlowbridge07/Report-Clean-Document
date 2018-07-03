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

haltpoint=Attack+(d+1)-2

l=400

V=data.frame(nrow=l,ncol=2) #Value as predicted by start,middle,end
for(i in 1:l)
{
  V[i,1]=i+StartHorizon
  if(i+StartHorizon >= haltpoint)
  {
    V[i,2]=Attack/(2*d)
  }
  else
  {
    V[i,2]=Value(i+StartHorizon,Attack,d)
  }
  
}

PV=data.frame(nrow=l,ncol=2) #Values Predicted as m/2d
for(i in 1:l)
{
  PV[i,1]=i+StartHorizon
  PV[i,2]=Attack/(2*d)
}




library(tikzDevice)
library(ggplot2)

df<-data.frame(V,PV)

tikz(file=paste("/local/pmxtol/Dropbox/R/TimeLimitedDiametricAttack(m_",toString(Attack),",d_",toString(d),").tex",sep=""),width=6,height=4)

plot1<-ggplot(df,show.legend='True')+
geom_point(aes(x = V[,1], y = V[,2]))+geom_line(aes(x = V[,1], y = V[,2]))+
geom_line(aes(x = PV[,1], y = PV[,2]))+
xlab("Time Horizon, $T$")+ylab("Upper Bound")
#theme(axis.title = element_text(face="bold", size=20),axis.text= element_text(size=20))

  print(plot1)
dev.off()
  
#ggsave(filename = "DiametricAttack(m_45,d_30).png",path="/local/pmxtol/Dropbox/R",plot=plot1,dpi=600)