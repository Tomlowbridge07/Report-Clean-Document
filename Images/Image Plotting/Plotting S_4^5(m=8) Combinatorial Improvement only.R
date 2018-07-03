#load libraries
library(ggplot2)
library(tikzDevice)
options(
  tikzMetricPackages = c(
    "\\usepackage{bm}\n",
    "\\usetikzlibrary{calc}\n"
  ),
  tikzUnicodeMetricPackages = c(
    "\\usepackage{bm}\n",
    "\\usetikzlibrary{calc}\n"
  )
)

A=data.frame(nrow=8,ncol = 2)
A[1,1]="1"
A[2,1]="2"
A[3,1]="3"
A[4,1]="4"
A[5,1]="5"
A[6,1]="6"
A[7,1]="c"
A[8,1]="*"

A[1,2]=8/18
A[2,2]=10/18
A[3,2]=12/18
A[4,2]=14/18
A[5,2]=16/18
A[6,2]=14/18
A[7,2]=14/18
A[8,2]=8/18

B=A
B[1,2]=(9/17)*A[1,2]+(4/17)
B[2,2]=(9/17)*A[2,2]+(4/17)
B[3,2]=(9/17)*A[3,2]+(4/17)
B[4,2]=(9/17)*A[4,2]+(4/17)
B[5,2]=(9/17)*A[5,2]
B[6,2]=(9/17)*A[6,2]+(4/17)
B[7,2]=(9/17)*A[7,2]+(4/17)
B[8,2]=(9/17)*A[8,2]+(4/17)

X=vector(length=8)
X=1:8

df<-data.frame(A,B)

#Images will be size
w=4.8
h=3


tikz(file=paste("/maths/pg/pmxtol/Documents/Git Repositories/2nd-Year-Report/Images/CombinatorialInterceptionOnS_4^5(m=8).tex",sep=""),width=w,height=h)
ggplot(df,show.legend='True') + geom_point(aes(x = X, y = A[,2])) +geom_line(aes(x = X, y = A[,2]))+
  geom_point(aes(x = X, y = B[,2]),colour='blue')+geom_line(aes(x = X, y = B[,2]),colour='blue')+
  ylab("Probability, \\textcolor{blue}{$P(\\bm{\\pi}_{C}(\\frac{2}{13},\\frac{2}{13}),i)$}")+xlab("Node, $i$")+
  #theme(axis.title = element_text(face="bold", size=30),axis.text= element_text(size=30))+
  scale_x_discrete(limits=c("$1$","$2$","$3$","$4$","$5$","$6$","$c$","$*$"))
dev.off()