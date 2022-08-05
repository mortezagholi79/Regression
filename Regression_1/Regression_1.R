
data1<-read.table("C:/Users/12345/Desktop/TABLE.txt",header = T)


chemical=data1[1:36,-6]
normal=data1[37:112,-6]
overt=data1[113:145,-6]

y1:relative.weight
y2:fasting.plasma.glucose

x1:glucose.intolerance
x2:insulin.response
x3:insulin.resistance


ch.Model=lm(cbind(y1,y2) ~ x1+x2+x3,data=chemical) 
coef(ch.Model)

(test.coef1=linearHypothesis(ch.Model,hypothesis.matrix=c("x1=0","x2=0","x3=0")))



N.Model=lm(cbind(y1,y2)~x1+x2+x3, data=normal)
coef(N.Model)

(test.coef2<-linearHypothesis(N.Model,hypothesis.matrix=c("x1=0","x2=0","x3=0")))



O.Model=lm(cbind(y1,y2)~x1+x2+x3, data=overt)
coef(O.Model)
(test.coef3<-linearHypothesis(N.Model,hypothesis.matrix=c("x1=0","x2=0","x3=0")))
