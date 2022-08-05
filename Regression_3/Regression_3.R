library(car)
data<-read.table("C:/Users/12345/Desktop/TABLE.txt",header = T)


chemical<-data[1:36,-6]
normal<-data[37:112,-6]
overt<-data[113:145,-6]

#group:chemical
(conf.y1=predict(lm(y1~x1+x2+x3,data=chemical),list(x1=413,x2=344,x3=270),interval ='confidence'))
(conf.y2=predict(lm(y2~x1+x2+x3,data=chemical),list(x1=413,x2=344,x3=270),interval ='confidence'))

(conf.y1=predict(lm(y1~x1+x2+x3,data=chemical),list(x1=493,x2=288,x3=208),interval ='prediction'))
(conf.y2=predict(lm(y2~x1+x2+x3,data=chemical),list(x1=493,x2=288,x3=208),interval ='prediction'))


ch.Model=lm(cbind(y1,y2) ~ x1+x2+x3,data=chemical) 
test.coef1=linearHypothesis(ch.Model,hypothesis.matrix=c("x1=0","x2=0","x3=0"))

E.ch<-test.coef1$SSPE
H.ch<-test.coef1$SSPH
landa.ch<-det(E.ch)/det(E.ch+H.ch)
(R2.ch=1-landa.ch)
(A.landa=1-landa.ch^2)
E.ch.prim=solve(E.ch)
landa =eigen( E.ch.prim*H.ch)
(teta.ch=min(landa$values)/1+min(landa$values))

#group:normal

(conf.y1=predict(lm(y1~x1+x2+x3,data=normal),list(x1=306,x2=178,x3=66),interval ='confidence'))
(conf.y2=predict(lm(y2~x1+x2+x3,data=normal),list(x1=306,x2=178,x3=66),interval ='confidence'))


(conf.y1=predict(lm(y1~x1+x2+x3,data=normal),list(x1=349,x2=172,x3=114),interval ='prediction'))
(conf.y2=predict(lm(y2~x1+x2+x3,data=normal),list(x1=349,x2=172,x3=114),interval ='prediction'))


N.Model=lm(cbind(y1,y2)~x1+x2+x3, data=normal)

test.coef2<-linearHypothesis(N.Model,hypothesis.matrix=c("x1=0","x2=0","x3=0"))

E.norm=test.coef2$SSPE
H.norm=test.coef2$SSPH
landa.norm=det(E.norm)/det(E.norm+H.norm)
(R2.norm=1-landa.norm)
(A.landa=1-landa.norm^2)
E.norm.prim=solve(E.norm)
landa =eigen( E.norm.prim*H.norm)
(teta.norm=min(landa$values)/1+min(landa$values))



#group:overt

(conf.y1=predict(lm(y1~x1+x2+x3,data=overt),list(x1=849,x2=159,x3=310),interval ='confidence'))
(conf.y2=predict(lm(y2~x1+x2+x3,data=overt),list(x1=849,x2=159,x3=310),interval ='confidence'))


(conf.y1=predict(lm(y1~x1+x2+x3,data=overt),list(x1=1043,x2=106,x3=318),interval ='prediction'))
(conf.y2=predict(lm(y2~x1+x2+x3,data=overt),list(x1=1043,x2=106,x3=318),interval ='prediction'))



O.Model=lm(cbind(y1,y2)~x1+x2+x3, data=overt)

test.coef3<-linearHypothesis(N.Model,hypothesis.matrix=c("x1=0","x2=0","x3=0"))

E.overt=test.coef3$SSPE
H.overt=test.coef3$SSPH
landa.overt=det(E.overt)/det(E.overt+H.overt)
(R2.overt=1-landa.overt)
(A.landa=1-landa.overt^2)
E.overt.prim=solve(E.overt)
landa =eigen( E.overt.prim*H.overt)
(teta.overt=min(landa$values)/1+min(landa$values))