data<-read.table("C:/Users/12345/Desktop/TABLE.txt", header=T)

chemical<-data[1:36,-6]
normal<-data[37:112,-6]
overt<-data[113:145,-6]

#group:chemical

x11<-chemical$x1*{2}
x22<-chemical$x2*{2}
x33<-chemical$x3*{2}
x12<-chemical$x1*chemical$x2
x13<-chemical$x1*chemical$x3
x23<-chemical$x2*chemical$x3

RM.ch<-lm(cbind(y1,y2)~x1+x2+x3,data=chemical)

FM.ch<-lm(cbind(y1,y2)~x1+x2+x3+x11+x22+x33+x12+x13+x23,data=chemical)

anova(RM.ch,FM.ch,test="Wilks")


#group:normal


X11<-normal$x1*{2}
X22<-normal$x2*{2}
X33<-normal$x3*{2}
X12<-normal$x1*normal$x2
X13<-normal$x1*normal$x3
X23<-normal$x2*normal$x3

RM.normal<-lm(cbind(y1,y2)~x1+x2+x3,data=normal)

FM.normal<-lm(cbind(y1,y2)~x1+x2+x3+X11+X22+X33+X12+X13+X23,data=normal)

anova(RM.normal,FM.normal,test="Wilks")

#group:overt

x11<-overt$x1*{2}
x22<-overt$x2*{2}
x33<-overt$x3*{2}
x12<-overt$x1*overt$x2
x13<-overt$x1*overt$x3
x23<-overt$x2*overt$x3

RM.overt<-lm(cbind(y1,y2)~x1+x2+x3,data=overt)

FM.overt<-lm(cbind(y1,y2)~x1+x2+x3+x11+x22+x33+x12+x13+x23,data=overt)

anova(RM.overt,FM.overt,test="Wilks")