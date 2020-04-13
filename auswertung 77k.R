K77<- read.csv("Mitttelwerte.csv",header = T,sep=",")


for (i in c(2:length(K77))){
  K77[,i]<-K77[,i]-mean(K77[0:100,i])
  slope <- (mean(K77[240:260,i])-mean(K77[0:40,i]))/230
  slope1 <- seq(1,995)
  slope1<-slope1*slope
  K77[,i]<-K77[,i]-slope1
  plot(K77[,i]~K77[,1],typ="l",main=names(K77[i]))
  K77W<-K77[,i]
  PSIIa<-c(K77W[348:413])
  x<-length(PSIIa)-1
  for (a in c(1:x)){
    PSIIa<-c(PSIIa,PSIIa[x-a])
  }
  K77W[348:477]<-K77W[348:477]-PSIIa
  
  
  PSIIb<-c(K77W[414:463])
  x<-length(PSIIb)-1
  for (a in c(1:x)){
    PSIIb<-c(PSIIb,PSIIb[x-a])
  }
  K77W[414:511]<-K77W[414:511]-PSIIb
  if (sum(K77W)>0){
  PSI<-c(K77W[415:match(max(K77W),K77W)])

  x<-length(PSI)-1
  for (a in c(1:x)){
    PSI<-c(PSI,PSI[x-a])
  }
  K77W[415:(414+length(PSI))]<-  K77W[415:(414+length(PSI))]-PSI
  }
  else{
    PSI<-0
  }
  lines(PSIIa~K77[348:477,1],col=2)
  lines(PSIIb~K77[414:511,1],col=3)
  lines(PSI~K77[415:(415+length(PSI)-1),1],col=4)
  lines(K77W~K77[,1])
  print(paste(names(K77)[i],sum(PSIIa),sum(PSIIb),sum(PSI),(sum(PSI)/96)/(sum(PSIIa,PSIIb)/35)))
  }

