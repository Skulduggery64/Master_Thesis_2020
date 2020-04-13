if("zoo" %in% rownames(installed.packages()) == FALSE) {install.packages("zoo")}
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
if("matrixStats" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixStats")}

require(plyr)
require(zoo)
require(matrixStats)


MSfrO<-c(0.5654,0.7106,1.000,0.4797)#c( 0.5654,0.7106,1.0000,0.4797)#c(0.6166,0.7864,1.0000,0.4242)     #Old model-spectra
MSp700O<-c(0.069042378,	1.159986408,	1,	0.29295087)#c(0.0160,0.8515,1.0000,0.3527)#c(0.1047463, 1.1690437, 1.0000000, 0.2837269)
MSpcO<-c(0.0703280125,	0.51793242,	1	,0.61409297)#c(0.1056,0.4385,1.0000,0.5966)#c(0.0754,0.4113,1.0000,0.6187)

MSfr<-c(0.5789626714285714,	0.8039594857142857,	1	,0.40485001428571427)
MSp700<-c(0.09714304928571428	,1.1391963878571427	,1,	0.29695458357142857)
MSpc<-c(0.0994090725,	0.52567512,	1,	0.5725156625)

MS=cbind(MSpc,MSfr,MSp700) 

RAWplot<-F             # plot the raw data 
OLDplot<-F             # plot the deconvoluted data written in the loaded file 
NEWplot<-T           # plot the deconvoluted data calculated with the model-spectra defined at the top of the script
SPEcTRAPLOT<-F         # plot the spectra normalized against the "all-signal"840nm-965nm at the area defined at the top of the script
WRITE<-F               # write a new file with time,raw data (in the original order), new deconvoluted data in a dictionary called with the current data and time in the current working dictionary  
sumplot<-F           # plot the spectra-plots over each other, the mean raw data, mean spectra-plots in the area defined at the top (only usable with no other plot active)
MEAN<-F
areaAll<-F
backround<-F
Kin<-c()
KIN<-T
par(mfrow=c(1,1))      # set the display area, show different plots over (X,) or next to  each other (,X)

MEANarea<-list(c(11050:11150))
area<-c(6000:7000)     # set area to look for model-spectra (for spectra plot,in data points)
area2<-c(7300:7500)    # set the area to calculate means for new model-spectra (for spectra-plot)
width<-1            # set the smooth-width (rolling window mean)
roolingNumber<-1
areaKLAS<-c(2000:10000)

steigung<-c()
Norm<-F                # plot the OldPlot and NewPlot normalized
RAWplotVersatz<-.0     # set an offset between the raw data lines
cutoff<-0              # cutoff on the edges of the data to prevent artifacts
SlopeCor<-F
SubZero<-T


temp = list.files(pattern="*.CSV")      #take all .CSV from the current working dictionary


#defines which model-spectra are used from top

allPC<-dv$time
allP700<-dv$time
allFR<-dv$time
if (backround==T){
  all1<-KLASX[,1]
  all2<-KLASX[,1]
  all3<-KLASX[,1]
  all4<-KLASX[,1]
}


titelList<-c()
datalist = list()
KLASlist<-list()
n<-0
light<-1
par(mar=c(4.1, 4.1, 4.1, 5.1), xpd=T) # set area outside of Plots for legend
dark2<-c()
dark3<-c()
p700<-c()
pc<-c()
Fdx<-c()
for (i in 1:length(temp)){
  dat<- read.csv(temp[i],header = T,sep=";",skip = 1) #read the data, skip first line with the title, takes 2.line as header
  datalist[[i]] <- dat # add it to your list
  if (WRITE==T){
    titelList[i] <- toString(readLines(temp[i])[1]) #add the first line as title to a list
  }
}

if (WRITE==T){
  folder<-strtrim(toString(format(Sys.time(),"%Y%m%d_%X")),14) #create a folder named with date and time in the current dictionary
  dir.create(folder)
}

if (MEAN==T){ 
  MEANarea<-list()                                            # create a file with the means dinfined in MEANarea() and labled of the file 
  MEANfile<-paste(strtrim(toString(format(Sys.time(),"%Y%m%d_%X")),14),"-area:",MEANarea,".csv",sep = "")
}
t=0
for (d in datalist){                    #loop to analyse all Data
  curentDATA<-d
  n<-n+1
  NAME<-temp[n]                         # name of the current data
  if (t==0){
    KLAS2<-curentDATA[,1:2]/10000
    t=t+1
    next}
  if (t==1){
    KLAS3<-curentDATA[,1:2]/10000
    t=t+1
    next
  }
  if (t==2){
    KLAS4<-curentDATA[,1:2]/10000
    t=t+1
    next
  }
  if (t==3){
    KLAS5<-curentDATA[,1:2]/10000
    t=t+1
  }  
  if (t==4){
    light<-light+1 
    KLASX<-cbind(KLAS2[,1],KLAS4[,2],KLAS2[,2],KLAS5[,2],KLAS3[,2])
    KLAS1<-KLASX
    if (backround==T){
      all1<-cbind(all1,KLASX[,2])
      all2<-cbind(all2,KLASX[,3])
      all3<-cbind(all3,KLASX[,4])
      all4<-cbind(all4,KLASX[,5])
    }
  #KLAS1[,2]<-KLAS1[,2]-all1[,light]
#  KLAS1[,3]<-KLAS1[,3]-all2[,light]
#  KLAS1[,4]<-KLAS1[,4]-all3[,light]
#  KLAS1[,5]<-KLAS1[,5]-all4[,light]
  }

    colnames(KLAS1)<- c("time","820-870","870-965","780-820","840-965")
    
    
    KLAS<-KLAS1[cutoff:(length(KLAS1[,1])-cutoff),]
    options(OutDec= ".")
    
    
    for ( i in c(2:5)){ 
      if (SubZero==T){
        KLAS[,i]<-(KLAS[,i]-mean(KLAS[150:550,i]) )      #sub zero (first 0.5s as background)
      }
      if (SlopeCor==T){
        slope<-as.numeric(lm(KLAS[28000:31000,i]~KLAS[28000:31000,1])$coefficients[2])  
        slope1<-seq(0,(max(KLAS[,1])-min(KLAS[,1])),length.out = length(KLAS[,1]))*slope
        KLAS[,i]<-KLAS[,i]-slope1              # slope correction between the first 500ms and the last 500ms(without the last 100ms)
      }
    if (width>1){
      for (a in c(1:roolingNumber))
      KLAS[,i]<-rollapply(KLAS[,i],width=width,FUN = mean,fill=c(0,0,0))
    }
    }
    
    dv<-t(qr.solve(MS,t(KLAS[,2:5]))) # Deconvulatate Data
    dv<-data.frame(time=KLAS[,1],PC=dv[,1],Fdx=dv[,2],P700=dv[,3])
    
    
    #print(c(max(dv$P700[3900:4170]),mean(dv$P700[9200:9700]),mean(dv$P700[10200:10700])),quote=F)
    for ( i in c(2:4)){ 

      
      if (SlopeCor==T){
        slope<-as.numeric(lm(dv[28000:31000,i]~dv[28000:31000,1])$coefficients[2])  
        slope1<-seq(0,(max(dv[,1])-min(dv[,1])),length.out = length(dv[,1]))*slope
        dv[,i]<-dv[,i]-slope1              # slope correction between the first 500ms and the last 500ms(without the last 100ms)
        
      }
      if (SubZero==T){
        dv[,i]<-dv[,i]-mean(dv[10:150,i])  #sub zero (first 0.5s as backround)
      }
      if (Norm==T){
        dv[,i]<-dv[,i]/max(abs(dv[,i]))
      }
    }
    if (KIN==T){  
      p700<-as.numeric(lm(dv[2040:2960,4]~dv[2040:2960,1])$coefficients[2])
      pc<-as.numeric(lm(dv[2040:2960,2]~dv[2040:2960,1])$coefficients[2])
      Fdx<-as.numeric(lm(dv[2040:2960,3]~dv[2040:2960,1])$coefficients[2])
      #  print(c(summary(lm(dv[101:123,4]~dv[101:123,1]))$r.squared,summary(lm(dv[101:123,2]~dv[101:123,1]))$r.squared))
      Kin<-rbind(Kin,c(p700,pc,Fdx))
    }
    #p700<-c(p700,as.numeric(lm(dv[2155:2955,4]~dv[2155:2955,1])$coefficients[2]))
    #pc<-c(pc,as.numeric(lm(dv[2155:2955,2]~dv[2155:2955,1])$coefficients[2]))
    #Fdx<-c(Fdx,as.numeric(lm(dv[2155:2955,3]~dv[2155:2955,1])$coefficients[2]))
    p700<-cbind(p700,max(dv$P700))
    pc<-cbind(pc,max(dv$PC[2250:3000]))
    Fdx<-cbind(Fdx,min(dv$Fdx))
    print(c(match(max(dv$P700),dv$P700),match(max(dv$PC[2250:4000]),dv$PC),match(min(dv$Fdx),dv$Fdx)))
    allPC<-cbind(allPC,dv$PC)
    allP700<-cbind(allP700,dv$P700)
    allFR<-cbind(allFR,dv$Fdx)

    if (OLDplot==T){
      
      OPLOT<-cbind(rollapply((curentDATA[grepl("time",names(curentDATA),fixed = TRUE)]),width=width,FUN = mean),
                   rollapply((curentDATA[grepl("P700",names(curentDATA),fixed = TRUE)]),width=width,FUN = mean),
                   rollapply((curentDATA[grepl("PC",names(curentDATA),fixed = TRUE)]),width=width,FUN = mean),
                   rollapply((curentDATA[grepl("Fd",names(curentDATA),fixed = TRUE)]),width=width,FUN = mean))
      # get the deconvoluted data from the file (smoothed)
      for ( i in c(2:4)){
        if (SubZero==T){
          OPLOT[,i]<-OPLOT[,i]-mean(OPLOT[1:500,i])  #sub zero (first 0.5s as backround)
        }
        if (Norm==T){
          OPLOT[,i]<-OPLOT[,i]/max(abs(OPLOT[,i]))
        }
      }     
      if (areaAll==T){
        OPLOT<-OPLOT[areaKLAS,]
      }  
      colnames(OPLOT)= c("time","P700","PC","Fdx")
      plot(OPLOT[,1],OPLOT[,2],
           type = "l",
           ylim = c(min(OPLOT[,2:4]),max(OPLOT[,2:4])),
           xlim = c(min(OPLOT[,1]),max(OPLOT[,1])),
           main = paste(NAME,"\n","deconvoluted(old)"),
           xlab = names(curentDATA)[grep("time",names(curentDATA),fixed = TRUE)],
           ylab = "relativ redox changes",
           bty = "L")
      lines(OPLOT[,1],OPLOT[,3],col="red")
      lines(OPLOT[,1],OPLOT[,4],col="blue")
      par(xpd=TRUE)
      legend("topright", inset=c(-0.2,0), legend = c("P700", "PC","Fdxx"),
             text.width = strwidth("1,000"),
             lty = 1, xjust = 1, yjust = 1,
             col=c("black","red","blue"),
             bty = "n",
             seg.len = .5)
      
      
    }
    
    if (NEWplot==T){

      
      if (areaAll==T){
        dv<-dv[areaKLAS,]
      } 
      plot(dv[,1],(dv$P700),
           type = "l",
           ylim = c(min(dv[,2:4]),max(dv[,2:4])),
           xlim = c(min(dv[,1]),max(dv[,1])),
           main = paste(NAME,"\n","deconvoluted(new)"),
           xlab = names(curentDATA)[grep("time",names(curentDATA),fixed = TRUE)],
           ylab = "relativ redox changes",
           bty = "L")
      lines(dv[,1],(dv$PC),col="red")
      lines(dv[,1],(dv$Fdx),col="blue") 
      legend("topright", inset=c(-0.1,0), legend = c("P700", "PC","Fdxx"),
             text.width = strwidth("1,000"),
             lty = 1, xjust = 1, yjust = 1,
             col=c("black","red","blue"),
             bty = "n",
             seg.len = .5)
      #lines(dv$time[(match(max(dv$P700),dv$P700)+1200):(match(max(dv$P700),dv$P700)+1500)],dv$P700[(match(max(dv$P700),dv$P700)+1200):(match(max(dv$P700),dv$P700)+1500)],col="brown")
      #lines(dv[2100:2900,1],dv[2100:2900,2],col="brown")
      #lines(dv[2100:2900,1],dv[2100:2900,3],col="brown")
      #lines(dv[2100:2900,1],dv[2100:2900,4],col="brown")
        }
    
    if (MEAN==T){
      dv<-t(qr.solve(MS,t(KLAS[,2:5]))) # Deconvulatate Data
      dv<-data.frame(PC=dv[,1],Fdx=dv[,2],P700=dv[,3])
      for ( i in c(1:3)){ 
        dv[,i]<-dv[,i]-mean(dv[1:500,i])  #sub zero (first 0.5s as backround)
      }
      t<-0
      for (m in MEANarea){      #print the means defined in MEANarea in a file 
        t<-t+1
        MEANprint<-paste(mean(dv[m,1]),mean(dv[m,2]),mean(dv[m,3]),substr(NAME,16,nchar(NAME)) ,sep = ";")
        write.table(MEANprint,file =paste(MEANfile,t,sep = "-"),append = T,row.names = F,col.names = F)
      }
      
    }
    
    if (RAWplot==T){
      RAW<-KLAS
      for ( i in c(2:5)){
        if (SubZero==T){
          RAW[,i]<-RAW[,i]-mean(RAW[1:500,i])     
        }  
        if (Norm==T){
          RAW[,i]<-RAW[,i]/max(RAW[,i])  #sub zero (first 0.5s as backround)
        } 
        RAW[,i]<-RAW[,i]+RAWplotVersatz*i
      } 
      if (areaAll==T){
        RAW<-RAW[areaKLAS,]
      } 
      par(mar=c(4.1, 4.1, 4.1, 6.7), xpd=TRUE)
      plot(RAW[,1],(RAW[,2]),
           type="l",
           ylim = c(min(RAW[,2:5]),(max(RAW[,2:5]))),
           main = paste(NAME,"\n","raw-data"),
           xlab = names(curentDATA)[grep("time",names(curentDATA),fixed = TRUE)],
           ylab = "absorption changes",
           bty = "L")                                  
      lines(RAW[,1],(RAW[,3]),type = "l",col="red")  
      lines(RAW[,1],(RAW[,4]),type = "l",col="blue")  
      lines(RAW[,1],(RAW[,5]),type = "l",col="green")  
      legend("topright", legend = c("780nm-820nm","820nm-870nm","840nm-965nm","870nm-965nm"),
             text.width = strwidth("1,00"),
             lty = 1,
             col=c("black","red","blue","green"),
             bty = "n",
             seg.len = .5 ,
             inset=c(-0.1,0) )
      print(min(RAW[,2]))
      print(min(RAW[,3]))
      print(min(RAW[,4]))
      print(min(RAW[,5]))
      print("___")
    }
    
    if (SPEcTRAPLOT==T){
      SPectra<-KLAS
      
      for ( i in c(2:3,5)){ 
        SPectra[,i]<-(KLAS[,i]/KLAS[,4])
      }
      
      SPectra[,4]<-1
      
      print(c(mean(SPectra[area2,2]),
              mean(SPectra[area2,3]),
              mean(SPectra[area2,4]),
              mean(SPectra[area2,5])))
      
      par(mar=c(4.1, 4.1, 4.1, 6.7), xpd=F)    
      plot(SPectra[area,1],SPectra[area,2],
           type="l",
           ylim = c(-.2,2),
           xlim = c(min(SPectra[area,1]),max(SPectra[area,1])),
           main = paste(NAME,"\n","model-spectra"),
           xlab = names(curentDATA)[grep("time",names(curentDATA),fixed = TRUE)],
           ylab = " absorption relativ to 840nm-965nm")
      
      lines(SPectra[area,1],SPectra[area,3],type = "l",col="red")  
      lines(SPectra[area,1],SPectra[area,4],type = "l",col="blue")  
      lines(SPectra[area,1],SPectra[area,5],type = "l",col="green")
      par(mar=c(4.1, 4.1, 4.1, 6.7), xpd=T)        
      legend("topright", legend = c("780nm-820nm","820nm-870nm","840nm-965nm","870nm-965nm"),
             text.width = strwidth("1,00"),
             lty = 1,
             col=c("black","red","blue","green"),
             bty = "n",
             seg.len = .5 ,
             inset=c(-0.21,0) )
    }
    
    if (WRITE==T){
      MS=cbind(MSpc,MSfr,MSp700)
      dv<-t(qr.solve(MS,t(KLAS1[,2:5]))) 
      dv<-data.frame(PC=dv[,1],Fdx=dv[,2],P700=dv[,3])
      towrite<-cbind(KLAS1,dv)
      
      towrite$X<-NULL
      write.table(titelList[n], file =paste(folder,NAME,sep = "/"),col.names = F, row.names = F, dec = ".", sep = "", quote = FALSE, append = F)
      write.table(towrite, file =paste(folder,NAME,sep = "/"), row.names = T, dec = ".", sep = ";", quote = FALSE, append = T)
    }
    
    if (sumplot==T){
      SPectra<-KLAS
      
      for ( i in c(2:3,5)){ 
        SPectra[,i]<-(KLAS[,i]/KLAS[,4])
      }
      SPectra[,4]<-1
      print(c(mean(SPectra[area2,2]),
              mean(SPectra[area2,3]),
              mean(SPectra[area2,4]),
              mean(SPectra[area2,5])))
      print(NAME)
      if (n==1){
        par(mar=c(4.1, 4.1, 4.1, 6.7), xpd=F)    
        plot(SPectra[area,1],SPectra[area,2],
             type="l",
             ylim = c(-.2,2),
             xlim = c(min(SPectra[area,1]),max(SPectra[area,1])),
             main = paste(NAME,"\n","all model-spectra"),
             xlab = names(curentDATA)[grep("time",names(curentDATA),fixed = TRUE)],
             ylab = " absorption relativ to 840nm-965nm")
        
      }
      lines(SPectra[area,1],SPectra[area,2], type = "l")
      lines(SPectra[area,1],SPectra[area,3],type = "l",col="red"  )
      lines(SPectra[area,1],SPectra[area,4],type = "l",col="blue")
      lines(SPectra[area,1],SPectra[area,5],type = "l",col="green")  
      
      KLASlist[[n]]<-KLAS
      sum700<-vector(length = length(KLASlist[[1]][,1]))
      sumPC<-vector(length = length(KLASlist[[1]][,1]))
      sumFR<-vector(length = length(KLASlist[[1]][,1]))  
      sumAll<-vector(length = length(KLASlist[[1]][,1]))  
    }
    
    t=0
  }


if (sumplot==T){
  par(mar=c(4.1, 4.1, 4.1, 6.7), xpd=T) 
  legend("topright", legend = c("780nm-820nm","820nm-870nm","840nm-965nm","870nm-965nm"),
         text.width = strwidth("1,00"),
         lty = 1,
         col=c("black","red","blue","green"),
         bty = "n",
         seg.len = .5 ,
         inset=c(-0.21,0) )  
  summenA<-cbind(sumFR,sum700,sumAll,sumPC)
  sum700<-sum700/n
  sumPC<-sumPC/n
  sumFR<-sumFR/n
  sumAll<-sumAll/n
  summen<-cbind(sumFR,sum700,sumAll,sumPC)
  par(mar=c(4.1, 4.1, 4.1, 6.7), xpd=F) 
  plot(SPectra[area,1],summen[area,2],
       type="l",
       ylim = c(min(summen[area,1:4]),max(summen[area,1:4])),
       xlim = c(min(SPectra[area,1]),max(SPectra[area,1])),
       main = paste(NAME,"\n","mean raw data"),
       xlab = names(curentDATA)[grep("time",names(curentDATA),fixed = TRUE)],
       ylab = "absorption changes",
       bty="L")                              
  
  
  lines(SPectra[area,1],summen[area,2],type = "l",col="red")  
  lines(SPectra[area,1],summen[area,3],type = "l",col="blue")  
  lines(SPectra[area,1],summen[area,4],type = "l",col="green")  
  par(mar=c(4.1, 4.1, 4.1, 6.7), xpd=T)    
  legend("topright", legend = c("780nm-820nm","820nm-870nm","840nm-965nm","870nm-965nm"),
         text.width = strwidth("1,00"),
         lty = 1,
         col=c("black","red","blue","green"),
         bty = "n",
         seg.len = .5 ,
         inset=c(-0.21,0) )
  
  sum700<-(sum700/sumAll)
  sumPC<-(sumPC/sumAll)
  sumFR<-(sumFR/sumAll)
  sumAll<-(sumAll/sumAll)
  summen<-cbind(sumFR,sum700,sumAll,sumPC)
  par(mar=c(4.1, 4.1, 4.1, 6.7), xpd=F)     
  
  plot(SPectra[area,1],summen[area,1],
       type="l",
       ylim = c(-.2,2),
       xlim = c(min(SPectra[area,1]),max(SPectra[area,1])),
       main = paste(NAME,"\n","mean model-spectra"),
       xlab = names(curentDATA)[grep("time",names(curentDATA),fixed = TRUE)],
       ylab = " absorption relativ to 840nm-965nm")                                               
  
  
  lines(SPectra[area,1],summen[area,2],type = "l",col="red")  
  lines(SPectra[area,1],summen[area,3],type = "l",col="blue")  
  lines(SPectra[area,1],summen[area,4],type = "l",col="green") 
  par(mar=c(4.1, 4.1, 4.1, 6.7), xpd=T)    
  legend("topright", legend = c("780nm-820nm","820nm-870nm","840nm-965nm","870nm-965nm"),
         text.width = strwidth("1,00"),
         lty = 1,
         col=c("black","red","blue","green"),
         bty = "n",
         seg.len = .5 ,
         inset=c(-0.21,0) )
  
}

print(p700)
print(pc)
print(Fdx)

slopeall<-rbind(p700,pc,Fdx)


#write.csv2(allP700,file="p700.csv")
#write.csv2(allPC,file="pc.csv")
#write.csv2(allFR,file="Fdx.csv")