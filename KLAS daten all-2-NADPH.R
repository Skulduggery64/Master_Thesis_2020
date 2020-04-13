if("zoo" %in% rownames(installed.packages()) == FALSE) {install.packages("zoo")}
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
if("matrixStats" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixStats")}

require(plyr)
require(zoo)
require(matrixStats)

MSfrO<-c(0.5654,0.7106,1.000,0.4797)#c( 0.5654,0.7106,1.0000,0.4797)#c(0.6166,0.7864,1.0000,0.4242)     #Old model-spectra
MSp700O<-c(0.069042378,	1.159986408,	1,	0.29295087)#c(0.0160,0.8515,1.0000,0.3527)#c(0.1047463, 1.1690437, 1.0000000, 0.2837269)
MSpcO<-c(0.0703280125,	0.51793242,	1	,0.61409297)#c(0.1056,0.4385,1.0000,0.5966)#c(0.0754,0.4113,1.0000,0.6187)

MSfr<-c(0.6357491333333333,	0.8811377,	1,	0.3847311666666667)#0.5846, 0.7747,1, 0.4140)# 58255124,	0.77916518,	1,	0.40774122)   #New model-spectra
MSp700<-c(0.08115028833333333,	1.1824632766666665,	1,	0.28433813166666666)#0.0724, 1.1616, 1, 0.2916) #c(0.175,1.1,1,0.5) .08842147,	1.1933835750000001,	1	,0.2697128366666667) #c(0.079071108,1.165637688,1,0.291831616)
MSpc<-c(0.0781098575,	0.51836993,	1,	0.5666418025)#0.06485346,	0.57466225,	1,	0.65342652) #c(0.1083375,	0.522252,	1,	0.584084)c(0.0598297875,	0.7104789525,	1,	0.5090400308)


MS=cbind(MSpc,MSfr,MSp700) 
#MS<-MSrandS
RAWplot<-F            # plot the raw data 
OLDplot<-F             # plot the deconvoluted data written in the loaded file 
NEWplot<-T           # plot the deconvoluted data calculated with the model-spectra defined at the top of the script
SPEcTRAPLOT<-F         # plot the spectra normalized against the "all-signal"840nm-965nm at the area defined at the top of the script
WRITE<-F               # write a new file with time,raw data (in the original order), new deconvoluted data in a dictionary called with the current data and time in the current working dictionary  
sumplot<-F           # plot the spectra-plots over each other, the mean raw data, mean spectra-plots in the area defined at the top (only usable with no other plot active)
MEAN<-F
areaAll<-F
safe<-F
KIN=T
NirMax=F
Kin<-c()


par(mfrow=c(1,1))      # set the display area, show different plots over (X,) or next to  each other (,X)

MEANarea<-list(c(11050:11150))
area<-c(1050:10900)     # set area to look for model-spectra (for spectra plot,in data points)
area2<-c(2800:3200)    # set the area to calculate means for new model-spectra (for spectra-plot)
width<-1            # set the smooth-width (rolling window mean)
areaKLAS<-c(1050:35400)

steigung<-c()

Norm<-F                # plot the OldPlot and NewPlot normalized
RAWplotVersatz<-.0     # set an offset between the raw data lines
cutoff<-5          # cutoff on the edges of the data to prevent artifacts
SlopeCor<-F
SubZero<-T
PCredox<-c()
P00redox<-c()
Fdxredox<-c()

safeP700<-KLAS[,1]
safePC<-KLAS[,1]
safeFdx<-KLAS[,1]
safeNADPH<-KLAS[,1]

temp = list.files(pattern="*.CSV")      #take all .CSV from the current working dictionary


  #defines which model-spectra are used from top

titelList<-c()
datalist = list()
KLASlist<-list()
n<-0
light <-0
par(mar=c(4.1, 4.1, 4.1, 5.1), xpd=TRUE) # set area outside of Plots for legend
dark2<-c()
dark3<-c()
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
for (d in datalist){                    #loop to analyse all Data
  curentDATA<-d
namelist<-names(curentDATA)
  n<-n+1
  NAME<-temp[n]                         # name of the current data
  KLAS<-cbind(curentDATA[grepl("time",names(curentDATA),fixed = TRUE)],
               curentDATA[grepl("780",names(curentDATA),fixed = TRUE)],
               curentDATA[grepl("820.870",names(curentDATA),fixed = TRUE)],
               curentDATA[grepl("840",names(curentDATA),fixed = TRUE)],
               curentDATA[grepl("870.965",names(curentDATA),fixed = TRUE)],
               curentDATA[grepl(c("Fluo"),names(curentDATA),fixed = TRUE)])

  names(KLAS)<-c(namelist[grepl("time",namelist)],
                 namelist[grepl("780",namelist)],
                 namelist[grepl("820.870",namelist)],
                 namelist[grepl("840",namelist)],
                 namelist[grepl("870.965",namelist)],
                 namelist[grepl("Fluo",namelist)])


  
  options(OutDec= ".")
  
  for ( i in c(2:6)){
    if (SubZero==T){
    KLAS[,i]<-(KLAS[,i]-mean(KLAS[5:55,i]) )      #sub zero (first 0.5s as background)
    }
    if (SlopeCor==T){
    slope<-mean(KLAS[(length(KLAS[,i])-100):length(KLAS[,i]),i])-mean(KLAS[1050:1500,i])  
    slope1<-c(seq(0,1,length.out = length(KLAS[,i]))*slope)
    KLAS[,i]<-KLAS[,i]-slope1              # slope correction between the first 500ms and the last 500ms(without the last 100ms)
    }
    KLAS[0:cutoff,i]<-0
    KLAS[(length(KLAS[,1])-cutoff):(length(KLAS[,1])),i]<-0
    }


  
 

    if (areaAll==T){
    dv<-dv[areaKLAS,]
  } 

  if (OLDplot==T){
    
    OPLOT<-cbind(rollapply((curentDATA[grepl("time",names(curentDATA),fixed = TRUE)]),width=width,FUN = mean),
                 rollapply((curentDATA[grepl("P700",names(curentDATA),fixed = TRUE)]),width=width,FUN = mean),
                 rollapply((curentDATA[grepl("PC",names(curentDATA),fixed = TRUE)]),width=width,FUN = mean),
                 rollapply((curentDATA[grepl("Fd",names(curentDATA),fixed = TRUE)]),width=width,FUN = mean))
      # get the deconvoluted data from the file (smoothed)
    for ( i in c(2:4)){
      if (SubZero==T){
      OPLOT[,i]<-OPLOT[,i]-mean(OPLOT[5:55,i])  #sub zero (first 0.5s as backround)
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
  dv<-t(qr.solve(MS,t(KLAS[,2:5]))) # Deconvulatate Data
  dv<-data.frame(time=KLAS[,1],PC=dv[,1],Fdx=dv[,2],P700=dv[,3],KLAS[,6])
  if (NirMax==T){  

  #    PCredox<-rbind(PCredox,c(max(dv[4010:4090,2]),mean(dv[3930:3980,2]),mean(dv[10200:10700,2])))
  #   P00redox<-rbind(P00redox,c(max(dv[4010:4090,4]),mean(dv[3930:3980,4]),mean(dv[10200:10700,4])))
  #  Fdxredox<-rbind(Fdxredox,c(min(dv[4010:4090,3]),mean(dv[3930:3980,3]),mean(dv[10200:10700,3])))
  print(c(max(dv$PC)/max(dv$P700),min(dv$Fdx)/max(dv$P700),max(dv[,5])/max(dv$P700)))
}
  if (KIN==T){
  p700<-as.numeric(lm(dv[101:121,4]~dv[101:121,1])$coefficients[2])
  pc<-as.numeric(lm(dv[101:121,2]~dv[101:121,1])$coefficients[2])
  Fdx<-as.numeric(lm(dv[101:121,3]~dv[101:121,1])$coefficients[2])
  Fluo<-as.numeric(lm(dv[101:111,5]~dv[101:111,1])$coefficients[2])
  print(c(p700,pc,Fdx,Fluo))     
  Kin<-rbind(Kin,c(p700,pc,Fdx,Fluo))
  }
  if (NEWplot==T){
    dv<-t(qr.solve(MS,t(KLAS[,2:5]))) # Deconvulatate Data
    dv<-data.frame(time=KLAS[,1],PC=dv[,1],Fdx=dv[,2],P700=dv[,3],KLAS[,6])
    
    for ( i in c(2:5)){ 
      dv[,i]<-rollapply(dv[,i],width=width,FUN = mean,fill=c(0,0,0))
    }
    
#    PCredox<-rbind(PCredox,c(max(dv[4010:4090,2]),mean(dv[3930:3980,2]),mean(dv[10200:10700,2])))
 #   P00redox<-rbind(P00redox,c(max(dv[4010:4090,4]),mean(dv[3930:3980,4]),mean(dv[10200:10700,4])))
  #  Fdxredox<-rbind(Fdxredox,c(min(dv[4010:4090,3]),mean(dv[3930:3980,3]),mean(dv[10200:10700,3])))
    
    if (areaAll==T){
      dv<-dv[areaKLAS,]
    } 
 #   dv$Fdx<-dv$Fdx/min(dv$Fdx)

dv[,5]<-dv[,5]*10
    plot(dv[,1],(dv$P700),
         type = "l",
         ylim = c(min(dv[,2:5]),max(dv[,2:5])),
         xlim = c(min(dv[,1]),max(dv[,1])),
         main = paste(NAME,"\n","deconvoluted(new)"),
         xlab = names(curentDATA)[grep("time",names(curentDATA),fixed = TRUE)],
         ylab = "relativ redox changes",
         bty = "L")
    lines(dv[,1],(dv$PC),col="red")
    lines(dv[,1],(dv$Fdx),col="darkgreen")
    lines(dv[,1],dv[,5],col="blue")
    legend("topright", inset=c(-0.1,0), legend = c("P700", "PC","Fdxx",paste(namelist[grepl("Fluo",namelist)])),
           text.width = strwidth("1,000"),
           lty = 1, xjust = 1, yjust = 1,
           col=c("black","red","darkgreen","blue"),
           bty = "n",
           seg.len = .5)
  }
  if (safe==T){
    safeP700<-cbind(safeP700,dv[,4])
    safePC<-cbind(safePC,dv[,2])
    safeFdx<-cbind(safeFdx,dv[,3])
    safeNADPH<-cbind(safeNADPH,dv[,5])
    
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
    dv<-t(qr.solve(MS,t(KLASR[,2:5]))) 
    dv<-data.frame(KLASR,PC=dv[,1],Fdx=dv[,2],P700=dv[,3])
    towrite<-cbind(dv)
    
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
  
  for (l in KLASlist){
    sum700<-sum700+l[,3]
    sumPC<-sumPC+l[,5]
    sumFR<-sumFR+l[,2]
    sumAll<-sumAll+l[,4]
  }

 # print(c(max(dv$P700),max(dv$PC),min(dv$Fdx)))
  #print(c(max(dv$P700),max(dv$PC),min(dv$Fdx))/max(dv$P700))
  
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
#Kin<-data.frame(P700=Kin[,1],PC=Kin[,2],Fdx=Kin[,3],Flou=Kin[,4])

#write.csv(file="safeNADPH.csv",safeNADPH)
#write.csv(file="safeP700.csv",safeP700)
#write.csv(file="safePC.csv",safePC)
#write.csv(file="safeFdx.csv",safeFdx)

