if("zoo" %in% rownames(installed.packages()) == FALSE) {install.packages("zoo")}
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
if("matrixStats" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixStats")}

library(scales)
require(plyr)
require(zoo)
require(matrixStats)

MSfr<-c(0.5789626714285714,	0.8039594857142857,	1	,0.40485001428571427)
MSp700<-c(0.09714304928571428	,1.1391963878571427	,1,	0.29695458357142857)
MSpc<-c(0.0994090725,	0.52567512,	1,	0.5725156625)

RAWplot<-F             # plot the raw data A
NEWplot<-T             # plot the deconvoluted data calculated with the model-spectra defined at the top of the script
WRITE<-F               # write a new file with time,raw data (in the original order), new deconvoluted data in a dictionary called with the current data and time in the current working dictionary  
KIN<-F
NirMax<-F
Yield<-T

Kinarea<-c(201:224)
yieldarea<-c(10200:10500)

width<-25                # set the smooth-width (rolling window mean)
roolingNumber<-1

cutoff<-0              # cutoff on the edges of the data to prevent artifactsa
SlopeCor<-F
SubZero<-T

par(mfrow=c(1,1))      # set the display area, show different plots over (X,) or next to  each other (,X)

Kin<-c()
YIELD<-c()
MS=cbind(MSpc,MSfr,MSp700) 

temp = list.files(pattern="*.CSV")      #take all .CSV from the current working dictionary

titelList<-c()
datalist = list()
KLASlist<-list()
n<-0

par(mar=c(4.1, 4.1, 4.1, 5.1), xpd=TRUE) # set area outside of Plots for legend

for (i in 1:length(temp)){
  dat<- read.csv(temp[i],header = T,sep=";",skip = 1) #read the data, skip first line with the title, takes 2.line as header
  datalist[[i]] <- dat # add it to your list
  if (WRITE==T){
    titelList[i] <- toString(readLines(temp[i])[1]) #add the first line as title to a list
  }
}

if (WRITE==T){
  folder<-strtrim(toString(format(Sys.time(),"%Y%m%d")),14) #create a folder named with date and time in the current dictionary
  dir.create(folder)
}

for (d in datalist){                    #loop to analyse all Data
  curentDATA<-d
  n<-n+1
  NAME<-temp[n]                         # name of the current data
  KLAS<-cbind(curentDATA[grepl("time",names(curentDATA),fixed = TRUE)],
               curentDATA[grepl("780",names(curentDATA),fixed = TRUE)],
               curentDATA[grepl("820.870",names(curentDATA),fixed = TRUE)],
               curentDATA[grepl("840",names(curentDATA),fixed = TRUE)],
               curentDATA[grepl("870.965",names(curentDATA),fixed = TRUE)])
  if ("Fluo.deltaI.I.x10e3" %in% names(curentDATA)){
   KLAS<-cbind(curentDATA[grepl("time",names(curentDATA),fixed = TRUE)],
               curentDATA[grepl("780",names(curentDATA),fixed = TRUE)],
               curentDATA[grepl("820.870",names(curentDATA),fixed = TRUE)],
               curentDATA[grepl("840",names(curentDATA),fixed = TRUE)],
               curentDATA[grepl("870.965",names(curentDATA),fixed = TRUE)],
               curentDATA[grepl("Fluo",names(curentDATA),fixed = TRUE)])
 }

  KLAS<-KLAS[cutoff:(length(KLAS[,1])-cutoff),]
  options(OutDec= ".")
  
  for ( i in c(2:length(KLAS[1,]))){ 
    if (SubZero==T){
    KLAS[,i]<-(KLAS[,i]-mean(KLAS[10:100,i]) )      #sub zero (first 0.5s as background)
    }
    if (SlopeCor==T){
    slope<-mean(KLAS[(length(KLAS[,i])-200):length(KLAS[,i])-50,i])-mean(KLAS[1:150,i])  
    slope1<-c(seq(0,1,length.out = length(KLAS[,i]))*slope)
    KLAS[,i]<-KLAS[,i]-slope1              # slope correction between the first 500ms and the last 500ms(without the last 100ms)
    }
    if (width>1){
      for (a in c(1:roolingNumber))
        KLAS[,i]<-rollapply(KLAS[,i],width=width,FUN = mean,fill=c(0,0,0))
    }
  }
  
  dv<-t(qr.solve(MS,t(KLAS[,2:5]))) # Deconvulatate Data
  dv<-data.frame(time=KLAS[,1],PC=dv[,1],Fdx=dv[,2],P700=dv[,3])
  
  if (KIN==T){  
    p700<-as.numeric(lm(dv[Kinarea,4]~dv[Kinarea,1])$coefficients[2])
    pc<-as.numeric(lm(dv[Kinarea,2]~dv[Kinarea,1])$coefficients[2])
    Fdx<-as.numeric(lm(dv[Kinarea,3]~dv[Kinarea,1])$coefficients[2])
    Kin<-rbind(Kin,c(p700,pc,Fdx))
  }

if (NirMax==T){
 print(c(max(dv$P700),max(dv$PC),min(dv$Fdx)))
}
  if (Yield==T){
    if ("Fluo.deltaI.I.x10e3" %in% names(curentDATA)){   
        YIELD<-rbind(YIELD,c(max(dv$P700),mean(dv$P700[yieldarea]),max(dv$PC),mean(dv$PC[yieldarea]),max(KLAS$Fluo.deltaI.I.x10e3),mean(KLAS$Fluo.deltaI.I.x10e3[yieldarea])))
    } else {YIELD<-rbind(YIELD,c(max(dv$P700),mean(dv$P700[yieldarea]),max(dv$PC),mean(dv$PC[yieldarea])))}
  }
  if (NEWplot==T){
    if ("Fluo.deltaI.I.x10e3" %in% names(curentDATA)){
    plot(dv[,1],(dv$P700),
         type = "l",
         ylim = c(min(dv[,2:4]),max(dv[,2:4])),
         xlim = c(min(dv[,1]),max(dv[,1])),
         main = paste(NAME,"\n","deconvoluted"),
         xlab = names(curentDATA)[grep("time",names(curentDATA),fixed = TRUE)],
         ylab = "relativ redox changes",
         bty = "L")
    lines(dv[,1],(dv$PC),col="red")
    lines(dv[,1],(dv$Fdx),col="blue")
    lines(KLAS$time.s,(KLAS$Fluo.deltaI.I.x10e3/max(KLAS$Fluo.deltaI.I.x10e3)), col="orange")
    legend("topright", inset=c(-0.1,0), legend = c("P700", "PC","Fdx","Chl"),
           text.width = strwidth("1,000"),
           lty = 1, xjust = 1, yjust = 1,
           col=c("black","red","blue","orange"),
           bty = "n",
           seg.len = .5)
    } else {    plot(dv[,1],(dv$P700),
                     type = "l",
                     ylim = c(min(dv[,2:4]),max(dv[,2:4])),
                     xlim = c(min(dv[,1]),max(dv[,1])),
                     main = paste(NAME,"\n","deconvoluted"),
                     xlab = names(curentDATA)[grep("time",names(curentDATA),fixed = TRUE)],
                     ylab = "relativ redox changes",
                     bty = "L")
      lines(dv[,1],(dv$PC),col="red")
      lines(dv[,1],(dv$Fdx),col="blue")
      lines(KLAS$time.s,KLAS$Fluo.deltaI.I.x10e3, col="cyan")
      legend("topright", inset=c(-0.1,0), legend = c("P700", "PC","Fdx","Chl"),
             text.width = strwidth("1,000"),
             lty = 1, xjust = 1, yjust = 1,
             col=c("black","red","blue","cyan"),
             bty = "n",
             seg.len = .5)}
  }
  if (RAWplot==T){
    RAW<-KLAS

    
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
  if (WRITE==T){
    dv<-t(qr.solve(MS,t(KLAS[,2:5]))) 
    dv<-data.frame(KLAS,PC=dv[,1],Fdx=dv[,2],P700=dv[,3])
    towrite<-cbind(dv)
    
    towrite$X<-NULL
    write.table(titelList[n], file =paste(folder,NAME,sep = "/"),col.names = F, row.names = F, dec = ".", sep = "", quote = FALSE, append = F)
    write.table(towrite, file =paste(folder,NAME,sep = "/"), row.names = T, dec = ".", sep = ";", quote = FALSE, append = T)
  }
}
Kin<-data.frame(P700=Kin[,1],PC=Kin[,2],Fdx=Kin[,3])
if ("Fluo.deltaI.I.x10e3" %in% names(curentDATA)){   
  YIELD<-data.frame(p700Max=YIELD[,1],p700Base=YIELD[,2],PCmax=YIELD[,3],PCBase=YIELD[,4],ChlFluoMax=YIELD[,5],ChlFluoBase=YIELD[,6])
}   else {YIELD<-data.frame(p700Max=YIELD[,1],p700Base=YIELD[,2],PCmax=YIELD[,3],PCBase=YIELD[,4])}

