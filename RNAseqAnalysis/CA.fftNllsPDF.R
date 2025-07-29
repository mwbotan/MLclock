CA.fftNllsPDF <- function (X,path="./",dataType="Unknown",dT=0.5,row=4,col=5,time=(0:(length(X[[1]])-1))*dT,AA=c(time[1],time[length(time)]),name=c(),plot.mode=T,range=c(15,35)){
 
 message(paste("dataType:",dataType,"  AA:",AA[1],AA[2]))
 
 X <- X[AA[1]<=time & time<=AA[2],]
 
 outDir = paste(path,"FFTnlls_",dataType,"_",AA[1],"_",AA[2],sep="")
 
 if(!file.exists(paste(outDir,sep=""))){dir.create(paste(outDir,"/",sep=""))}
 
 path=paste(outDir,"/NllsPlot",dataType,"_",AA[1],"_",AA[2],".pdf",sep="")
 
 pdf(path, height=21, width=29.7)
  par(cex=2)
  par(mfrow=c(row,col))
  par(oma = c(3, 0, 0, 0)) 
  
  o <- data.frame(Amp=0,Phi=0,Tau=0,RAE=0,TOL=0)
  
  message(paste(paste("___","n","/","all","        ,"),"Amp","Phi","Tau","RAE","tol",sep="\t"))
  
  for(i in 1:length(X)){
   
   
   fftNllsRes <- CA.fftNllsVec(X[[i]],dT=dT,main=name[i],plot.mode=plot.mode,range=range)
   o[i,1] <- fftNllsRes[[1]][1]
   o[i,2] <- fftNllsRes[[1]][2]
   o[i,3] <- fftNllsRes[[1]][3]
   o[i,4] <- fftNllsRes[[1]][4]
   o[i,5] <- fftNllsRes[[2]]
   
   tmp <- round(c(t(o[i,1:4])),3)
   
   message(paste(paste(" |_",i,"/",length(X),"finished,"),tmp[1],tmp[2],tmp[3],tmp[4],o[i,5],sep="\t"))
   
   if(plot.mode)if(i%%(row*col)==1)mtext(side = 1, line=1, outer=T, text = outDir, cex=1.5)
   
  }
  
  write.table(cbind(data.frame(name=name),o),paste(outDir,"/FFTNLLS_Results_",dataType,"_",AA[1],"_",AA[2],".txt",sep=""),sep="\t",quote=F,row.names=F)
  
 dev.off()
 message(" |_ finishied\n")
 
 plot(o$Tau,o$RAE,xlim=range,ylim=c(0,1))
 
 png(paste(outDir,"/NllsPlot",dataType,"_",AA[1],"_",AA[2],".png",sep=""))
  plot(o$Tau,o$RAE,xlim=range,ylim=c(0,1))
 dev.off()
 
 return(o)
 
}

