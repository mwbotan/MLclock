##
## T. Muranaka
## R-scripts for FFT-NLLS 
## x: time-series vector
## dT: interval time
## 

CA.fftNllsVec <- function(x,dT,conf=0.05,maxiter=50,main=c(),plot.mode=T,lim=25,range=c(15,35),lwd=1,ylim=c()){
 
 require("splus2R")
 
 # FFT
 fftres <- fft(x)
 
 # Calculation of amplitude and phase for each frequency
 Amp <- (abs(fftres)*2/length(x))[1:(length(x)/2)]
 Phi <- (Arg(fftres)/pi)[1:(length(x)/2)]
 
 # Get the Constant value in the fitting. Amplitude of the first, k=0
 cons <- Amp[1]/2
 
 # Frequency 
 f <- (0:(length(x)-1)*(1/dT)/length(x))[1:(length(x)/2)]
 
 # Summary table of FFT 
 fftPeak <- data.frame(freq=f,Amp=Amp,Phi=Phi)
 
 # Pick up parameters for fitting
 fftPeak <- fftPeak[peaks(Amp,span=3),] 
 fftPeak <- fftPeak[order(fftPeak$Amp, decreasing=T),]
 
 # y(y-axis) and time(x-axis) for fitting model
 y <<- x
 ti <<- 0:(length(x)-1)*dT
 
 # Vector for initial value set for fitting parameter
 start <- c()
 
 result <- c()
 tol <- NA
 estParAll  <- c()
 
 # tl: The tolerance level for the relative offset convergence criterion
 for(tl in c(1e-5,1e-4,1e-3,1e-2,5e-2,1e-1)){
  model <- "y ~ cons"
  count=0
  for(i in 1:length(fftPeak$Amp)){
   if(count==lim)break
   
   res <- c()
   
   model <- paste(model, " + A",i," * cos((2 * pi * ti/C",i,") + B",i,"  * pi)",sep="")
   
   
   # A Amplitude
   start[i*3-2] <- fftPeak[i,2]
   names(start)[i*3-2] <- paste("A",i,sep="")
   
   # B Phase
   start[i*3-1] <- fftPeak[i,3]
   names(start)[i*3-1] <- paste("B",i,sep="")
   
   # C Tau
   start[i*3] <- 1/fftPeak[i,1]
   names(start)[i*3] <- paste("C",i,sep="")
   
   # NLS
   tryRes <- try(res <- nls(formula=model,start=start,control=nls.control(maxiter=maxiter,tol=tl)),silent=T)
   
   # Error check
   if(class(tryRes)=="try-error")break
   
   # Fitted parameter
   par <- summary(res)$parameters
   
   # P value check for amplitude
   if(max(par[(1:i)*3-2,4])>conf)break
   
   # Standard Error check for amplitude
   if(min( abs(par[(1:i)*3-2,2])/par[(1:i)*3-2,1])>1)break
   
   # Minus sign check for Tau
   if(min(par[(1:i)*3,1])<0)break
   
   
   result <- res
   count=count+1
  }
  
  # Initiatialize
  RAE <- NA
  
  if(length(result)!=0){
   
   # Fitted parameters
   para <- summary(result)$parameters
   
   l <- length(para)/12
   
   # Parameteres table
   estPar <- data.frame(
    Amp = para[1:l*3-2,1],
    Tau = para[1:l*3,1],
    Phi = (-1*para[1:l*3-1,1])/2,
    RAE = para[1:l*3-2,1]
   )
   
   
   
   for(i in 1:length(estPar[[1]])){
    rownames(estPar)[[i]] <- paste("A",i,sep="")
   }
   
   
   # Confidential interval 95%
   confInt <- stats:::confint.default(result,parm=rownames(estPar),level=0.95)
   
   # RAE Calculation
   estPar$RAE <- abs((confInt[,2] - confInt[,1])/estPar$Amp)/2
   
   # Ordered by RAE value
   estParAll <- estPar[order(estPar$RAE),]
   
   # Circaidan range
   estPar <- estParAll[estParAll$Tau >=range[[1]] & estParAll$Tau <=range[[2]],]
   
   #print(estPar)
   
   tol <- tl
   if(!is.na(estPar$RAE[1]))break
  }
 }
 
 
 day = ceiling(length(ti)*dT/24)
 xlim = c(0,day*24)
 xlabels <- (0:day)*24
 xposition <- (0:day)*24
 
 if(length(result)!=0){
  
  
  if(plot.mode){
   if(length(ylim)){
    plot(ti,y,sub=paste("T: ",round(estPar$Tau[1],3)," hr   RAE: ",round(estPar$RAE[1],3),"  Tol: ",tol," num",i,sep=""),main=main,xlab="",ylab="",axes=F,ylim=ylim)
   }else{
    plot(ti,y,sub=paste("T: ",round(estPar$Tau[1],3)," hr   RAE: ",round(estPar$RAE[1],3),"  Tol: ",tol," num",i,sep=""),main=main,xlab="",ylab="",axes=F)
   }
   box()
   axis(side=2,lwd=lwd)
   axis(side=1,labels=xlabels,at=xposition,lwd=lwd)
  }
  if(plot.mode){
    lines(ti,predict(result),col=2)
    lines(ti,estPar$Amp[1] * cos(2 * pi * ti/estPar$Tau[1] - estPar$Phi[1] * 2 * pi) + cons,col=4,lty="dashed")
    abline(h=cons,col="forestgreen",lty="dashed")
   }
 }else{
  if(plot.mode)plot(ti,y,main=main,xlab="",ylab="")
  estPar <- data.frame(
   Amp=NA,
   Tau=NA,
   Phi=NA,
   RAE=NA
  )
 }
 
 Amp = estPar$Amp[1]
 Phi = estPar$Phi[1]
 Tau = estPar$Tau[1]
 RAE = estPar$RAE[1]
 
 if(!is.na(Amp)){
  if(Amp < 0){
   Amp = abs(Amp)
   Phi = (Phi+0.5)
  }
 Phi = Phi%%1
 }
 return(list(parameter=c(Amp=Amp,Phi=Phi,Tau=Tau,RAE=RAE,CONS=cons),Tol=tol,estPar=estPar,estParAll=estParAll,fit.result=result))
 
}
