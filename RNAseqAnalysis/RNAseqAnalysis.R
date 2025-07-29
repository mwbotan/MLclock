
### DATA INPUT
#rpm
load("data/OM2011-2013_refv4_bowtie2_rpm")
rpm <- rpm[substr(rownames(rpm),1,1)=="g",]
rpm <- data.frame(t(rpm))
rpm_log2 <- log2(rpm+1)
rm(rpm)

#mean
require(dplyr)

tmp <- sampleAll %>%
 dplyr::group_by(year,month,day,hour,set) %>%
 dplyr::summarize_all(funs(mean))

o <- data.frame(tmp)

for(i in 1:7){
 
 message(paste(i*5000,"start"))
 
 tmp <- cbind(sampleAll ,rpm_log2[,(1+5000*(i-1)):(min(ncol(rpm_log2),5000*i))]) %>%
  dplyr::group_by(year,month,day,hour,set) %>%
  dplyr::summarize_all(funs(mean))
 
 message(paste(i*5000,"end"))
 
 o <- cbind(o,data.frame(tmp[6:ncol(tmp)]))
}
sample_mean <- o[1:5]
rpm_log2_mean <- o[6:ncol(o)]
rm(o)


#geneList
geneList <- read.table("data/Ahg_geneList.txt",sep="\t",stringsAsFactors=F,h=T)

#sample
sampleAll <- read.table("data/140519_SampleAttribute.txt",sep="\t",stringsAsFactors=F,h=T)
sampleAll <- sampleAll[,c(2,3,4,5,11)]
sampleAll[sampleAll$set=="SE2013","set"] <- "VE2013"

#Temperature
temp.nishi.2013.1hr <- read.table("data/temp.nishi.2013.1hr.txt",h=T)
temp.nishi.all.1hr <- read.table("data/temp.nishi.all.1hr.txt",h=T)

temp.nishi <- data.frame(
 VE=temp.nishi.2013.1hr[temp.nishi.2013.1hr[temp.nishi.2013.1hr$year==2013 & temp.nishi.2013.1hr$month==3 & temp.nishi.2013.1hr$day==19 & temp.nishi.2013.1hr$hour==16,"index"]:temp.nishi.2013.1hr[temp.nishi.2013.1hr$year==2013 & temp.nishi.2013.1hr$month==3 & temp.nishi.2013.1hr$day==21 & temp.nishi.2013.1hr$hour==14,"index"],"temp"],
 SS=temp.nishi.2013.1hr[temp.nishi.2013.1hr[temp.nishi.2013.1hr$year==2013 & temp.nishi.2013.1hr$month==6 & temp.nishi.2013.1hr$day==26 & temp.nishi.2013.1hr$hour==16,"index"]:temp.nishi.2013.1hr[temp.nishi.2013.1hr$year==2013 & temp.nishi.2013.1hr$month==6 & temp.nishi.2013.1hr$day==28 & temp.nishi.2013.1hr$hour==14,"index"],"temp"],
 AE=temp.nishi.2013.1hr[temp.nishi.2013.1hr[temp.nishi.2013.1hr$year==2013 & temp.nishi.2013.1hr$month==9 & temp.nishi.2013.1hr$day==24 & temp.nishi.2013.1hr$hour==16,"index"]:temp.nishi.2013.1hr[temp.nishi.2013.1hr$year==2013 & temp.nishi.2013.1hr$month==9 & temp.nishi.2013.1hr$day==26 & temp.nishi.2013.1hr$hour==14,"index"],"temp"],
 WS=temp.nishi.2013.1hr[temp.nishi.2013.1hr[temp.nishi.2013.1hr$year==2013 & temp.nishi.2013.1hr$month==12 & temp.nishi.2013.1hr$day==24 & temp.nishi.2013.1hr$hour==16,"index"]:temp.nishi.2013.1hr[temp.nishi.2013.1hr$year==2013 & temp.nishi.2013.1hr$month==12 & temp.nishi.2013.1hr$day==26 & temp.nishi.2013.1hr$hour==14,"index"],"temp"]
 )

#daylength
daylength.nishi.2013 <- read.table("data/daylength.nishi.txt",h=T)

### RHYTHM ANALYSIS

LTau <- 20
Utau <- 28

URAE <- 0.4
m2p <- 0.001

CA.fftNllsPDF(rpm_log2_mean[sample_mean$set=="VE2013",],path="FFTNLLS/",dT=2,dataType="VE2013",name=colnames(rpm_log2_mean),plot.mode=F,range=c(0,48))
CA.fftNllsPDF(rpm_log2_mean[sample_mean$set=="SS2013",],path="FFTNLLS/",dT=2,dataType="SS2013",name=colnames(rpm_log2_mean),plot.mode=F,range=c(0,48))
CA.fftNllsPDF(rpm_log2_mean[sample_mean$set=="AE2013",],path="FFTNLLS/",dT=2,dataType="AE2013",name=colnames(rpm_log2_mean),plot.mode=F,range=c(0,48))
CA.fftNllsPDF(rpm_log2_mean[sample_mean$set=="WS2013",],path="FFTNLLS/",dT=2,dataType="WS2013",name=colnames(rpm_log2_mean),plot.mode=F,range=c(0,48))

library(MetaCycle)

write.table(cbind(data.frame(geneSymbol=colnames(rpm_log2_mean)),t(rpm_log2_mean[sample_mean$set=="VE2013",])),"MetaCycle/VE48.txt",sep="\t",quote=F,row.names=F)
write.table(cbind(data.frame(geneSymbol=colnames(rpm_log2_mean)),t(rpm_log2_mean[sample_mean$set=="SS2013",])),"MetaCycle/SS48.txt",sep="\t",quote=F,row.names=F)
write.table(cbind(data.frame(geneSymbol=colnames(rpm_log2_mean)),t(rpm_log2_mean[sample_mean$set=="AE2013",])),"MetaCycle/AE48.txt",sep="\t",quote=F,row.names=F)
write.table(cbind(data.frame(geneSymbol=colnames(rpm_log2_mean)),t(rpm_log2_mean[sample_mean$set=="WS2013",])),"MetaCycle/WS48.txt",sep="\t",quote=F,row.names=F)


meta2d(infile="./MetaCycle/VE48.txt",filestyle="txt",timepoints= rep(seq(0, 46, by=2)))
meta2d(infile="./MetaCycle/SS48.txt",filestyle="txt",timepoints= rep(seq(0, 46, by=2)))
meta2d(infile="./MetaCycle/AE48.txt",filestyle="txt",timepoints= rep(seq(0, 46, by=2)))
meta2d(infile="./MetaCycle/WS48.txt",filestyle="txt",timepoints= rep(seq(0, 46, by=2)))

sumVE <- cbind(read.table("./fftNLLS/FFTnlls_VE2013_0_46/FFTNLLS_Results_VE2013_0_46.txt",h=T,stringsAsFactors=F),read.table("./metaout/meta2d_VE48.txt",h=T,stringsAsFactors=F))
sumSS <- cbind(read.table("./fftNLLS/FFTnlls_SS2013_0_46/FFTNLLS_Results_SS2013_0_46.txt",h=T,stringsAsFactors=F),read.table("./metaout/meta2d_SS48.txt",h=T,stringsAsFactors=F))
sumAE <- cbind(read.table("./fftNLLS/FFTnlls_AE2013_0_46/FFTNLLS_Results_AE2013_0_46.txt",h=T,stringsAsFactors=F),read.table("./metaout/meta2d_AE48.txt",h=T,stringsAsFactors=F))
sumWS <- cbind(read.table("./fftNLLS/FFTnlls_WS2013_0_46/FFTNLLS_Results_WS2013_0_46.txt",h=T,stringsAsFactors=F),read.table("./metaout/meta2d_WS48.txt",h=T,stringsAsFactors=F))

sumVE$Phi <- (sumVE$Phi*24 +16)%%24
sumSS$Phi <- (sumSS$Phi*24 +16)%%24
sumAE$Phi <- (sumAE$Phi*24 +16)%%24
sumWS$Phi <- (sumWS$Phi*24 +16)%%24

sumVE$meta2d_phase <- (sumVE$meta2d_phase +16)%%24
sumSS$meta2d_phase <- (sumSS$meta2d_phase +16)%%24
sumAE$meta2d_phase <- (sumAE$meta2d_phase +16)%%24
sumWS$meta2d_phase <- (sumWS$meta2d_phase +16)%%24

rownames(sumVE) <- sumVE$name
rownames(sumSS) <- sumSS$name
rownames(sumAE) <- sumAE$name
rownames(sumWS) <- sumWS$name



URAE = 0.4
Ltau = 20
Utau = 28
m2p = 0.001

rgListF <- list(
 VE=sumVE[!is.na(sumVE$Tau) & sumVE$Tau > LTau & sumVE$Tau < Utau & sumVE$RAE < URAE & substr(sumVE$name,1,1)=="g" ,"name"],
 SS=sumSS[!is.na(sumSS$Tau) & sumSS$Tau > LTau & sumSS$Tau < Utau & sumSS$RAE < URAE & substr(sumSS$name,1,1)=="g" ,"name"],
 AE=sumAE[!is.na(sumAE$Tau) & sumAE$Tau > LTau & sumAE$Tau < Utau & sumAE$RAE < URAE & substr(sumAE$name,1,1)=="g" ,"name"],
 WS=sumWS[!is.na(sumWS$Tau) & sumWS$Tau > LTau & sumWS$Tau < Utau & sumWS$RAE < URAE & substr(sumWS$name,1,1)=="g" ,"name"]
)

rgListM <- list(
 VE=sumVE[!is.na(sumVE$Tau) & sumVE$Tau > LTau & sumVE$Tau < Utau & sumVE$meta2d_pvalue < m2p & substr(sumVE$name,1,1)=="g" ,"name"],
 SS=sumSS[!is.na(sumSS$Tau) & sumSS$Tau > LTau & sumSS$Tau < Utau & sumSS$meta2d_pvalue < m2p & substr(sumSS$name,1,1)=="g" ,"name"],
 AE=sumAE[!is.na(sumAE$Tau) & sumAE$Tau > LTau & sumAE$Tau < Utau & sumAE$meta2d_pvalue < m2p & substr(sumAE$name,1,1)=="g" ,"name"],
 WS=sumWS[!is.na(sumWS$Tau) & sumWS$Tau > LTau & sumWS$Tau < Utau & sumWS$meta2d_pvalue < m2p & substr(sumWS$name,1,1)=="g" ,"name"]
)

rgListFM <- list(
 VE=sumVE[!is.na(sumVE$Tau) & sumVE$Tau > LTau & sumVE$Tau < Utau & sumVE$RAE < URAE & sumVE$meta2d_pvalue < m2p & substr(sumVE$name,1,1)=="g"  ,"name"],
 SS=sumSS[!is.na(sumSS$Tau) & sumSS$Tau > LTau & sumSS$Tau < Utau & sumSS$RAE < URAE & sumSS$meta2d_pvalue < m2p & substr(sumSS$name,1,1)=="g"  ,"name"],
 AE=sumAE[!is.na(sumAE$Tau) & sumAE$Tau > LTau & sumAE$Tau < Utau & sumAE$RAE < URAE & sumAE$meta2d_pvalue < m2p & substr(sumAE$name,1,1)=="g"  ,"name"],
 WS=sumWS[!is.na(sumWS$Tau) & sumWS$Tau > LTau & sumWS$Tau < Utau & sumWS$RAE < URAE & sumWS$meta2d_pvalue < m2p & substr(sumWS$name,1,1)=="g"  ,"name"]
)


rgListFMA <- list(
 VE=sumVE[!is.na(sumVE$Tau) & sumVE$Tau > LTau & sumVE$Tau < Utau & sumVE$RAE < URAE & sumVE$meta2d_pvalue < m2p & substr(sumVE$name,1,1)=="g" & sumVE$Amp/2/sumVE$CONS > 0.05  ,"name"],
 SS=sumSS[!is.na(sumSS$Tau) & sumSS$Tau > LTau & sumSS$Tau < Utau & sumSS$RAE < URAE & sumSS$meta2d_pvalue < m2p & substr(sumSS$name,1,1)=="g" & sumSS$Amp/2/sumSS$CONS > 0.05 ,"name"],
 AE=sumAE[!is.na(sumAE$Tau) & sumAE$Tau > LTau & sumAE$Tau < Utau & sumAE$RAE < URAE & sumAE$meta2d_pvalue < m2p & substr(sumAE$name,1,1)=="g" & sumAE$Amp/2/sumAE$CONS > 0.05 ,"name"],
 WS=sumWS[!is.na(sumWS$Tau) & sumWS$Tau > LTau & sumWS$Tau < Utau & sumWS$RAE < URAE & sumWS$meta2d_pvalue < m2p & substr(sumWS$name,1,1)=="g" & sumWS$Amp/2/sumWS$CONS > 0.05 ,"name"]
)



