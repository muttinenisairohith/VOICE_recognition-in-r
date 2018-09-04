snd <- readWave("E:/pro_hand/samples/teja3.wav")
snd2 <- readWave("E:/pro_hand/samples/jas2.wav")
s1 <- snd@left
s1 <- s1 / 2^(snd@bit -1)
#plot(s1)
a1 = analyze(s1, samplingRate = 16000, plot = TRUE, ylim = c(0, 4))
#summary(a1)
#median(a1$pitch, na.rm = TRUE)
#median(a1$amp1Voiced, na.rm = TRUE)
#library(xlsx)
a2 <- a1[a1$voiced == "TRUE",]
t1 <- a2$time[1]*16.02
n=nrow(a2)
t2 <- a2$time[n-1]*16.02
snd1 <- snd[t1:t2]
play(snd1)
play(snd)
plot(snd1@left)
m1 <- melfcc(snd1)
m0 <- melfcc(snd)
#plot(m1)
#plot(m0)
write.xlsx(m1, "E:/pro_hand/teja/teja2pitch.xlsx")
write.xlsx(a2, "E:/pro_hand/teja/teja2mfcc.xlsx")
s11 <- snd1@left / 2^(snd1@bit -1)
a11 <- analyze(s11, samplingRate = 16000, plot = TRUE, ylim = c(0, 4))
#------------------------------------------------------

s2 <- snd2@left
s2 <- s2 / 2^(snd2@bit -1)
#plot(s1)
a3 = analyze(s2, samplingRate = 16000, plot = TRUE, ylim = c(0, 4))
#summary(a1)
#median(a1$pitch, na.rm = TRUE)
#median(a1$amp1Voiced, na.rm = TRUE)
#library(xlsx)
a4 <- a3[a3$voiced == "TRUE",]
t3 <- a4$time[1]*16.02
n=nrow(a4)
t4 <- a4$time[n-1]*16.02
snd3 <- snd2[t3:t4]
play(snd3)
#play(snd2)
#plot(snd2@left)
m3 <- melfcc(snd3)
m2 <- melfcc(snd2)
#plot(m1)
#plot(m0)
#write.xlsx(m1, "E:/pro_hand/teja/teja2pitch.xlsx")
#write.xlsx(a2, "E:/pro_hand/teja/teja2mfcc.xlsx")
s22 <- snd3@left / 2^(snd3@bit -1)
a22 <- analyze(s22, samplingRate = 16000, plot = TRUE, ylim = c(0, 4))
#-------------------------------------------------------
dtw1 <- dtw(m1,m3,dist.method="eJaccard")
plot(dtw1)
plot(dtw1@normalizedDistance)
#-------------------------------------------------------
snd1 <- readWave("E:/pro_hand/samples/none2.wav")
plot(s2 <- snd1@left)
s2 <- s2 / 2^(snd1@bit -1)
a2 = analyze(s2, samplingRate = 16000, plot = TRUE, ylim = c(0, 4))
summary(a2)
median(a2$pitch, na.rm = TRUE)
median(a2$amp1Voiced, na.rm = TRUE)
library(xlsx)
write.xlsx(a2, "E:/123/none2.xlsx")
#----------------------------------------------
p <- analyzeFolder("E:/123/samples/", summary = FALSE,  verbose = TRUE)

write.xlsx(p, "E:/123/features/samples1.xlsx")
#----------------------------------------------

plot(s1 <- snd@left)
s1 <- s1 / 2^(snd@bit -1)
s3 <- decimate(s1,2)
t=0:132096/2
plot(s1)
t <- seq(1, 132096, by = 1)
y <- decimate(s1, 10) # factor of 4 decimation
plot(t, s1, type = "l")
lines(t[seq(1,length(t), by = 10)], y, col = "blue")
plot(y)
#-------------------------------------------------------
plot(fft(s1))
spec(s1)
spec(s1,16000)
sn1<-rmnoise(s1,1000,output = "wave")
plot(sn1)
#-------------------------------------------------------
psd <- pwelch(s1,seglength=1,windowingPsdCorrection=FALSE)
#-----------------------------------------------------------

dir <- "E:/pro_hand/samples/"
dire <- "E:/pro_hand/feat/"
labels <- c("rahulhello","rohith","rohithhello")
for (a in labels){
  i <- 1
  while(i<=7){
    dir1 <- paste(dir,a,i,".wav",sep="")
    print(dir1)
    snd <- readWave(dir1)
    s1 <- snd@left
    s1 <- s1 / 2^(snd@bit -1)
    a1 = analyze(s1, samplingRate = 16000, plot = TRUE, ylim = c(0, 4))
    write.xlsx(a1, paste(dire,a,i,".xlsx",sep=""))
    a2 <- a1[a1$voiced == "TRUE",]
    n <- nrow(a2)
    print(n)
    t1 <- a2$time[1]*16.02
    t2 <- a2$time[n-1]*16.02
    snd1 <- snd[t1:t2]
    #play(snd1)
    s11 <- snd1@left / 2^(snd1@bit -1)
    a11 <- analyze(s11, samplingRate = 16000, plot = TRUE, ylim = c(0, 4))
    m1 <- melfcc(snd1)
    m0 <- melfcc(snd)
    write.xlsx(a11,paste(dire,a,i,"h",".xlsx",sep=""))
    write.xlsx(m1,paste(dire,a,i,"mfccf",".xlsx",sep=""))
    write.xlsx(m1,paste(dire,a,i,"mfcch",".xlsx",sep=""))
    i <- i+1
  }
}
SpeedOfSound(100,1)
cepstro(snd)
coh(snd,snd2,4000)
combfilter(snd, alpha=0.5, K=1e-4, units="seconds", plot=TRUE)
corenv(snd,snd2,f=22050)
#-----------------------------------------------------------
i=1
snd <- readWave("E:/pro_hand/samples/teja7.wav")
s1 <- snd@left
s1 <- s1 / 2^(snd@bit -1)
#plot(s1)
a1 = analyze(s1, samplingRate = 16000, plot = TRUE, ylim = c(0, 4))
#summary(a1)
#median(a1$pitch, na.rm = TRUE)
#median(a1$amp1Voiced, na.rm = TRUE)
#library(xlsx)
a2 <- a1[a1$voiced == "TRUE",]
t1 <- a2$time[1]*16.02
n=nrow(a2)
t2 <- a2$time[n-1]*16.02
snd1 <- snd[t1:t2]
#play(snd1)
#play(snd)
#plot(snd1@left)
m1 <- melfcc(snd1)
m0 <- melfcc(snd)
#plot(m1)
#plot(m0)
#write.xlsx(m1, "E:/pro_hand/teja/teja2pitch.xlsx")
#write.xlsx(a2, "E:/pro_hand/teja/teja2mfcc.xlsx")
#s11 <- snd1@left / 2^(snd1@bit -1)
#a11 <- analyze(s11, samplingRate = 16000, plot = TRUE, ylim = c(0, 4))
i=8
while(i<11){
  snd2 <- readWave(paste("E:/pro_hand/testing_data/teja",i,".wav",sep=""))
 # snd2 <- readWave(paste("E:/pro_hand/samples/rahul",i,".wav",sep=""))
  s2 <- snd2@left
  s2 <- s2 / 2^(snd2@bit -1)
  #plot(s1)
  a3 = analyze(s2, samplingRate = 16000, plot = TRUE, ylim = c(0, 4))
  #summary(a1)
  #median(a1$pitch, na.rm = TRUE)
  #median(a1$amp1Voiced, na.rm = TRUE)
  #library(xlsx)
  a4 <- a3[a3$voiced == "TRUE",]
  t3 <- a4$time[1]*16.02
  n=nrow(a4)
  t4 <- a4$time[n-1]*16.02
  snd3 <- snd2[t3:t4]
  #play(snd3)
  #play(snd2)
  #plot(snd2@left)
  m3 <- melfcc(snd3)
  m2 <- melfcc(snd2)
  #plot(m1)
  #plot(m0)
  dtw1 <- dtw(m1,m3,dist.method="eJaccard")
 # write.xlsx(dtw1[["normalizedDistance"]],file = "E:/pro_hand/result.xlsx",append = TRUE)
  #write.table(dtw1[["normalizedDistance"]],file = "E:/pro_hand/result.csv",append=T,sep=",")
  print(dtw1[["normalizedDistance"]])
  i=i+1
}
i=1
