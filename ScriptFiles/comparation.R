apres=read.csv('compres.csv', header=FALSE)
bbres=read.csv('result98.csv', header=FALSE)
apres$V5=NULL
bbres$V9=NULL
colnames(apres)=c('PN','Planner','Time','PL')
colnames(bbres)=c('PN','FN','ADD','Competition', 'D','E','PL','GE')
apBres=cbind.data.frame(PN=apres[apres$Planner=='BLACKBOX',]$PN,BBT=apres[apres$Planner=='BLACKBOX',]$Time,BBPL=apres[apres$Planner=='BLACKBOX',]$PL)
apHres=cbind.data.frame(PN=apres[apres$Planner=='HSP',]$PN,HT=apres[apres$Planner=='HSP',]$Time,HPL=apres[apres$Planner=='HSP',]$PL)
apIres=cbind.data.frame(PN=apres[apres$Planner=='IPP',]$PN,IT=apres[apres$Planner=='IPP',]$Time,IPL=apres[apres$Planner=='IPP',]$PL)
apSres=cbind.data.frame(PN=apres[apres$Planner=='STAN',]$PN,ST=apres[apres$Planner=='STAN',]$Time,SPL=apres[apres$Planner=='STAN',]$PL)
ares=bbres
ares=merge(ares,apBres, by='PN', all.x=TRUE )
ares=merge(ares,apHres, by='PN', all.x=TRUE )
ares=merge(ares,apIres, by='PN', all.x=TRUE )
ares=merge(ares,apSres, by='PN', all.x=TRUE )
ares[ares==NA]=0
write.csv(ares, "ares.csv")
png(paste("bxplt_BB_Domain_length.png",sep=""),width=620, height=480)
boxplot(ares$BBPL~ares$D, main="Planner Blackbox", xlab="Domain", ylab="Plan length")
graphics.off()
png(paste("bxplt_BB_Domain_Time.png",sep=""),width=620, height=480)
par(mar=c(4.1, 7.1, 4.1, 4.1), xpd=TRUE)
boxplot(log(ares$BBT,10)~ares$D, yaxt="n", xlab="Domain")
axis(side=2, labels=paste("10^",seq(1,4,0.5),sep=""),cex.axis=0.8, at=seq(1,4,0.5), las=2)
mtext("Time (sec)", side=2, line=4)
graphics.off()

png(paste("bxplt_HSP_Domain_length.png",sep=""),width=620, height=480)
boxplot(ares$HPL~ares$D, main="Planner HSP", xlab="Domain", ylab="Plan length")
graphics.off()
png(paste("bxplt_HSP_Domain_Time.png",sep=""),width=620, height=480)
par(mar=c(4.1, 7.1, 4.1, 4.1), xpd=TRUE)
boxplot(log(ares$HT)~ares$D, main="Planner HSP", yaxt="n", xlab="Domain")
axis(side=2, labels=paste("10^",seq(8,13,1),sep=""),cex.axis=0.8, at=seq(8,13,1), las=2)
mtext("Time (sec)", side=2, line=4)
graphics.off()

png(paste("bxplt_IPP_Domain_length.png",sep=""),width=620, height=480)
boxplot(ares$IPL~ares$D, main="Planner IPP", xlab="Domain", ylab="Plan length")
graphics.off()
png(paste("bxplt_IPP_Domain_Time.png",sep=""),width=620, height=480)
par(mar=c(4.1, 7.1, 4.1, 4.1), xpd=TRUE)
boxplot(log(ares$IT)~ares$D, main="Planner IPP", yaxt="n", xlab="Domain")
axis(side=2, labels=paste("10^",seq(2,12,1),sep=""),cex.axis=0.8, at=seq(2,12,1), las=2)
mtext("Time (sec)", side=2, line=4)
graphics.off()

png(paste("bxplt_STAN_Domain_length.png",sep=""),width=620, height=480)
boxplot(ares$SPL~ares$D, main="Planner STAN", xlab="Domain", ylab="Plan length")
graphics.off()
png(paste("bxplt_STAN_Domain_Time.png",sep=""),width=620, height=480)
par(mar=c(4.1, 7.1, 4.1, 4.1), xpd=TRUE)
boxplot(log(ares$ST)~ares$D, main="Planner STAN", yaxt="n", xlab="Domain")
axis(side=2, labels=paste("10^",seq(3,15,1),sep=""), cex.axis=0.9, at=seq(3,15,1), las=2)
mtext("Time (sec)", side=2, line=4)
graphics.off()

aptres=apres
aptres=merge(aptres, bbres, by='PN')
pl=levels(aptres$Planner)
dl=levels(aptres$D)
boxplot(log(aptres$Time,10)~ aptres$D)

png(paste("bxplt_ALL_Domain_Time.png",sep=""),width=620, height=480)
par(mar=c(4.1, 6.1, 4.1, 4.1), xpd=TRUE)
boxplot(log(aptres$Time,10)~ aptres$D, main="Planners Time", yaxt="n", xlab="Domain")
axis(side=2, labels=paste("10^",seq(1,6,1),sep=""), cex.axis=0.9, at=seq(1,6,1), las=2)
mtext("Time (sec)", side=2, line=4)
graphics.off()

png(paste("bxplt_All_Domain_GPlanner_Time.png",sep=""),width=620, height=480)
lgv=log(aptres$Time,10)
par(mar=c(4.1, 6.1, 4.1, 10.1), xpd=TRUE)
#boxplot(log(aptres$Time,10)~aptres$D+aptres$Planner, las=2, col=rainbow(6))
boxplot(log(aptres$Time,10)~aptres$D+aptres$Planner, las=2, col=rainbow(length(dl)), xaxt="n", yaxt="n")

axis(side=2, labels=paste("10^",seq(min(lgv),max(lgv),1),sep=""), cex.axis=0.9, at=seq(min(lgv),max(lgv),1), las=2)
axis(side=1, labels=sort(pl), cex.axis=0.9, at=seq(4,22,6))
abline(v=7,xpd=FALSE)
abline(v=13,xpd=FALSE)
abline(v=19,xpd=FALSE)
legend(length(levels(aptres$D))*length(levels(aptres$Planner))+2, max(log(aptres$Time,10)), dl, cex = 0.8,fill=rainbow(length(dl)) )
graphics.off()


#boxplot(aptres$PL.x~ aptres$D)
png(paste("bxplt_ALL_Domain_length.png",sep=""),width=620, height=480)
boxplot(aptres$PL.x~ aptres$D, main="All planners lenght", ylab="Plan length", xlab="Domain")
graphics.off()

#boxplot(aptres$PL.x~ aptres$D+aptres$Planner, las=2)
png(paste("bxplt_All_Domain_GPlanner_lenght.png",sep=""),width=620, height=480)
par(mar=c(4.1, 6.1, 4.1, 10.1), xpd=TRUE)
boxplot(aptres$PL.x~ aptres$D+aptres$Planner, las=2, col=rainbow(length(dl)), xaxt="n", ylab="Plan length")

axis(side=1, labels=sort(pl), cex.axis=0.9, at=seq(4,22,6))
abline(v=7,xpd=FALSE)
abline(v=13,xpd=FALSE)
abline(v=19,xpd=FALSE)
legend(length(levels(aptres$D))*length(levels(aptres$Planner))+2, max(aptres$PL.x), dl, cex = 0.8,fill=rainbow(length(dl)) )
graphics.off()


#boxplot(aptres$PL.x~ aptres$Planner+aptres$D, las=2)
png(paste("bxplt_All_GDomain_Planner_lenght.png",sep=""),width=620, height=480)
par(mar=c(4.1, 6.1, 4.1, 10.1), xpd=TRUE)
boxplot(aptres$PL.x~ aptres$Planner+aptres$D, las=2, col=rainbow(length(pl)), xaxt="n", ylab="Plan length")

axis(side=1, labels=sort(dl), cex.axis=0.9, at=seq(2,24,length(pl)))
abline(v=4.5,xpd=FALSE)
abline(v=8.5,xpd=FALSE)
abline(v=12.5,xpd=FALSE)
abline(v=16.5,xpd=FALSE)
abline(v=20.5,xpd=FALSE)
legend(length(levels(aptres$D))*length(levels(aptres$Planner))+2, max(aptres$PL.x), pl, cex = 0.8,fill=rainbow(length(pl)) )
graphics.off()

png(paste("bxplt_All_GDomain_Planner_Time.png",sep=""),width=620, height=480)
par(mar=c(4.1, 6.1, 4.1, 10.1), xpd=TRUE)
boxplot(log(aptres$Time,10)~aptres$Planner+aptres$D, las=2, col=rainbow(length(pl)), xaxt="n", ylab="Time (Sec)")

axis(side=1, labels=sort(dl), cex.axis=0.9, at=seq(3,25,length(pl)))
abline(v=4.5,xpd=FALSE)
abline(v=8.5,xpd=FALSE)
abline(v=12.5,xpd=FALSE)
abline(v=16.5,xpd=FALSE)
abline(v=20.5,xpd=FALSE)
legend(length(levels(aptres$D))*length(levels(aptres$Planner))+2, max(log(aptres$Time,10)), pl, cex = 0.8,fill=rainbow(length(pl)) )

graphics.off()

#agregar descripcion de escala logaritmica, abline para separar grupos, acomodar colores de agrupar, nombres alos ejes
#crear grafica con ejes (PL,T) donde los planners son colores y el dominio la figura

#cbind.data.frame(aptres$Time, aptres$PL.y, aptres$PL.x, aptres$D, aptres$Planner)



png(paste("bxplt_All_Time_length.png",sep=""),width=960, height=720)
par(mar=c(4.1, 6.1, 4.1, 10.1), xpd=TRUE)
plot(x=1,y=1, yaxt="n", type="n",ylim=c(1,8), xlim=c(1,200), xlab="Plan length", ylab="Time (sec)" )
axis(side=2, labels=paste("10^",seq(1,8,1),sep=""), cex.axis=0.9, at=seq(1,8,1), las=2)
colores=c("orange","black","red","blue","green", "brown")
i=1
for( lvl in levels(ares$D)){
    points(x=ares[ares$D==lvl,]$BBPL, y=log(ares[ares$D==lvl,]$BBT,10),col=colores[i],pch=16, cex=1)
    points(x=ares[ares$D==lvl,]$HPL, y=log(ares[ares$D==lvl,]$HT,10),col=colores[i],pch=17, cex=1)
    points(x=ares[ares$D==lvl,]$IPL, y=log(ares[ares$D==lvl,]$IT,10),col=colores[i],pch=18, cex=1)
    points(x=ares[ares$D==lvl,]$SPL, y=log(ares[ares$D==lvl,]$ST,10),col=colores[i],pch=15, cex=1)
    i=i+1
}

legend(215, 8, pl, cex = 1.2,pch=c(16,17,18,15) )

legend(215, 5, levels(ares$D), cex = 1.2,pch=c(16), col=colores )
graphics.off()


png(paste("bxplt_BB_Time_length.png",sep=""),width=960, height=720)
par(mar=c(4.1, 6.1, 4.1, 10.1), xpd=TRUE)
plot(x=1,y=1, yaxt="n", type="n",ylim=c(1,8), xlim=c(1,200), xlab="Plan length", ylab="Time (sec)" )
axis(side=2, labels=paste("10^",seq(1,8,1),sep=""), cex.axis=0.9, at=seq(1,8,1), las=2)
colores=c("orange","black","red","blue","green", "brown")
i=1
for( lvl in levels(ares$D)){
    points(x=ares[ares$D==lvl,]$BBPL, y=log(ares[ares$D==lvl,]$BBT,10),col=colores[i],pch=16, cex=1)
    i=i+1
}

legend(215, 5, levels(ares$D), cex = 1.2,pch=c(16), col=colores )
graphics.off()


png(paste("bxplt_HSP_Time_length.png",sep=""),width=960, height=720)
par(mar=c(4.1, 6.1, 4.1, 10.1), xpd=TRUE)
plot(x=1,y=1, yaxt="n", type="n",ylim=c(1,8), xlim=c(1,200), xlab="Plan length", ylab="Time (sec)" )
axis(side=2, labels=paste("10^",seq(1,8,1),sep=""), cex.axis=0.9, at=seq(1,8,1), las=2)
colores=c("orange","black","red","blue","green", "brown")
i=1
for( lvl in levels(ares$D)){
    points(x=ares[ares$D==lvl,]$HPL, y=log(ares[ares$D==lvl,]$HT,10),col=colores[i],pch=17, cex=1)
    i=i+1
}

legend(215, 5, levels(ares$D), cex = 1.2,pch=c(16), col=colores )
graphics.off()


png(paste("bxplt_ISP_Time_length.png",sep=""),width=960, height=720)
par(mar=c(4.1, 6.1, 4.1, 10.1), xpd=TRUE)
plot(x=1,y=1, yaxt="n", type="n",ylim=c(1,8), xlim=c(1,200), xlab="Plan length", ylab="Time (sec)" )
axis(side=2, labels=paste("10^",seq(1,8,1),sep=""), cex.axis=0.9, at=seq(1,8,1), las=2)
colores=c("orange","black","red","blue","green", "brown")
i=1
for( lvl in levels(ares$D)){
    points(x=ares[ares$D==lvl,]$IPL, y=log(ares[ares$D==lvl,]$IT,10),col=colores[i],pch=18, cex=1)
    i=i+1
}
legend(215, 5, levels(ares$D), cex = 1.2,pch=c(16), col=colores )
graphics.off()


png(paste("bxplt_STAN_Time_length.png",sep=""),width=960, height=720)
par(mar=c(4.1, 6.1, 4.1, 10.1), xpd=TRUE)
plot(x=1,y=1, yaxt="n", type="n",ylim=c(1,8), xlim=c(1,200), xlab="Plan length", ylab="Time (sec)" )
axis(side=2, labels=paste("10^",seq(1,8,1),sep=""), cex.axis=0.9, at=seq(1,8,1), las=2)
colores=c("orange","black","red","blue","green", "brown")
i=1
for( lvl in levels(ares$D)){
    points(x=ares[ares$D==lvl,]$SPL, y=log(ares[ares$D==lvl,]$ST,10),col=colores[i],pch=15, cex=1)
    i=i+1
}
legend(215, 5, levels(ares$D), cex = 1.2,pch=c(16), col=colores )
graphics.off()
