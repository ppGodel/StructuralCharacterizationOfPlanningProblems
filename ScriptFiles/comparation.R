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
boxplot(ares$BBPL~ares$D)
boxplot(ares$BBT~ares$D)
boxplot(ares$HPL~ares$D)
boxplot(ares$HT~ares$D)
boxplot(ares$IPL~ares$D)
boxplot(ares$IT~ares$D)
boxplot(ares$SPL~ares$D)
boxplot(ares$ST~ares$D)
aptres=apres
aptres=merge(aptres, bbres, by='PN')
boxplot(aptres$Time~ aptres$D)
boxplot(aptres$Time~ aptres$D+aptres$Planner)
boxplot(aptres$PL.x~ aptres$D)
par(mar=c(10.1, 4.1, 4.1, 4.1), xpd=TRUE)
boxplot(aptres$PL.x~ aptres$D+aptres$Planner, las=2)
par(mar=c(10.1, 4.1, 4.1, 4.1), xpd=TRUE)
boxplot(aptres$PL.x~ aptres$Planner+aptres$D, las=2)


par(mar=c(10.1, 4.1, 4.1, 4.1), xpd=TRUE)
boxplot(log(aptres$Time,2)~aptres$D+aptres$Planner, las=2, col=rainbow(6))
par(mar=c(10.1, 4.1, 4.1, 4.1), xpd=TRUE)
boxplot(log(aptres$Time,2)~aptres$Planner+aptres$D, las=2, col=c("olivedrab","firebrick","mediumorchid1","darkturquoise"))

#agregar descripcion de escala logaritmica, abline para separar grupos, acomodar colores de agrupar, nombres alos ejes
#crear grafica con ejes (PL,T) donde los planners son colores y el dominio la figura

