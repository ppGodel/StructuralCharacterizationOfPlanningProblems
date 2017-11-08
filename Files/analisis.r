suppressMessages(library(ggplot2))

args = commandArgs(trailingOnly=TRUE)
bn = "strips-mysty-x-19distedges"
fn = paste(bn,'.csv',sep='')
if(length(args)>0)
    fn=args[1]
if(length(args)>1)
    bn=args[2]
gp = read.csv(fn)
#normalizacion de bc
mv = min(gp$bc)
gp$bc = gp$bc - mv
mv = max(gp$bc)
gp$bc = gp$bc / mv

png(paste(bn, "dadbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$ad)) + labs( x="Level", y="Distribution all degree" ) + geom_violin()  + geom_boxplot(width=0.05) + theme(text = element_text(size=20))
graphics.off()

png(paste(bn, "diidbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$iid)) + labs( x="Level", y="Distribution in degree" ) + geom_violin()  + geom_boxplot(width=0.05) + theme(text = element_text(size=20))
graphics.off()


png(paste(bn, "diidbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$iid)) + labs( x="Level", y="Distribution in degree" ) + geom_violin()  + geom_boxplot(width=.1) + theme(text = element_text(size=20))
graphics.off()

png(paste(bn, "diodbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$iod)) + labs( x="Level", y="Distribution out degree" ) + geom_violin()  + geom_boxplot(width=0.1) + theme(text = element_text(size=20))
graphics.off()

png(paste(bn, "diddbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$idd)) + labs( x="Level", y="Distribution del degree" ) + geom_violin()  + geom_boxplot(width=0.1) + theme(text = element_text(size=20))
graphics.off()

png(paste(bn, "dixdbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$ixd)) + labs( x="Level", y="Distribution exclusive degree" ) + geom_violin()  + geom_boxplot(width=0.1) + theme(text = element_text(size=20))
graphics.off()

png(paste(bn, "disdbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$isd)) + labs( x="Level", y="Distribution in degree" ) + geom_violin()  + geom_boxplot(width=0.1) + theme(text = element_text(size=20))
graphics.off()



png(paste(bn, "dodbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$otd)) + labs( x="Level", y="Distribution out degree" ) + geom_violin()  + geom_boxplot(width=0.05) + theme(text = element_text(size=20))
graphics.off()



png(paste(bn, "doidbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$oid)) + labs( x="Level", y="Distribution in degree" ) + geom_violin()  + geom_boxplot(width=.1) + theme(text = element_text(size=20))
graphics.off()

png(paste(bn, "doodbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$ood)) + labs( x="Level", y="Distribution out degree" ) + geom_violin()  + geom_boxplot(width=0.1) + theme(text = element_text(size=20))
graphics.off()

png(paste(bn, "doddbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$odd)) + labs( x="Level", y="Distribution del degree" ) + geom_violin()  + geom_boxplot(width=0.1) + theme(text = element_text(size=20))
graphics.off()

png(paste(bn, "doxdbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$oxd)) + labs( x="Level", y="Distribution exclusive degree" ) + geom_violin()  + geom_boxplot(width=0.1) + theme(text = element_text(size=20))
graphics.off()

png(paste(bn, "dosdbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$osd)) + labs( x="Level", y="Distribution in degree" ) + geom_violin()  + geom_boxplot(width=0.1) + theme(text = element_text(size=20))
graphics.off()




png(paste(bn, "dbcbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$bc)) + labs( x="Level", y="Distribution betweenness centrality" ) + geom_violin()  + geom_boxplot(width=0.05) + theme(text = element_text(size=20))
graphics.off()


png(paste(bn, "dad.png" ,sep=""))
hist(gp$ad)
graphics.off()
png(paste(bn, "did.png" ,sep=""))
hist(gp$ind)
graphics.off()

png(paste(bn, "diid.png" ,sep=""))
hist(gp$iid)
graphics.off()
png(paste(bn, "diod.png" ,sep=""))
hist(gp$iod)
graphics.off()
png(paste(bn, "didd.png" ,sep=""))
hist(gp$idd)
graphics.off()
png(paste(bn, "dixd.png" ,sep=""))
hist(gp$ixd)
graphics.off()
png(paste(bn, "disd.png" ,sep=""))
hist(gp$isd)
graphics.off()

png(paste(bn, "dod.png" ,sep=""))
hist(gp$otd)
graphics.off()



#png(paste(bn, "dbc.png" ,sep=""))
#hist(gp$bc)
#graphics.off()
