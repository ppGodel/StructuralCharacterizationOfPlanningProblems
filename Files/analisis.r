suppressMessages(library(ggplot2))
args = commandArgs(trailingOnly=TRUE)
bn = "strips-mysty-x-19distedges"
fn = paste(bn,'.csv',sep='')
if(length(args)>0)
    fn=args[1]
if(length(args)>1)
    bn=args[2]
gp = read.csv()
#normalizacion de bc
mv = min(gp$bc)
gp$bc = gp$bc - mv
mv = max(gp$bc)
gp$bc = gp$bc / mv

png(paste(bn, "dadbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$ad)) + labs( x="Level", y="Distribution all degree" ) + geom_violin()  + geom_boxplot(width=0.05) + theme(text = element_text(size=20))
graphics.off()
png(paste(bn, "didbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$ind)) + labs( x="Level", y="Distribution in degree" ) + geom_violin()  + geom_boxplot(width=0.05) + theme(text = element_text(size=20))
graphics.off()
png(paste(bn, "dodbl.png" ,sep=""))
ggplot(data = gp, aes(x=factor(gp$time), y=gp$otd)) + labs( x="Level", y="Distribution out degree" ) + geom_violin()  + geom_boxplot(width=0.05) + theme(text = element_text(size=20))
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
png(paste(bn, "dod.png" ,sep=""))
hist(gp$otd)
graphics.off()
png(paste(bn, "dbc.png" ,sep=""))
hist(gp$bc)
graphics.off()
