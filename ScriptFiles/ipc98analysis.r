library('psych')
library('ggplot2')
library('plyr')

typu="png"
typg<<-"eps"
"/" <- function(x,y) {ifelse(y==0,0,base:::"/"(x,y))}
gpath="images/"
imprimirini= function(typ, name){
    typg<<-typ
    if(typg=="eps"){
        postscript(paste(gpath,name,".eps", sep=""), width=32,height=18, units = 'in', res= 300)
        #dev.print(file=paste(name,".eps", sep=""), device=eps, width=1440,height=960,units = "px")
    }
    else
    {
        png(paste(gpath,name,".png", sep=""), width=32,height=18, units = 'in', res=300)
        #dev.print(file=paste(name,".png", sep=""), device=png, width=1440,height=960)
    }
    
}
imprimirfin= function(){
    if(typg=="eps"){
        dev.off()
    }
    else
    {
        dev.off()
        graphics.off()
    }
    
}

distfunc= function(dis){
    lol = hist(dis,breaks=seq(0,1,0.1))
    lol3 = lol$counts
    h1=which.max(lol3)
    hl=order(lol3,decreasing=T)[2:round((1+length(lol3))*0.5)]
    if(h1>which.max(hl)){
        h2=min(hl)-1+which.max(lol3[min(hl):(h1-1)])

    }else{
        h2=min(hl)-1+which.max((h1+1):lol3[max(hl)])

    }
    m= which.min(lol3[h1:h2])
}

joinall= function(){
    datar = read.file(f=paste("strips-gripper-all.csv", sep=''))
    datar=cbind(dom=rep("gripper",nrow(datar)),pn=sub(".*x-", "",datar$gn),datar)
    fns=c( "log","mprime","movie","mystery")
    for(fn in fns){
        datar2 = read.file(f=paste("strips-",fn,"-all.csv", sep=''))
        datar2=cbind(dom=rep(fn,nrow(datar2)),pn=sub(".*x-", "",datar2$gn),datar2)
        datar=rbind(datar,datar2)
    }
    return(datar)
}

#fn="gripper"
#fns=c("gripper", "log","mprime","movie")
compresultsraw = read.file(f="fullresultIPC98.csv")
compresultsbase=aggregate(cbind(ts,fa)~probname+comp+dom, compresultsraw, max)
compresultstan=aggregate(cbind(stantime=ptime,stansteps=psteps)~probname+comp+dom, compresultsraw[compresultsraw$planner=="STAN",], max)
compresultsbb=aggregate(cbind(bbtime=ptime,bbsteps=psteps)~probname+comp+dom, compresultsraw[compresultsraw$planner=="BLACKBOX",], max)
compresultsipp=aggregate(cbind(ipptime=ptime,ippsteps=psteps)~probname+comp+dom, compresultsraw[compresultsraw$planner=="IPP",], max)
compresultshsp=aggregate(cbind(hsptime=ptime,hspsteps=psteps)~probname+comp+dom, compresultsraw[compresultsraw$planner=="HSP",], max)
compresultMIN=aggregate(cbind(mintime=ptime,minsteps=psteps)~probname+comp+dom, compresultsraw, function(x) {min(x[x > 0], na.rm=T)})
#merge(x=compresultsbase, y=compresultstan, by=c("probname","comp","dom"))
compresults=join_all(dfs=list(compresultsbase,compresultstan,compresultsbb,compresultsipp,compresultshsp,compresultMIN), by=c("probname","comp","dom"),type="full")

#for(fn in fns){
#    dataraw = read.file(f=paste("strips-",fn,"-all.csv", sep=''))
dataraw=joinall()

                                        #by graph
    #nodes
factsbg=aggregate(hash~gn+dom+pn, dataraw[dataraw$type=="f",], length)
actionsbg=aggregate(hash~gn+dom+pn, dataraw[dataraw$type=="a",], length)
                                        #edges
tedgbg=aggregate(cbind(ind,ixd,otd,oxd,ad)~gn+dom+pn, dataraw, sum)
summedgbg=aggregate(cbind(ind,ixd,otd,oxd,ad)~gn+dom+pn+time+type, dataraw, summary)
                                        #by level
    #nodes
nodesbl=aggregate(hash~gn+dom+pn+time+type, dataraw, length)
factsbl=aggregate(hash~gn+dom+pn+time, dataraw[dataraw$type=="f",], length)
actionsbl=aggregate(hash~gn+dom+pn+time, dataraw[dataraw$type=="a",], length)
summactionsbl=aggregate(oxd[,3]~gn+dom+pn+time, dataraw[dataraw$type=="a",], sum)
    #edges
#sumbl=aggregate(cbind(ind,ixd,otd,oxd,ad)~gn+dom+pn+type+time, dataraw, sum)
summbl=aggregate(cbind(ind,ixd,otd,oxd,ad)~gn+dom+pn+type+time, dataraw, summary)
sumbl=aggregate(cbind(ind,ixd,otd,oxd,ad)~gn+pn+dom+type+time, dataraw, sum)
allresult=merge(x=compresults, y=sumbl, by.x=c("probname"), by.y=c("gn"))
info=merge(x=summbl, y=nodesbl, by=c("gn","dom","pn", "time", "type"))

#compresults=join_all(dfs=list(compresultsbase,compresultstan,compresultsbb,compresultsipp,compresultshsp), by=c("probname","comp","dom"),type="full")

    #nodes
imprimirini(typ=typu,name=paste("factcountByGraph",sep=""))
boxplot(hash~dom+pn,data=nodesbl[nodesbl$type=="f",])
imprimirfin()
    
imprimirini(typ=typu,name=paste("factcountByLevel",sep=""))
boxplot(hash~time,data=nodesbl[nodesbl$type=="f",])
imprimirfin()

#
ggplot(data = allresult, aes(x=factor(pn, levels=sort(as.numeric(levels(sumbl$pn)))), y=hash)) + labs( x="Problem", y="Fact nodes amount" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"factcountByProbViol",".",typu, sep=""), device=typu, width=32,height=18)

#

ggplot(data = nodesbl[nodesbl$type=="f",], aes(x=factor(pn, levels=sort(as.numeric(levels(sumbl$pn)))), y=hash)) + labs( x="Problem", y="Fact nodes amount" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"factcountByProbViol",".",typu, sep=""), device=typu, width=32,height=18)
    
ggplot(data = nodesbl[nodesbl$type=="a",], aes(x=factor(pn, levels=sort(as.numeric(levels(sumbl$pn)))), y=hash)) + labs( x="Problem", y="Action nodes amount" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"actioncountByProbVio",".",typu, sep=""), device=typu, width=32,height=18)
    
imprimirini(typ=typu,name=paste("actioncountByLevel",sep=""))
boxplot(hash~time+dom,data=nodesbl[nodesbl$type=="a",])
imprimirfin()
    
imprimirini(typ=typu,name=paste("actioncountByLevel",sep=""))
boxplot(hash~pn+dom,data=nodesbl[nodesbl$type=="a",])
imprimirfin()

    #edges
imprimirini(typ=typu,name=paste("inedgesMedianByLevel",sep=""))
boxplot(ind[,3]~time+dom,data=summbl)
imprimirfin()

imprimirini(typ=typu,name=paste("inexclusiveMedianByLevel",sep=""))
boxplot(ixd[,3]~time+dom,data=summbl)
imprimirfin()

imprimirini(typ=typu,name=paste("outedgesMedianByLevel",sep=""))
boxplot(otd[,3]~time+dom,data=summbl)
imprimirfin()
imprimirini(typ=typu,name=paste("outexclusiveMedianByLevel",sep=""))
boxplot(oxd[,3]~time+dom,data=summbl)
imprimirfin()

imprimirini(typ=typu,name=paste("allMedianByLevel",sep=""))
boxplot(ad[,3]~time+dom,data=summbl)
imprimirfin()

imprimirini(typ=typu,name=paste("inedgesMeanByLevel",sep=""))
boxplot(ind[,4]~time+dom,data=summbl)
imprimirfin()

imprimirini(typ=typu,name=paste("inexclusiveMeanByLevel",sep=""))
boxplot(otd[,4]~time+dom,data=summbl)
imprimirfin()

imprimirini(typ=typu,name=paste("outedgesMeanByLevel",sep=""))
boxplot(otd[,4]~time+dom,data=summbl)
imprimirfin()

imprimirini(typ=typu,name=paste("outexclusiveMeanByLevel",sep=""))
boxplot(oxd[,4]~time+dom,data=summbl)
imprimirfin()

imprimirini(typ=typu,name=paste("allMeanByLevel",sep=""))
boxplot(ad[,4]~time+dom,data=summbl)
imprimirfin()

imprimirini(typ=typu,name=paste("inedgesMAXByLevel",sep=""))
boxplot(ind[,6]~time+dom,data=summbl)
imprimirfin()

imprimirini(typ=typu,name=paste("inexclusiveMAXByLevel",sep=""))
boxplot(otd[,6]~time+dom,data=summbl)
imprimirfin()

imprimirini(typ=typu,name=paste("outedgesMAXByLevel",sep=""))
boxplot(otd[,6]~time+dom,data=summbl)
imprimirfin()

imprimirini(typ=typu,name=paste("outexclusiveMAXByLevel",sep=""))
boxplot(oxd[,6]~time+dom,data=summbl)
imprimirfin()


imprimirini(typ=typu,name=paste("allMAXByLevel",sep=""))
boxplot(ad[,6]~time+dom,data=summbl)
imprimirfin()


ggplot(data = sumbl, aes(x=factor(pn, levels=sort(as.numeric(levels(sumbl$pn)))), y=oxd/otd)) + labs( x="Problem", y="Percentage out mutex by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom, scales="free")
ggsave(paste(gpath,"distPercentageOMbyP",".",typu, sep=""), device=typu, width=32,height=18)

imprimirini(typ=typu,name=paste("distPercentageIMbyP",sep=""))
ggplot(data = sumbl, aes(x=factor(pn, levels=sort(as.numeric(levels(sumbl$pn)))), y=(ixd/ind))) + labs( x="Problem", y="Percentage in mutex by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"distPercentageIMbyP",".",typu, sep=""), device=typu, width=32,height=18)

ggplot(data = sumbl[sumbl$type=="a",], aes(x=factor(pn, levels=sort(as.numeric(levels(sumbl$pn)))), y=(oxd/otd))) + labs( x="Problem", y="Percentage out mutex in actions by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"distPercentageOMAbyP",".",typu, sep=""), device=typu, width=32,height=18)
    
ggplot(data = sumbl[sumbl$type=="f",], aes(x=factor(pn, levels=sort(as.numeric(levels(sumbl$pn)))), y=(oxd/otd))) + labs( x="Problem", y="Percentage out mutex in facts by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"distPercentageOMFbyP",".",typu, sep=""), device=typu, width=32,height=18)

ggplot(data = sumbl[sumbl$type=="a",], aes(x=factor(pn, levels=sort(as.numeric(levels(sumbl$pn)))), y=(ixd/ind))) + labs( x="Problem", y="Percentage in mutex in actions by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"distPercentageIMAbyP",".",typu, sep=""), device=typu, width=32,height=18)

ggplot(data = sumbl[sumbl$type=="f",], aes(x=factor(pn, levels=sort(as.numeric(levels(sumbl$pn)))), y=(ixd/ind))) + labs( x="Problem", y="Percentage in mutex in facts by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"distPercentageIMFbyP",".",typu, sep=""), device=typu, width=32,height=18)

#levels

ggplot(data = sumbl, aes(x=factor(time), y=oxd/otd)) + labs( x="Time Step", y="Percentage out mutex by time step" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom, scales="free")
ggsave(paste(gpath,"distPercentageOMbyTS",".",typu, sep=""), device=typu, width=32,height=18)

#imprimirini(typ=typu,name=paste("distPercentageIMbyT",sep=""))
ggplot(data = sumbl, aes(x=factor(time), y=(ixd/ind))) + labs( x="Time step", y="Percentage in mutex by time step" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"distPercentageIMbyTS",".",typu, sep=""), device=typu, width=32,height=18)

ggplot(data = sumbl[sumbl$type=="a",], aes(x=factor(time), y=(oxd/otd))) + labs( x="Time step", y="Percentage out mutex in actions by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"distPercentageOMAbyTS",".",typu, sep=""), device=typu, width=32,height=18)
    
ggplot(data = sumbl[sumbl$type=="f",], aes(x=factor(time), y=(oxd/otd))) + labs( x="Time step", y="Percentage out mutex in facts by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"distPercentageOMFbyTS",".",typu, sep=""), device=typu, width=32,height=18)

ggplot(data = sumbl[sumbl$type=="a",], aes(x=factor(time), y=(ixd/ind))) + labs( x="Time step", y="Percentage in mutex in actions by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"distPercentageIMAbyTS",".",typu, sep=""), device=typu, width=32,height=18)

ggplot(data = sumbl[sumbl$type=="f",], aes(x=factor(time), y=(ixd/ind))) + labs( x="Time step", y="Percentage in mutex in facts by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"distPercentageIMFbyTS",".",typu, sep=""), device=typu, width=32,height=18)


#}

#que puede venir en una instancia
# dist mutex por capa
# revisar posibles caracteristicas en lugar de por dominio
# por ejemplo tamaño de distribucion, numero de maximos o minimos etc
