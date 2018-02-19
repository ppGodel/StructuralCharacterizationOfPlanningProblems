
library('psych')
library('ggplot2')
library('plyr')
library('corrplot')

typu="png"
typg<<-"eps"
"/" <- function(x,y) {ifelse(y==0,0,base:::"/"(x,y))}
gpath="images/"
imprimirini= function(typ, name, w=32, h=18){
    typg<<-typ
    if(typg=="eps"){
        postscript(paste(gpath,name,".eps", sep=""), width=32,height=18, units = 'in', res= 300)
        #dev.print(file=paste(name,".eps", sep=""), device=eps, width=1440,height=960,units = "px")
    }
    else
    {
        png(paste(gpath,name,".png", sep=""), width=w,height=h, units = 'in', res=300)
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
    fns=c("log","mprime","movie","mystery")
    for(fn in fns){
        datar2 = read.file(f=paste("strips-",fn,"-all.csv", sep=''))
        if(fn=="log"){
            dn = "logistics"
        }else{
         dn = fn
        }
        datar2=cbind(dom=rep(dn,nrow(datar2)),pn=sub(".*x-", "",datar2$gn),datar2)
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
names(compresults)[names(compresults) == 'probname'] <- 'gn'
compresults$solved= ifelse(!is.na(compresults$minsteps) & compresults$minsteps>0,1,0)
#for(fn in fns){
#    dataraw = read.file(f=paste("strips-",fn,"-all.csv", sep=''))
dataraw=joinall()

                                        #by graph
                                        #nodes




counttypednodesbyg=ddply(dataraw, c("gn","dom","type"),  summarise, nodes.count=length(hash), ae.count=sum(otd), xe.count=sum(oxd), xp=100*xe.count/ae.count )
#countnodesbyg=aggregate(hash~gn+dom, dataraw, length)
#counttypednodesbyg=aggregate(hash~gn+dom+type, dataraw, length)
#names(counttypednodesbyg)[names(counttypednodesbyg) == 'hash'] <- 'ncountbyg'
factcountbyg=counttypednodesbyg[counttypednodesbyg$type=="f",]
factcountbyg$type=NULL
names(factcountbyg)[names(factcountbyg) == 'nodes.count'] <- 'gfact.count'
names(factcountbyg)[names(factcountbyg) == 'ae.count'] <- 'gfact.ae.count'
names(factcountbyg)[names(factcountbyg) == 'ae.min'] <- 'gfact.ae.min'
names(factcountbyg)[names(factcountbyg) == 'ae.median'] <- 'gfact.ae.median'
names(factcountbyg)[names(factcountbyg) == 'ae.mean'] <- 'gfact.ae.mean'
names(factcountbyg)[names(factcountbyg) == 'ae.max'] <- 'gfact.ae.max'
names(factcountbyg)[names(factcountbyg) == 'xe.count'] <- 'gfact.xe.count'
names(factcountbyg)[names(factcountbyg) == 'xe.min'] <- 'gfact.xe.min'
names(factcountbyg)[names(factcountbyg) == 'xe.median'] <- 'gfact.xe.median'
names(factcountbyg)[names(factcountbyg) == 'xe.mean'] <- 'gfact.xe.mean'
names(factcountbyg)[names(factcountbyg) == 'xe.max'] <- 'gfact.xe.max'
names(factcountbyg)[names(factcountbyg) == 'xp'] <- 'gfact.xp'
actioncountbyg=counttypednodesbyg[counttypednodesbyg$type=="a",]
actioncountbyg$type=NULL
names(actioncountbyg)[names(actioncountbyg) == 'nodes.count'] <- 'gaction.count'
names(actioncountbyg)[names(actioncountbyg) == 'ae.count'] <- 'gaction.ae.count'
names(actioncountbyg)[names(actioncountbyg) == 'ae.min'] <- 'gaction.ae.min'
names(actioncountbyg)[names(actioncountbyg) == 'ae.median'] <- 'gaction.ae.median'
names(actioncountbyg)[names(actioncountbyg) == 'ae.mean'] <- 'gaction.ae.mean'
names(actioncountbyg)[names(actioncountbyg) == 'ae.max'] <- 'gaction.ae.max'
names(actioncountbyg)[names(actioncountbyg) == 'xe.count'] <- 'gaction.xe.count'
names(actioncountbyg)[names(actioncountbyg) == 'xe.min'] <- 'gaction.xe.min'
names(actioncountbyg)[names(actioncountbyg) == 'xe.median'] <- 'gaction.xe.median'
names(actioncountbyg)[names(actioncountbyg) == 'xe.mean'] <- 'gaction.xe.mean'
names(actioncountbyg)[names(actioncountbyg) == 'xe.max'] <- 'gaction.xe.max'
names(actioncountbyg)[names(actioncountbyg) == 'xp'] <- 'gaction.xp'
                                        #edges

#sumaedgesbyg=aggregate(cbind(ind,ixd,otd,oxd,ad)~gn+dom, dataraw, sum)
sumaedgesbyg=ddply(dataraw, c("gn","dom","pn"),  summarise, levels=max(time), nodes.count=length(hash), ae.count=sum(otd), ae.min=min(otd), ae.median=median(otd), ae.mean=mean(otd), ae.max=max(otd), xe.count=sum(oxd), xe.min=min(oxd), xe.median=median(oxd), xe.mean=mean(oxd), xe.max=max(oxd), xp=100*xe.count/ae.count , density=100*ae.count/(nodes.count*(nodes.count-1)/2))#cbind(ind,ixd,otd,oxd,ad))
#summaryedgesbg=aggregate(cbind(ind,ixd,otd,oxd,ad)~gn+dom, dataraw, summary)

sumatypededgesbyg=aggregate(cbind(ind,ixd,otd,oxd,ad)~gn+dom+type, dataraw, sum)
summarytypededgesbg=aggregate(cbind(ind,ixd,otd,oxd,ad)~gn+dom+type, dataraw, summary)
sumaresultbyg=join_all(dfs=list(compresults,sumaedgesbyg,factcountbyg,actioncountbyg), by=c("gn","dom"),type="full")


                                        #by level
    #nodes
#counttypednodesbyl=aggregate(hash~gn+dom+pn+type+time, dataraw, length)
counttypednodesbyl=ddply(dataraw, c("gn","dom","type","time"),  summarise, nodes.count=length(hash),ae.count=sum(otd), ae.min=min(otd), ae.median=median(otd), ae.mean=mean(otd), ae.max=max(otd), xe.count=sum(oxd), xe.min=min(oxd), xe.median=median(oxd), xe.mean=mean(oxd), xe.max=max(oxd), xp=100*xe.count/ae.count )

factcountbyl=counttypednodesbyl[counttypednodesbyl$type=="f",]
factcountbyl$type=NULL
names(factcountbyl)[names(factcountbyl) == 'nodes.count'] <- 'lfact.count'
names(factcountbyl)[names(factcountbyl) == 'ae.count'] <- 'lfact.ae.count'
names(factcountbyl)[names(factcountbyl) == 'ae.min'] <- 'lfact.ae.min'
names(factcountbyl)[names(factcountbyl) == 'ae.median'] <- 'lfact.ae.median'
names(factcountbyl)[names(factcountbyl) == 'ae.mean'] <- 'lfact.ae.mean'
names(factcountbyl)[names(factcountbyl) == 'ae.max'] <- 'lfact.ae.max'
names(factcountbyl)[names(factcountbyl) == 'xe.count'] <- 'lfact.xe.count'
names(factcountbyl)[names(factcountbyl) == 'xe.min'] <- 'lfact.xe.min'
names(factcountbyl)[names(factcountbyl) == 'xe.median'] <- 'lfact.xe.median'
names(factcountbyl)[names(factcountbyl) == 'xe.mean'] <- 'lfact.xe.mean'
names(factcountbyl)[names(factcountbyl) == 'xe.max'] <- 'lfact.xe.max'
names(factcountbyl)[names(factcountbyl) == 'xp'] <- 'lfact.xp'
actioncountbyl=counttypednodesbyl[counttypednodesbyl$type=="a",]
actioncountbyl$type=NULL
names(actioncountbyl)[names(actioncountbyl) == 'nodes.count'] <- 'laction.count'
names(actioncountbyl)[names(actioncountbyl) == 'ae.count'] <- 'laction.ae.count'
names(actioncountbyl)[names(actioncountbyl) == 'ae.min'] <- 'laction.ae.min'
names(actioncountbyl)[names(actioncountbyl) == 'ae.median'] <- 'laction.ae.median'
names(actioncountbyl)[names(actioncountbyl) == 'ae.mean'] <- 'laction.ae.mean'
names(actioncountbyl)[names(actioncountbyl) == 'ae.max'] <- 'laction.ae.max'
names(actioncountbyl)[names(actioncountbyl) == 'xe.count'] <- 'laction.xe.count'
names(actioncountbyl)[names(actioncountbyl) == 'xe.min'] <- 'laction.xe.min'
names(actioncountbyl)[names(actioncountbyl) == 'xe.median'] <- 'laction.xe.median'
names(actioncountbyl)[names(actioncountbyl) == 'xe.mean'] <- 'laction.xe.mean'
names(actioncountbyl)[names(actioncountbyl) == 'xe.max'] <- 'laction.xe.max'
names(actioncountbyl)[names(actioncountbyl) == 'xp'] <- 'laction.xp'

sumaedgesbyl=ddply(dataraw, c("gn","dom","pn","time"),  summarise, nodes.count=length(hash), ae.count=sum(otd), ae.min=min(otd), ae.median=median(otd), ae.mean=mean(otd), ae.max=max(otd), xe.count=sum(oxd), xe.min=min(oxd), xe.median=median(oxd), xe.mean=mean(oxd), xe.max=max(oxd), xp=100*xe.count/ae.count , leveldensity=100*ae.count/(nodes.count*(nodes.count-1)/2))



nlevelsinfo= sumaedgesbyl[,c("dom","gn","time","nodes.count")]
nlevelsinfo$time=nlevelsinfo$time-1
names(nlevelsinfo)[names(nlevelsinfo) == 'nodes.count'] <- 'nodes.count.nextl'

flevelsinfo= factcountbyl[,c("dom","gn","time","lfact.count")]
flevelsinfo$time=flevelsinfo$time-1
names(flevelsinfo)[names(flevelsinfo) == 'lfact.count'] <- 'fact.count.nextl'
levelsinfo=merge(x=nlevelsinfo, y=flevelsinfo, by=c("dom","gn","time"), all.x=T)

levelsinfo$action.count.nextl= levelsinfo$nodes.count.nextl-levelsinfo$fact.count.nextl

#testlevels=merge(x=fullresults, y=levelsinfo, by=c("dom","gn","time"), all.x=T)

sumaresultbyl=join_all(dfs=list(sumaedgesbyl,factcountbyl,actioncountbyl, levelsinfo), by=c("gn","dom","time"),type="full")

sumaresultbyl$Maxedge.nextl=(sumaresultbyl$nodes.count-1)*sumaresultbyl$nodes.count.nextl
sumaresultbyl$dist.ae.mean.nextl=sumaresultbyl$ae.median/sumaresultbyl$Maxedge.nextl
sumaresultbyl$dist.ae.max.nextl=sumaresultbyl$ae.max/sumaresultbyl$Maxedge.nextl

sumaresultbyl$Maxedge.fact.nextl=(sumaresultbyl$lfact.count-1)*sumaresultbyl$fact.count.nextl
sumaresultbyl$dist.fact.ae.mean.nextl=sumaresultbyl$lfact.ae.mean/sumaresultbyl$Maxedge.fact.nextl
sumaresultbyl$dist.fact.ae.max.nextl=sumaresultbyl$lfact.ae.max/sumaresultbyl$Maxedge.fact.nextl

sumaresultbyl$Maxedge.fact.nextl=(sumaresultbyl$lfact.count-1)*sumaresultbyl$fact.count.nextl
sumaresultbyl$dist.fact.xe.mean.nextl=sumaresultbyl$lfact.xe.mean/sumaresultbyl$Maxedge.fact.nextl
sumaresultbyl$dist.fact.xe.max.nextl=sumaresultbyl$lfact.xe.max/sumaresultbyl$Maxedge.fact.nextl

sumaresultbyl$Maxedge.action.nextl=(sumaresultbyl$laction.count-1)*sumaresultbyl$action.count.nextl
sumaresultbyl$dist.action.ae.mean.nextl=sumaresultbyl$laction.ae.mean/sumaresultbyl$Maxedge.action.nextl
sumaresultbyl$dist.action.ae.max.nextl=sumaresultbyl$laction.ae.max/sumaresultbyl$Maxedge.action.nextl

sumaresultbyl$Maxedge.action.nextl=(sumaresultbyl$laction.count-1)*sumaresultbyl$action.count.nextl
sumaresultbyl$dist.action.xe.mean.nextl=sumaresultbyl$laction.xe.mean/sumaresultbyl$Maxedge.action.nextl
sumaresultbyl$dist.action.xe.max.nextl=sumaresultbyl$laction.xe.max/sumaresultbyl$Maxedge.action.nextl

fullresults=merge(x=sumaresultbyg, y=sumaresultbyl, by=c("gn","dom"))
#compresults=join_all(dfs=list(compresultsbase,compresultstan,compresultsbb,compresultsipp,compresultshsp), by=c("probname","comp","dom"),type="full")



fn="correlationbyg"
ct=sumaresultbyg[c("solved","fa","minsteps", "mintime", "levels","xp","ae.median","ae.count","xe.median","xe.count","gfact.count","gfact.ae.count","gfact.xe.count","gaction.count","gaction.ae.count","gaction.xe.count")]
imprimirini(typ=typu,name=paste(fn,sep=""),12,7.25)
corrplot(cor(ct, use = "complete.obs"))
imprimirfin()

fn="correlationbyl"
ct=sumaresultbyl[, c("time","xp","nodes.count","ae.median","ae.max","ae.count","xe.median","xe.count","lfact.ae.median","lfact.ae.count","lfact.xe.median","lfact.xe.count","laction.ae.median","laction.ae.count","laction.xe.median","laction.xe.count", "nodes.count.nextl","dist.ae.mean.nextl","dist.ae.max.nextl","dist.fact.ae.mean.nextl","dist.fact.ae.max.nextl","dist.action.ae.mean.nextl","dist.action.ae.max.nextl")]
imprimirini(typ=typu,name=paste(fn,sep=""),12,7.25)
corrplot(cor(ct, use = "complete.obs"))
imprimirfin()

fn="correlationallbyPlan"
ct=fullresults[,c("fa","time","nodes.count.x","nodes.count.x","xp.x","ae.median.x","ae.count.x","xe.median.x","xe.count.x","lfact.ae.median","lfact.ae.count","lfact.xe.median","lfact.xe.count","laction.ae.median","laction.ae.count","laction.xe.median","laction.xe.count", "density","stansteps","stantime","bbsteps","bbtime","ippsteps","ipptime","hspsteps","hsptime")]



imprimirini(typ=typu,name=paste(fn,sep=""),12,7.25)
corrplot(cor(ct, use = "complete.obs"))
imprimirfin()

res.man=manova(cbind(ipptime,ippsteps,bbtime,bbsteps)~ae.count.x*laction.ae.count*laction.ae.median*laction.xe.count*laction.xe.median ,data=ct)
summary.aov(res.man)


fn="correlationall"
ct=fullresults[, c("ts","fa","time","nodes.count.x","nodes.count.x","xp.x","ae.median.x","ae.count.x","xe.median.x","xe.count.x","lfact.ae.median","lfact.ae.count","lfact.xe.median","lfact.xe.count","laction.ae.median","laction.ae.count","laction.xe.median","laction.xe.count")]
imprimirini(typ=typu,name=paste(fn,sep=""),12,7.25)
corrplot(cor(ct, use = "complete.obs"))
imprimirfin()


fn="exclperactionratio-time"
title="Num Total Actions vs time"
xt="Percentage exclusive edges per action node"
yt="time in seconds and scaled in ln"
pchs=c(16,13)
imprimirini(typ=typu,name=paste(fn,sep=""),12,7.25)
plot(log(mintime)~gaction.xp, data=sumaresultbyg, pch=pchs[as.numeric(factor(!is.na(bbtime)))], col=dom, main=title, xlab=xt, ylab=yt)
legend(35,10, legend=c("not solved","solved"),pch=pchs )
legend(35,8, legend=unique(sumaresultbyg$dom),col=1:length(sumaresultbyg$dom),pch=19)
#abline(h=3.76)
imprimirfin()


fn="exclperactionratio-totalactionnodes"
title="Num Total Actions vs Exclusive ratio"
xt="Number of actions in log 10"
yt="Percentage exclusive edges per action node"
pchs=c(16,13)
imprimirini(typ=typu,name=paste(fn,sep=""),12,7.25)
plot(gaction.xp~log(gaction.count,10), data=sumaresultbyg, pch=pchs[as.numeric(factor(!is.na(bbtime)))], col=dom, main=title, xlab=xt, ylab=yt )
legend(4.7,75, legend=c("not solved","solved"),pch=pchs )
legend(4.7,50, legend=unique(sumaresultbyg$dom),col=1:length(sumaresultbyg$dom),pch=19)
abline(v=3.76)
imprimirfin()


fn="exclperfactratio-totalfactnodes"
title="Num Total facts vs Exclusive ratio"
xt="Number of facts in log 10"
yt="Percentage exclusive edges per facts node"
pchs=c(16,13)
imprimirini(typ=typu,name=paste(fn,sep=""),12,7.25)
plot(gfact.xp~log(gfact.count,10), data=sumaresultbyg, pch=pchs[as.numeric(factor(!is.na(bbtime)))], col=dom, main=title, xlab=xt, ylab=yt)
legend(4.2,50, legend=c("not solved","solved"),pch=pchs )
legend(4.2,30, legend=unique(sumaresultbyg$dom),col=1:length(sumaresultbyg$dom),pch=19)
abline(v=3.35)
imprimirfin()


fn="mintime-totalnodes"
title="min time vs Total nodes"
xt="Number of nodes in log 10"
yt="Min time"
pchs=c(16,13)
imprimirini(typ=typu,name=paste(fn,sep=""),12,7.25)
plot(log(mintime,10)~log(nodes.count,10), data=sumaresultbyg, pch=pchs[as.numeric(factor(!is.na(bbtime)))], col=dom, main=title, xlab=xt, ylab=yt )
legend(4.6,5000, legend=c("not solved","solved"),pch=pchs)
legend(4.6,15000, legend=unique(sumaresultbyg$dom),col=1:length(sumaresultbyg$dom),pch=19)
abline(v=3.9)
imprimirfin()


fn="minsteps-totalnodes"
title="min steps vs Total nodes"
xt="Number of nodes in log 10"
yt="Min steps"
pchs=c(16,13)
imprimirini(typ=typu,name=paste(fn,sep=""),12,7.25)
plot(minsteps~log(nodes.count,10), data=sumaresultbyg, pch=pchs[as.numeric(factor(!is.na(bbtime)))], col=dom, main=title, xlab=xt, ylab=yt )
legend(4.9,50, legend=c("not solved","solved"),pch=pchs )
legend(4.9,100, legend=unique(sumaresultbyg$dom),col=1:length(sumaresultbyg$dom),pch=19)
abline(v=3.9)
imprimirfin()


fn="FirstAppearance-totalnodes"
title="First Appearance vs Total nodes"
xt="Number of nodes in log 10"
yt="first appearance"
pchs=c(16,13)
imprimirini(typ=typu,name=paste(fn,sep=""),12,7.25)
plot(fa~log(nodes.count,10), data=sumaresultbyg, pch=pchs[as.numeric(factor(!is.na(bbtime)))], col=dom, main=title, xlab=xt, ylab=yt )
legend(5,6, legend=c("not solved","solved"),pch=pchs )
legend(5,3, legend=unique(sumaresultbyg$dom),col=1:length(sumaresultbyg$dom),pch=19)
abline(v=3.9)
imprimirfin()

fn="FirstAppearanceGap-time"
title="First Appearance Gap vs Time"
xt="Time in log 10"
yt="first appearance gap"
pchs=c(16,13)
imprimirini(typ=typu,name=paste(fn,sep=""),12,7.25)
plot(minsteps-fa~log(mintime,10), data=sumaresultbyg, pch=pchs[as.numeric(factor(!is.na(bbtime)))], col=dom, main=title, xlab=xt, ylab=yt )
legend(5,50, legend=c("not solved","solved"),pch=pchs )
legend(5,100, legend=unique(sumaresultbyg$dom),col=1:length(sumaresultbyg$dom),pch=19)
abline(v=3.35)
abline(h=19)
imprimirfin()

fn="FirstAppearanceGap-totalnodes"
title="First Appearance Gap vs Total nodes"
xt="Time in log 10"
yt="first appearance gap"
pchs=c(16,13)
imprimirini(typ=typu,name=paste(fn,sep=""),12,7.25)
plot(minsteps-fa~log(nodes.count,10), data=sumaresultbyg, pch=pchs[as.numeric(factor(!is.na(bbtime)))], col=dom, main=title, xlab=xt, ylab=yt )
legend(5,50, legend=c("not solved","solved"),pch=pchs )
legend(5,100, legend=unique(sumaresultbyg$dom),col=1:length(sumaresultbyg$dom),pch=19)
abline(v=3.9)
abline(h=19)
imprimirfin()


fn="Factratio-Solved"
title="Fact ratio vs Solved"
xt="Solved problem"
yt="fact ratio"
ggplot(data = sumaresultbyg, aes(x=factor(!is.na(bbtime)), y=gfact.xp)) + labs( x=xt, y=yt, title=title ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom)#,scales="free")
ggsave(paste(gpath,fn,".",typu, sep=""), device=typu, width=12,height=7.25)

fn="Action ratio-Solved"
title="Action ratio vs Solved"
xt="Solved problem"
yt="Action ratio nodes"
ggplot(data = sumaresultbyg, aes(x=factor(!is.na(bbtime)), y=gaction.xp)) + labs( x=xt, y=yt, title=title ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom)#,scales="free")
ggsave(paste(gpath,fn,".",typu, sep=""), device=typu, width=12,height=7.25)


fn="Solved-factnodesratioperlevel"
title="fact nodes ratio per level vs solved"
xt="Solved problem"
yt="fact nodes ratio per level"
ggplot(data = fullresults, aes(x=factor(!is.na(bbtime)), y=(100*lfact.count/nodes.count.y))) + labs( x=xt, y=yt, title=title ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom)#,scales="free")
ggsave(paste(gpath,fn,".",typu, sep=""), device=typu, width=12,height=7.25)


fn="Solved-factnodesratioperlevel-sized"
title="fact nodes ratio per level vs solved"
xt="Solved problem"
yt="fact nodes ratio per level"
ggplot(data = fullresults, aes(x=factor(!is.na(bbtime)), y=(100*lfact.count/nodes.count.y))) + labs( x=xt, y=yt, title=title ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~round(log(lfact.count,10),0))#,scales="free")
ggsave(paste(gpath,fn,".",typu, sep=""), device=typu, width=12,height=7.25)

fn="Solved-actionnodesratioperlevel-sized"
title="Action nodes ratio per level vs solved"
xt="Solved problem"
yt="Action nodes ratio per level"
ggplot(data = fullresults, aes(x=factor(!is.na(bbtime)), y=(100*laction.count/nodes.count.y))) + labs( x=xt, y=yt, title=title ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~round(log(lfact.count,10),0))#,scales="free")
ggsave(paste(gpath,fn,".",typu, sep=""), device=typu, width=12,height=7.25)

fn="Solved-actionnodesratioperlevel"
title="action nodes ratio per level vs solved"
xt="Solved problem"
yt="action nodes ratio per level"
ggplot(data = fullresults, aes(x=factor(!is.na(bbtime)), y=(100*laction.count/nodes.count.y))) + labs( x=xt, y=yt, title=title ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom)#,scales="free")
ggsave(paste(gpath,fn,".",typu, sep=""), device=typu, width=32,height=18)


    #nodes
imprimirini(typ=typu,name=paste("factcountByGraph",sep=""))
boxplot(log(gfact.count,10)~dom,data=sumaresultbyg)
imprimirfin()
    
imprimirini(typ=typu,name=paste("factcountByLevel",sep=""))
boxplot(lfact.count~time+dom,data=sumaresultbyl)
imprimirfin()

imprimirini(typ=typu,name=paste("actioncountByGraph",sep=""))
boxplot(log(gaction.count,10)~dom,data=sumaresultbyg)
imprimirfin()
    
imprimirini(typ=typu,name=paste("actioncountByLevel",sep=""))
boxplot(laction.count~time+dom,data=sumaresultbyl)
imprimirfin()


#
ggplot(data = sumaresultbyl, aes(x=factor(pn, levels=sort(as.numeric(levels(pn)))), y=log(nodes.count,10))) + labs( x="Problem", y="Nodes amount" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom)
ggsave(paste(gpath,"nodescountByProbViol",".",typu, sep=""), device=typu, width=12,height=7.25)

#

ggplot(data = sumaresultbyl, aes(x=factor(pn, levels=sort(as.numeric(levels(pn)))), y=log(lfact.count,10))) + labs( x="Problem", y="Fact nodes amount in log 10" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom)
ggsave(paste(gpath,"factcountByProbViol",".",typu, sep=""), device=typu, width=12,height=7.25)
    
ggplot(data = sumaresultbyl, aes(x=factor(pn, levels=sort(as.numeric(levels(pn)))), y=log(laction.count,10))) + labs( x="Problem", y="Action nodes amount in log 10" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom)
ggsave(paste(gpath,"actioncountByProbVio",".",typu, sep=""), device=typu, width=32,height=18)

ggplot(data = sumaresultbyl, aes(x=factor(time), y=log(laction.count,10))) + labs( x="Problem", y="Action nodes amount in log 10" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom, scales="free_x")
ggsave(paste(gpath,"actioncountBytimeVio",".",typu, sep=""), device=typu, width=32,height=18)


ggplot(data = sumaresultbyl, aes(x=factor(time), y=log(lfact.count,10))) + labs( x="Problem", y="Fact nodes amount in log 10" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom, scales="free_x")
ggsave(paste(gpath,"factcountBytimeVio",".",typu, sep=""), device=typu, width=32,height=18)



ggplot(data = sumaresultbyl, aes(x=factor(time), y=log(laction.xe.mean))) + labs( x="Problem", y="mean mutex edges in action nodes in log 10" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom, scales="free_x")
ggsave(paste(gpath,"actionMeanMutexBytimeVio",".",typu, sep=""), device=typu, width=32,height=18)

ggplot(data = sumaresultbyl, aes(x=factor(time), y=(lfact.xe.mean))) + labs( x="Problem", y="mean mutex edges in action nodes in log 10" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom, scales="free_x")
ggsave(paste(gpath,"factMeanMutexBytimeVio",".",typu, sep=""), device=typu, width=32,height=18)

ggplot(data = sumaresultbyl, aes(x=factor(time), y=log(xe.mean,10))) + labs( x="Problem", y="mean mutex edges in all nodes in log 10" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom, scales="free_x")
ggsave(paste(gpath,"allnodesMeanMutexBytimeVio",".",typu, sep=""), device=typu, width=32,height=18)


ggplot(data = sumaresultbyl, aes(x=factor(time), y=xp)) + labs( x="Problem", y="mean mutex edges in all nodes in log 10" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom, scales="free_x")
ggsave(paste(gpath,"allnodesMutexRatopBytimeVio",".",typu, sep=""), device=typu, width=32,height=18)

ggplot(data = sumaresultbyl, aes(x=factor(pn, levels=sort(as.numeric(levels(pn)))), y=xp)) + labs( x="Problem", y="Percentage out mutex by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free_x")
ggsave(paste(gpath,"distPercentMutexbyP",".",typu, sep=""), device=typu, width=32,height=18)

ggplot(data = sumaresultbyl, aes(x=factor(time), y=laction.xp)) + labs( x="Problem", y="mean mutex edges in all nodes in log 10" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom, scales="free_x")
ggsave(paste(gpath,"factnodesMutexRatopBytimeVio",".",typu, sep=""), device=typu, width=32,height=18)

ggplot(data = sumaresultbyl, aes(x=factor(time), y=lfact.xp)) + labs( x="Problem", y="mean mutex edges in all nodes in log 10" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom, scales="free_x")
ggsave(paste(gpath,"actionnodesMutexRatopBytimeVio",".",typu, sep=""), device=typu, width=32,height=18)




ggplot(data = sumaresultbyl, aes(x=factor(pn, levels=sort(as.numeric(levels(pn)))), y=(laction.xp))) + labs( x="Problem", y="Percentage out mutex in actions by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free_x")
ggsave(paste(gpath,"distPercentageOMAbyP",".",typu, sep=""), device=typu, width=32,height=18)
    
ggplot(data = sumaresultbyl, aes(x=factor(pn, levels=sort(as.numeric(levels(pn)))), y=lfact.xp)) + labs( x="Problem", y="Percentage out mutex in facts by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"distPercentageOMFbyP",".",typu, sep=""), device=typu, width=32,height=18)

#levels
ggplot(data = sumaresultbyl, aes(x=factor(time), y=xp)) + labs( x="Time Step", y="Percentage out mutex by time step" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom, scales="free")
ggsave(paste(gpath,"distPercentageOMbyTS",".",typu, sep=""), device=typu, width=32,height=18)

ggplot(data = sumaresultbyl, aes(x=factor(time), y=laction.xp)) + labs( x="Time step", y="Percentage out mutex in actions by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"distPercentageOMAbyTS",".",typu, sep=""), device=typu, width=32,height=18)
    
ggplot(data = sumaresultbyl, aes(x=factor(time), y=lfact.xp)) + labs( x="Time step", y="Percentage out mutex in facts by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"distPercentageOMFbyTS",".",typu, sep=""), device=typu, width=32,height=18)


ggplot(data = sumaresultbyl, aes(x=factor(time), y=lfact.xp)) + labs( x="Time step", y="Percentage out mutex in facts by level" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))+facet_wrap(~dom,scales="free")
ggsave(paste(gpath,"distPercentageOMFbyTS",".",typu, sep=""), device=typu, width=32,height=18)


#}

#que puede venir en una instancia
# dist mutex por capa
# revisar posibles caracteristicas en lugar de por dominio
# por ejemplo tamaño de distribucion, numero de maximos o minimos etc
