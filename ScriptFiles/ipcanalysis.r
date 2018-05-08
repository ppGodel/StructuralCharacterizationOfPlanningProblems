source("rfunctions.r")
if(FALSE){
    nx="D"
    ny="Time"
    data=compresultsgraphsolved[compresultsgraphsolved$parallel==1,]
    prnt=FALSE    
}
plotInstancesDifficulty <- function(bdf,fn){
    td2=aggregate(planner~Class+Cfactor, bdf, FUN=length)
    td2$Diff=""
    td2[td2$Class==0,]$Diff="2 Average"
    td2[td2$Class==1,]$Diff="1 Easy"
    td2[td2$Class==2,]$Diff="3 Hard"
    ggplot(data=td2, aes(x=(td2$Cfactor), y=planner, fill=as.factor(Diff)))+geom_bar(stat="identity", position = "dodge")+labs(x = "Property", y="Count", fill="Difficulty")+ scale_fill_manual(values=c("skyblue", "orange1", "red"))
    ggsave(paste0(gpath,fn,".",typu), device=typu, width=12,height=7.25)
}
createDataSetbyDomWithClassification <- function(basedataframe, filename, prin, prefn){
    if(!file.exists(filename)){
        c1=allplannersbydom(nx="D" ,ny="Time", data=basedataframe,prnt=prin, prefn=prefn)
        c2=allplannersbydom(nx="DM",ny="Time", data=basedataframe,prnt=prin, prefn=prefn)
        c3=allplannersbydom(nx="TN",ny="Time", data=basedataframe,prnt=prin, prefn=prefn)
        c4=allplannersbydom(nx="TE",ny="Time", data=basedataframe,prnt=prin, prefn=prefn)
        c5=allplannersbydom(nx="TME",ny="Time", data=basedataframe,prnt=prin, prefn=prefn)
        rdf=rbind(c1,c2,c3,c4,c5)
        write.csv(rdf, filename)
    }else{
        rdf = read.csv(filename)
    }
    return(rdf)
}

createDataSetbyComWithClassification <- function(basedataframe, filename, prin, prefn){
    if(!file.exists(filename)){
        c1=allplanners(nx="D" ,ny="Time", data=basedataframe,prnt=prin, prefn=prefn)
        c2=allplanners(nx="DM",ny="Time", data=basedataframe,prnt=prin, prefn=prefn)
        c3=allplanners(nx="TN",ny="Time", data=basedataframe,prnt=prin, prefn=prefn)
        c4=allplanners(nx="TE",ny="Time", data=basedataframe,prnt=prin, prefn=prefn)
        c5=allplanners(nx="TME",ny="Time", data=basedataframe,prnt=prin, prefn=prefn)
        rdf=rbind(c1,c2,c3,c4,c5)
        write.csv(rdf, filename)
    }else{
        rdf = read.csv(filename)
    }
    return(rdf)
}

grcon= mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="graphIPCResComp")
compresultsraw=data.frame(grcon$find())
exeres= read.csv("executionresults.csv")
exeres$pkey=paste(exeres$com,"-",exeres$dom,'-', exeres$gn, sep='')
compresultexec=merge(compresultsraw, exeres[,c("pkey","fap","pl","gl")], by="pkey", all.x=TRUE)
compresultexec=compresultexec[!duplicated(compresultexec),]
compresultexec$gcomp=ifelse(compresultexec$pl>0,1,0)
compresultexec$parallel=ifelse(compresultexec$pl>compresultexec$gl,1,0)
compresultsgraphsolved=compresultexec[compresultexec$solved==1&compresultexec$graph==1&compresultexec$gcomp==1,]
compresultsgraphsolved$LogTime=log(compresultsgraphsolved$Time+1)
compresultsgraphsolved$Class=0
compresultsgraphsolved$R2=0
compresultsgraphsolved$Dist=0
compresultsgraphsolved$MahaDist=0
compresultsgraphsolved$CookDist=0
compresultsgraphsolved$Disc=0
compresultsgraphsolved$MahaOut=FALSE
compresultsgraphsolved$CookOut=FALSE

typu="eps"
prin=TRUE
allclasscompar=createDataSetbyComWithClassification(compresultsgraphsolved[compresultsgraphsolved$parallel==1,],"allclassifications-parallelbycom.csv",prin,"ParByCom")
allclasscomnpar=createDataSetbyComWithClassification(compresultsgraphsolved[compresultsgraphsolved$parallel==0,],"allclassifications-noparallelbycom.csv",prin,"NoParByCom")

allclassdompar=createDataSetbyDomWithClassification(compresultsgraphsolved[compresultsgraphsolved$parallel==1,],"allclassifications-parallelbydom.csv",prin,"ParByDom" )
allclassdomnpar=createDataSetbyDomWithClassification(compresultsgraphsolved[compresultsgraphsolved$parallel==0,],"allclassifications-noparallelbydom.csv",prin,"NoParByDom" )
if(FALSE){

    sx="TN"
    tdf=compresultsgraphsolved[compresultsgraphsolved$parallel==1&compresultsgraphsolved$com=="IPC1998"&compresultsgraphsolved$planner=="stan",]
    bv=choose.lm(px=tdf[,sx],py=tdf$Time)
    trx=tukeyLadder(tdf[,sx],bv$tx)
    try=tukeyLadder(tdf$Time,bv$ty)
    #mod=lm(try~trx)
    #cookds = cooks.distance(mod)
                                        #mdf=cbind(trx,try)
    
    outmaha=sqrt(qchisq(0.995,2))
    #plot(m_dist, pch="*", cex=2, main="Influential Obs by mahalanobis distance")
    #abline(h = outmaha, col="red")
    #text(x=1:length(m_dist)+1, y=m_dist, labels=ifelse(m_dist> outmaha,tdf$gn,""), col="red", xpd=TRUE)
    
    #plot(cookds, pch="*", cex=2, main="Influential Obs by Cooks distance")
    #abline(h = 4*mean(cookds, na.rm=T), col="red")
    #text(x=1:length(cookds)+1, y=cookds, labels=ifelse(cookds>4*mean(cookds, na.rm=T),tdf$gn,""), col="red", xpd=TRUE)

    
    
    ppx=trx
    ppy=try
    plot(x=ppx, y=ppy)
    rmod=lm(ppy~ppx)
    rcookds = cooks.distance(rmod)
    mdf=cbind(try,trx)
    cvmdf=cov(mdf)
    tryCatch({ m_dist <- sqrt(mahalanobis(mdf, colMeans(mdf), cvmdf))},error=function(erm){ m_dist=trx*0 })  
    abline(rmod)
    text(x=ppx, y=ppy, labels=ifelse(m_dist<outmaha&rcookds>4*mean(rcookds, na.rm=T),tdf$gn,""), col="red", xpd=TRUE)
    text(x=ppx, y=ppy, labels=ifelse(m_dist>outmaha&rcookds<4*mean(rcookds, na.rm=T),tdf$gn,""), col="blue", xpd=TRUE)
    text(x=ppx, y=ppy, labels=ifelse(m_dist>outmaha&rcookds>4*mean(rcookds, na.rm=T),tdf$gn,""), col="green", xpd=TRUE)

    ppx=tdf[,sx]
    ppy=tdf$Time
    plot(x=ppx, y=ppy)
    rmod=lm(ppy~ppx)
    rcookds = cooks.distance(rmod)
    mdf=cbind(try,trx)
    cvmdf=cov(mdf)
    tryCatch({ m_dist <- sqrt(mahalanobis(mdf, colMeans(mdf), cvmdf))},error=function(erm){ m_dist=trx*0 })    
    abline(rmod)
    text(x=ppx, y=ppy, labels=ifelse(m_dist<outmaha&rcookds>4*mean(rcookds, na.rm=T),tdf$gn,""), col="red", xpd=TRUE)
    text(x=ppx, y=ppy, labels=ifelse(m_dist>outmaha&rcookds<4*mean(rcookds, na.rm=T),tdf$gn,""), col="blue", xpd=TRUE)
    text(x=ppx, y=ppy, labels=ifelse(m_dist>outmaha&rcookds>4*mean(rcookds, na.rm=T),tdf$gn,""), col="green", xpd=TRUE)
    
    


}

boxplot(R2~Cresp+Cfactor, data=allclasscompar)
boxplot(R2~Cresp+Cfactor, data=allclasscomnpar)
boxplot(R2~Cresp+Cfactor, data=allclassdompar)
boxplot(R2~Cresp+Cfactor, data=allclassdomnpar)
#ggplot(data = allclass[allclass$R2>=0.85&allclass$R2<0.985,], aes(x=factor(Cfactor), y=log(abs(Dist)+1))) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))#+facet_wrap(~dom)
#ggsave(paste0(gpath,"Layers/","DistanceDistribution.",typu), device=typu, width=12,height=7.25)

sqv=seq(0.8,1,0.01)
tda=aggregate(planner~R2+Cfactor+com, allclassdompar, FUN=length)
tda=cbind(1:dim(tda)[1],tda[order(tda$R2, decreasing=TRUE),])
higm=sapply(X=sqv, FUN=function(item){    
    #return(100*sum(tda$R2>=item)/length(tda$R2))
    return(sum(tda$R2>=item))
})
#logm=100-higm
logm=length(tda$R2)-higm
td=data.frame(x=sqv,value="accepted", count=higm)
tp=data.frame(x=sqv,value="rejected", count=logm)
td=rbind(tp,td)
ggplot(data=td, aes(x=x, y=count, fill=value) )+geom_bar(stat="identity")+theme(axis.text=element_text(size=20), axis.title=element_text(size=25,  face="bold"))+labs(x = expression(R^2), y="Model Count", fill="Value")+scale_x_continuous(breaks=seq(0.8, 1, 0.01))+ theme_bw(base_size=20)
ggsave(paste0(gpath,"Layers/","ParallelR2Dist.",typu), device=typu, width=12,height=7.25)


#td2=aggregate(planner~Class+Cfactor, allclass[allclass$R2>=0.85&allclass$R2<=0.985,], FUN=length)
#diff comparison



plotinstancesDifficult(allclassdompar[between(allclassdompar$R2,0.85,0.98),], "Layers/InstanceDifficultyPar")
plotinstancesDifficult(allclassdomnpar[between(allclassdomnpar$R2,0.85,0.98),], "Layers/InstanceDifficultyNPar")

bdf=allclasscompar[between(allclasscompar$R2,0.85,0.98),]
mddf=aggregate(abs(Dist)~com+dom+planner,bdf,FUN=max)
names(mddf)= c("com","dom","planner","maxDist")
bdf= merge(bdf,mddf, by=c("com","dom","planner"))

td3=droplevels(aggregate(planner~Disc+MahaOut+CookOut+Dist+maxDist+MahaDist+CookDist+Class+dom+gn, bdf[bdf$Class!=0,], FUN=length))
td3[td3$Disc==2,]$Disc=2
td3[td3$Disc==0,]$Disc=1
td3$Diff=NA
td3$MahaDiff="Average"
td3$CookDiff="Average"
td3[td3$Class==0,]$Diff="Average"
td3[td3$Class==1,]$Diff="Easy"
td3[td3$Class==2,]$Diff="Hard"
td3[td3$Disc==1 & td3$MahaOut==1,]$MahaDiff="Easy"
td3[td3$Disc==2 & td3$MahaOut==1,]$MahaDiff="Hard"
td3[td3$Disc==1 & td3$CookOut==1,]$CookDiff="Easy"
td3[td3$Disc==2 & td3$CookOut==1,]$CookDiff="Hard"

td3ND=aggregate(planner~dom+gn+Diff, td3,FUN=sum)
td3MD=aggregate(planner~dom+gn+MahaDiff, td3,FUN=sum)
td3CD=aggregate(planner~dom+gn+CookDiff, td3,FUN=sum)

td3ND[td3ND$Diff=="Easy",]$planner=-td3ND[td3ND$Diff=="Easy",]$planner
td3CD[td3CD$CookDiff=="Easy",]$planner=-td3CD[td3CD$CookDiff=="Easy",]$planner
td3MD[td3MD$MahaDiff=="Easy",]$planner=-td3MD[td3MD$MahaDiff=="Easy",]$planner
names(td3MD)[names(td3MD) == 'MahaDiff'] <- 'Diff'
names(td3CD)[names(td3CD) == 'CookDiff'] <- 'Diff'

pltdf= droplevels(td3ND[td3ND$Diff!="Average",])
ggplot(data =pltdf, aes(x=factor(gn), y=planner, fill=as.factor(Diff) )) +geom_bar(stat="identity", position = position_stack(reverse = TRUE) ) + theme(text = element_text(size=10)) + labs(x="Instance", y="vote count", fill="Difficulty") + coord_flip() + scale_fill_manual(values=c("skyblue", "orange1", "red")) + scale_x_discrete( limits=rev(levels(pltdf$gn)))
ggsave(paste0(gpath,"Layers/","ClassByNormDistParallelbyComR2Int.",typu), device=typu, width=12,height=7.25)

pltdf= droplevels(td3CD[td3CD$Diff!="Average",])
ggplot(data =pltdf, aes(x=factor(gn), y=planner, fill=as.factor(Diff) )) +geom_bar(stat="identity", position = position_stack(reverse = TRUE) ) + theme(text = element_text(size=10)) + labs(x="Instance", y="vote count", fill="Difficulty") + coord_flip() + scale_fill_manual(values=c("skyblue", "orange1", "red")) + scale_x_discrete( limits=rev(levels(pltdf$gn)))
ggsave(paste0(gpath,"Layers/","ClassByCookDistParallelbyComR2Int.",typu), device=typu, width=12,height=7.25)

pltdf= droplevels(td3MD[td3MD$Diff!="Average",])
ggplot(data =pltdf, aes(x=factor(gn), y=planner, fill=as.factor(Diff) )) +geom_bar(stat="identity", position = position_stack(reverse = TRUE) ) + theme(text = element_text(size=10)) + labs(x="Instance", y="vote count", fill="Difficulty") + coord_flip() + scale_fill_manual(values=c("skyblue", "orange1", "red")) + scale_x_discrete( limits=rev(levels(pltdf$gn)))
ggsave(paste0(gpath,"Layers/","ClassByMahaDistParallelbyComR2Int.",typu), device=typu, width=12,height=7.25)


met=unique(allclass$Cfactor)
for(m in met){
    dtp=log(abs(td3[td3$Cfactor==m&td3$Class>0,]$Dist+1))#
    #mdtp=1 / max(dtp)
    #dtp=dtp*mdtp
    imprimirini(typ=typu,name=paste0("Layers/","HistDistanceR2",lr2,m),12,7.25)
    hist(dtp, xlab="Normalized Distance", main=paste("Histogram of Distance for R2 > ", lr2, " and",m))
    imprimirfin()
    uc=c(1,2)
    for(c in uc){
        dtp=log(abs(td3[td3$Cfactor==m&td3$Class==c,]$Dist+1))#
        mdtp=1 / max(dtp)
        dtp=dtp*mdtp
        imprimirini(typ=typu,name=paste0("Layers/","HistDistanceR2",lr2,m,"C",c),12,7.25)
        hist(dtp, xlab="Normalized Distance", main=paste("Histogram of Distance for R2 > ", lr2, " and",m, "Class", c))
        imprimirfin()
    }
}

#aggregate(gn~planner+com+dom, allclass, FUN=length)
#md=max(abs(allclass$Dist))
bdf=allclassdompar[between(allclassdompar$R2,0.85,0.98),]
td4=ddply(.data=bdf, c("com","dom","gn","Cfactor","Class"),  summarise, Class.count=length(planner), Class.sum=sum(Class), Dist.sum=sum(abs(Dist)),Dist.max=max(abs(Dist)) )

td4$Vote=0
td4[td4$Class==0,]$Vote=1
td4[td4$Class==1,]$Vote=0
td4[td4$Class==2,]$Vote=2
td5=ddply(td4, c("com","dom","gn","Cfactor"),  summarise, Class.MaxVote=sum(Class.count), Class.Vote=sum(Class.count*Vote), Class.Score=sum(Class.count*Vote)/(2*sum(Class.count)), Dist.sum=sum(ifelse(Dist.max==0,1,Dist.sum/Dist.max))  )
plot(td5$Class.Score~td5$Dist.sum)



bdf=allclassdompar[between(allclassdompar$R2,0.85,0.98),]

td4=ddply(.data=bdf, c("com","dom","gn","Cfactor","Disc"),  summarise, Class.count=length(planner), Class.sum=sum(Disc*MahaOut), Dist.sum=sum(abs(MahaDist)),Dist.max=max(abs(MahaDist)) )

td5=ddply(td4, c("com","dom","gn","Cfactor"),  summarise, Class.MaxVote=2*sum(Class.count), Class.Vote=sum(Class.sum), Class.Score=sum(Class.sum)/(2*sum(Class.count)), Dist.sum=sum(Dist.sum)  )


plot(td5$Class.Score~td5$Dist.sum)


bdf=allclassdompar[between(allclassdompar$R2,0.85,0.98),]

scom='IPC2000'
#my_data= compresultsraw[compresultsraw$com==scom ,c("Time","Steps","MT","TE","TN","D","DM","TME")]
my_data= compresultsraw[compresultsraw$com==scom ,c("Time","TE","TN","D","DM","TME")]
#colnames(my_data) = c("Time","Steps","Graph \nLevels","Edges","Nodes","Density","Mutex \nDensity"," Mutex \nEdges")
colnames(my_data) = c("Time","Edges","Nodes","Density","Mutex \nDensity"," Mutex \nEdges")
typu="eps"
#cex.before <- par("cex")
#par(cex = 3)
imprimirini(typ=typu,name=paste0("Layers/","CorrKendall",scom),12,7.25)
corrplot.mixed(cor(my_data,  method = "kendall", use="complete.obs"), p.mat=cor.mtest(my_data,  method = "kendall", use="complete.obs")$p, na.label="NA", sig.level=0.005, insig="pch",cl.cex=1.5, tl.cex=1.5, number.cex=1.5)
imprimirfin()
imprimirini(typ=typu,name=paste0("Layers/","CorrSpearman",scom),12,7.25)
corrplot.mixed(cor(my_data,  method = "spearman", use="complete.obs"), p.mat=cor.mtest(my_data,  method = "spearman", use="complete.obs")$p, na.label="NA", sig.level=0.005, insig="pch",cl.cex=1.5, tl.cex=1.5, number.cex=1.5)
imprimirfin()
#par(cex = cex.before)
#ggscatter(my_data, x = "D", y = "Time", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "kendall")


#write.csv(allclass,'allclass.csv')
plannerclass= read.csv('planners.csv',sep=",")
plannerclass$gpbase=ifelse(plannerclass$gpbase==1,'graphplan','no-graphplan')
plannerclass$heuristicbase=ifelse(plannerclass$heuristicbase==1,'heuristic','no-heuristic')
plannerclass$satbase=ifelse(plannerclass$satbase==1,'sat','no-sat')
plannerclass$binarydecisionbase=ifelse(plannerclass$binarydecisionbase==1,'binary-descision-tree','no-binary-descision-tree')
plannerclass$regresionbase=ifelse(plannerclass$regresionbase==1,'regresion','no-regresion')
plannerclass$rewritingbase=ifelse(plannerclass$rewritingbase==1,'rewriting','no-rewriting')
plannerclass$hobase=ifelse(plannerclass$hobase==1,'hierarchical-ordered','no-hierarchical-ordered')
plannerclass$gtbase=ifelse(plannerclass$gtbase==1,'game-theory','no-game-theory')
plannerclass$tlfbase=ifelse(plannerclass$tlfbase==1,'temporal-logic','no-temporal-logic')
domainclass= read.csv('domains.csv')


scom='IPC2000'

summary.planner.inst=ddply(.data=compresultsraw, c("com","dom","gn","planner","Time","Steps"),  summarise, Solved.count=sum(solved) )
summary.planner.inst.bycom=summary.planner.inst[summary.planner.inst$com==scom&!is.na(summary.planner.inst$Time),]
summary.planner.inst.bycom=merge(summary.planner.inst.bycom,plannerclass,by="planner") 
summary.planner.inst.bycom=merge(summary.planner.inst.bycom,domainclass,by="dom")
tdcom=summary.planner.inst.bycom[summary.planner.inst.bycom$com==scom,]
dl=unique(summary.planner.inst.bycom$dom)
pl=unique(summary.planner.inst.bycom$planner)
ps= length(pl)
ds= length(dl)
lgv=log(summary.planner.inst.bycom$Time+1,10)

pl=unique(tdcom$planner)
pc=primary.colors(length(pl))
pcl=cbind(pc, pl[order(pl)])
colnames(pcl) = c( "color","planner")
tdcom$col=apply(tdcom, MARGIN=1, function(row){ pcl[pcl[,"planner"]==row["planner"],"color"]})

collist=rainbow(length(pl))#c("red1","aquamarine2","darkgoldenrod3", "brown4","burlywood1","chocolate2","darkolivegreen3")
tam=1.5
imprimirini(typ='eps',name=paste0("Layers/","bxplt_All_GDomain00_Planner_Time"),12,7.25)
yti <- bquote("Time in milliseconds "~10^x)

par(mar=c(5,5,3,13),xpd=FALSE)
boxplot( log(Time+1,10) ~ planner+dom,data=tdcom, las=2,col=pc, xaxt="n", xlab="Domains", ylab=yti, cex=tam, cex.lab=tam,cex.axis=tam, xlim=c(0,ps*ds))
axis(side=1, labels=sort(dl), cex.axis=tam, at=seq(ps/2,(ds*(ps)),ps) )
for( l in 1:(ds-1)){
    abline(v=(ps*l)+0.5,xpd=FALSE)
}
legend(x=ps*ds+3.5,y=round(max(log(tdcom$Time,10))), sort(pl),fill=pc,cex=tam, xpd=TRUE)
imprimirfin()

imprimirini(typ='eps',name=paste0("Layers/","bxplt_All_GDomain00_Planner_Steps"),12,7.25)
yti <- bquote("Number of steps at"~10^x)

par(mar=c(5,5,3,13),xpd=FALSE)
boxplot( log(Steps,10) ~ planner+dom,data=tdcom, las=2, xaxt="n", xlab="Domains", ylab=yti,col=pc, cex=tam, cex.lab=tam, cex.axis=tam, xlim=c(0,ps*ds))
axis(side=1, labels=sort(dl), cex.axis=tam, at=seq(ps/2,(ds*(ps)),ps) )
for( l in 1:(ds-1)){
    abline(v=(ps*l)+0.5,xpd=FALSE)
}
legend(x=ps*ds+3.5,y=max(log(tdcom$Steps,10)), sort(pl),fill=pc,cex=tam, xpd=TRUE)

imprimirfin()

#tdp=ddply(.data=tdcom, c("com","dom","planner","type"),  summarise, Solved.Count=sum(Solved.count) )
tdbar=ddply(.data=tdcom, c("com","dom","planner"),  summarise, Solved.Count=sum(Solved.count) )
tdbar$xval=paste0(tdbar$dom,'-',tdbar$planner)
maxprobdom=data.frame(dom=c("blocks","elevator","freecell","logistics","schedule"), prob=c(100,150,60,285,500))
myv= apply(tdbar,1,function(row){ as.numeric(row["Solved.Count"]) / maxprobdom[maxprobdom[,"dom"]==row["dom"],"prob"]})

myv=myv*100
#tdbar$Solved.Count
names(myv)=tdbar$xval

pl=unique(tdcom$planner)
pc=primary.colors(length(pl))
pcl=cbind(pc, pl[order(pl)])
colnames(pcl) = c( "color","planner")
tdbar$col=apply(tdbar, MARGIN=1, function(row){ pcl[pcl[,"planner"]==row["planner"],"color"]})

imprimirini(typ='eps',name=paste0("Layers/","barplt_All_GDomain00_Planner_Solved"),64,36)

par(mar=c(11,5,3,13),xpd=FALSE)
barplot(myv,ylab = "Percentage of solved instancies",xlab = "Domain", las=2, cex=tam, cex.lab=tam,cex.axis=tam, col=tdbar$col,xaxt="n") #
tpd=ddply(.data=tdbar,"dom", summarise, tp=length(planner) )
abl=0
pdl=c()
for( l in 1:(ds-1)){
    fv=abl+(tpd[l,]$tp*1.2)
    abline(v=fv+.1,xpd=FALSE)
    pdl=rbind(pdl,abl+(tpd[l,]$tp*1.2)/2 )    
    abl=fv
}
pdl=rbind(pdl,abl+(tpd[ds,]$tp*1.2)/2 )
legend(x=dim(tdbar)[1]+11,y=max(myv), legend=pcl[,"planner"],fill=pcl[,"color"],cex=tam,xpd=TRUE)
axis(side=1, labels=dl, cex.axis=tam, at=pdl )

imprimirfin()



#unique(td98$type)
tdcom=droplevels(tdcom)
boxplot(log(Time,10)~gpbase+type,data=td98)
boxplot(log(Time,10)~heuristicbase+type,data=td98)
boxplot(log(Time,10)~satbase+type,data=td98)
boxplot(Steps~gpbase+type,data=td98)
boxplot(Steps~heuristicbase+type,data=td98)
boxplot(Steps~satbase+type,data=td98)
td98p=ddply(.data=td98, c("com","dom","planner", "gpbase", "heuristicbase","satbase","type"),  summarise, Solved.Count=sum(Solved.count) )
boxplot(Solved.Count~gpbase+type,data=td98p)
boxplot(Solved.Count~heuristicbase+type,data=td98p)
boxplot(Solved.Count~satbase+type,data=td98p)
boxplot(Solved.Count~planner+type,data=td98p)

td6=ddply(.data=compresultsraw, c("com","dom","gn","planner","Time","Steps"),  summarise, Solved.count=sum(solved) )
td00=td6[td6$com=='IPC2000'&!is.na(td6$Time),]
td00=merge(td00,plannerclass,by="planner") 
td00=merge(td00,domainclass,by="dom") 
dl=unique(td00$dom)
pl=unique(td00$planner)
lgv=log(td00$Time+1,10)
boxplot( log(Time+1,10) ~ planner+dom,data=td00, las=2,col=rainbow(length(pl)), xaxt="n", yaxt="n", xlab="Domains", ylab="Time (miliseconds)")
axis(side=2, labels=paste("10^",seq(floor(min(lgv)),ceiling(max(lgv)),1),sep=""), cex.axis=0.9, at=seq(floor(min(lgv)),ceiling(max(lgv)),1), las=2)
axis(side=1, labels=sort(dl), cex.axis=0.9, at=seq(7.5,5*15,15))
abline(v=15.5,xpd=FALSE)
abline(v=30.5,xpd=FALSE)
abline(v=45.5,xpd=FALSE)
abline(v=60.5,xpd=FALSE)

legend(x=71.5,y=6.2, pl,fill=rainbow(length(pl)),xpd=TRUE)

unique(td00$type)
td00=droplevels(td00)
imprimirini(typ=typu,name=paste0("CompetitionsAnalysis/","IPC2000gpbase"),12,7.25)
boxplot(log(Time+1,10)~gpbase+type,data=td00)
imprimirfin()
boxplot(log(Time+1,10)~heuristicbase+type,data=td00)
boxplot(log(Time+1,10)~satbase+type,data=td00)
boxplot(log(Time+1,10)~binarydecisionbase+type,data=td00)
boxplot(log(Time+1,10)~regresionbase+type,data=td00)
boxplot(log(Time+1,10)~rewritingbase+type,data=td00)
boxplot(log(Time+1,10)~hobase+type,data=td00)
boxplot(log(Time+1,10)~gtbase+type,data=td00)
boxplot(log(Time+1,10)~tlfbase+type,data=td00)
boxplot(log(Steps+1)~gpbase+type,data=td00)
boxplot(log(Steps+1)~heuristicbase+type,data=td00)
boxplot(log(Steps+1)~satbase+type,data=td00)
boxplot(log(Steps+1)~binarydecisionbase+type,data=td00)
boxplot(log(Steps+1)~regresionbase+type,data=td00)
boxplot(log(Steps+1)~rewritingbase+type,data=td00)
boxplot(log(Steps+1)~hobase+type,data=td00)
boxplot(log(Steps+1)~gtbase+type,data=td00)
boxplot(log(Steps+1)~tlfbase+type,data=td00)
td00p=ddply(.data=td00, c("com","dom","planner", "gpbase", "heuristicbase","satbase","binarydecisionbase","regresionbase","rewritingbase","hobase","gtbase","tlfbase","type"),  summarise, Solved.Count=sum(Solved.count) )
boxplot(Solved.Count~gpbase+type,data=td00p)
boxplot(Solved.Count~heuristicbase+type,data=td00p)
boxplot(Solved.Count~satbase+type,data=td00p)
boxplot(Solved.Count~binarydecisionbase+type,data=td00p)
boxplot(Solved.Count~regresionbase+type,data=td00p)
boxplot(Solved.Count~rewritingbase+type,data=td00p)
boxplot(Solved.Count~hobase+type,data=td00p)
boxplot(Solved.Count~gtbase+type,data=td00p)
boxplot(StepsSolved.Count~tlfbase+type,data=td00p)
boxplot(Solved.Count~type+planner,data=td00p, las=2)
