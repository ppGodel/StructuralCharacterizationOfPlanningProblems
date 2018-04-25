source("rfunctions.r") 
grcon= mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="graphIPCResComp")
compresultsraw=data.frame(grcon$find())
exeres= read.csv("executionresults.csv")
exeres$pkey=paste(exeres$com,"-",exeres$dom,'-', exeres$gn, sep='')
compresultexec=merge(compresultsraw, exeres[,c("pkey","fap","pl","gl")], by="pkey")
compresultexec$gcomp=ifelse(compresultexec$pl>0,1,0)
compresultexec$parallel=ifelse(compresultexec$pl>compresultexec$gl,1,0)
compresultsgraphsolved=compresultexec[compresultexec$solved==1&compresultexec$gcomp==1&compresultexec$parallel==1,]
compresultsgraphsolved$LogTime=log(compresultsgraphsolved$Time+1)
compresultsgraphsolved$Class=0
compresultsgraphsolved$R2=0
compresultsgraphsolved$Dist=0
destfile="allclassifications-noparallel.csv"
allclass=NULL
prin=TRUE
if(!file.exists(destfile)){
    clas1=allplannersbydom(nx="D" ,ny="Time", data=compresultsgraphsolved, prnt=prin)
    clas2=allplannersbydom(nx="DM",ny="Time", data=compresultsgraphsolved,prnt=prin)
    clas3=allplannersbydom(nx="TN",ny="Time", data=compresultsgraphsolved,prnt=prin)
    clas4=allplannersbydom(nx="TE",ny="Time", data=compresultsgraphsolved,prnt=prin)
    clas5=allplannersbydom(nx="TME",ny="Time", data=compresultsgraphsolved,prnt=prin)
    allclass=rbind(clas1,clas2,clas3,clas4,clas5)
    write.csv(allclass, destfile)
}else{
    allclass = read.csv(destfile)
}

boxplot(R2~Cresp+Cfactor, data=allclass)
#ggplot(data = allclass[allclass$R2>=0.85&allclass$R2<0.985,], aes(x=factor(Cfactor), y=log(abs(Dist)+1))) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))#+facet_wrap(~dom)
#ggsave(paste0(gpath,"Layers/","DistanceDistribution.",typu), device=typu, width=12,height=7.25)

sqv=seq(0,1,0.025)
tda=aggregate(gn~R2+Cfactor+planner+com, allclass, FUN=length)
tda=cbind(1:dim(tda)[1],tda[order(tda$R2, decreasing=TRUE),])
higm=sapply(X=sqv, FUN=function(item){    
    #return(100*sum(tda$R2>=item)/length(tda$R2))
    return(sum(tda$R2>=item))
})
#logm=100-higm
logm=length(tda$R2)-higm
td=data.frame(x=sqv,value="pass", count=higm)
tp=data.frame(x=sqv,value="no pass", count=logm)
td=rbind(tp,td)
ggplot(data=td, aes(x=x, y=count, fill=value) )+geom_bar(stat="identity")+theme(axis.text=element_text(size=16), axis.title=element_text(size=20, face="bold"))+labs(x = "R^2", y="Model Count", fill="Value")+scale_x_continuous(breaks=seq(0, 1, 0.05))
ggsave(paste0(gpath,"Layers/","R2Dist.",typu), device=typu, width=12,height=7.25)

acinterval=allclass$R2>=0.85&allclass$R2<=0.985
#td2=aggregate(planner~Class+Cfactor, allclass[allclass$R2>=0.85&allclass$R2<=0.985,], FUN=length)
td2=aggregate(planner~Class+gn, allclass[acinterval,], FUN=length)
td2$Diff=""
td2[td2$Class==0,]$Diff="1 Average"
td2[td2$Class==1,]$Diff="2 Easy"
td2[td2$Class==2,]$Diff="3 Hard"
ggplot(data=td2, aes(x=(td2$Cfactor), y=gn, fill=as.factor(Diff)) )+geom_bar(stat="identity")+labs(x = "Property", y="Count", fill="Difficulty")
ggsave(paste0(gpath,"Layers/","InstanceDifficultyatR20.9.",typu), device=typu, width=12,height=7.25)

lr2=0.85
td3=aggregate(Dist~Class+Cfactor+gn, allclass[acinterval,], FUN=sum)
names(td3)[names(td3) == 'abs(Dist)'] <- 'Dist'
dtp=log(abs(td3[td3$Class==1,]$Dist)+1)#allclass$Cfactor=="DM"&
#mdtp=1 / max(dtp)
#dtp=dtp*mdtp
imprimirini(typ=typu,name=paste0("Layers/","HistDistanceR2",lr2,"All"),12,7.25)
hist(dtp, xlab="Normalized Distance", main=paste("Histogram of Distance"))
imprimirfin()
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
td4=ddply(.data=allclass[allclass$R2>lr2,], c("com","dom","gn","Cfactor","Class"),  summarise, Class.count=length(planner), Class.sum=sum(Class), Dist.sum=sum(abs(Dist)),Dist.max=max(abs(Dist)) )

td4$Vote=0
td4[td4$Class==0,]$Vote=1
td4[td4$Class==1,]$Vote=0
td4[td4$Class==2,]$Vote=2


td5=ddply(td4, c("com","dom","gn","Cfactor"),  summarise, Class.MaxVote=sum(Class.count)*2, Class.Vote=sum(Class.count*Vote), Class.Score=sum(Class.count*Vote)/(2*sum(Class.count))   )

#imprimirini(typ=typu,name=paste0("Layers/","HistDistanceR2",lr2,m,"C",c),12,7.25)
#boxplot(Class.Score~dom ,data=td5)
#imprimirfin()

#hist(log(allclass[allclass$Cfactor=="TME"&allclass$R2>=0.9&allclass$Class==1,]$Dist+1))


#+ labs( x=xt, y=yt, title=title )

#clas5=allplanners(nx="D" ,ny="LogTime", data=compresultsraw)
#boxplot(clas5$R2)
#clas6=allplanners(nx="TN",ny="LogTime", data=compresultsraw)
#clas7=allplanners(nx="TE",ny="LogTime", data=compresultsraw)
#clas8=allplanners(nx="PM",ny="LogTime", data=compresultsraw)

#allclass= join_all(dfs=list(clas1,clas2,clas3,clas4,clas5,clas6,clas7,clas8), by=c("comp","Planner","Dom","gn"),type="left")

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


scom='IPC1998'

summary.planner.inst=ddply(.data=compresultsraw, c("com","dom","gn","planner","Time","Steps"),  summarise, Solved.count=sum(solved) )
summary.planner.inst.bycom=summary.planner.inst[summary.planner.inst$com==scom&!is.na(summary.planner.inst$Time),]
summary.planner.inst.bycom=merge(summary.planner.inst.bycom,plannerclass,by="planner") 
summary.planner.inst.bycom=merge(summary.planner.inst.bycom,domainclass,by="dom") 
dl=unique(summary.planner.inst.bycom$dom)
pl=unique(summary.planner.inst.bycom$planner)
lgv=log(summary.planner.inst.bycom$Time+1,10)
collist=c("red1","aquamarine2","darkgoldenrod3", "brown4","burlywood1","chocolate2","darkolivegreen3")
tam=1.5
imprimirini(typ='eps',name=paste0("Layers/","bxplt_All_GDomain_Planner_Time"),12,7.25)
par(mar=c(5,5,3,3),xpd=FALSE)
yti <- bquote("Time in milliseconds "~10^x)
boxplot( log(Time+1,10) ~ planner+dom,data=td98, las=2,col=collist, xaxt="n", xlab="Domains", ylab=yti, cex=tam, cex.lab=tam,cex.axis=tam)
axis(side=1, labels=sort(dl), cex.axis=tam, at=seq(2.5,20,4))
abline(v=4.5,xpd=FALSE)
abline(v=8.5,xpd=FALSE)
abline(v=12.5,xpd=FALSE)
abline(v=16.5,xpd=FALSE)
legend(x=17,y=6.7, sort(pl),fill=collist,cex=tam)
imprimirfin()

imprimirini(typ='eps',name=paste0("Layers/","bxplt_All_GDomain_Planner_Steps"),12,7.25)
par(mar=c(5,5,3,3),xpd=FALSE)
boxplot( Steps ~ planner+dom,data=td98, las=2, xaxt="n", xlab="Domains", ylab="Number of steps",col=collist, cex=tam, cex.lab=tam,cex.axis=tam)
axis(side=1, labels=sort(dl), cex.axis=tam, at=seq(2.5,20,4))
abline(v=4.5,xpd=FALSE)
abline(v=8.5,xpd=FALSE)
abline(v=12.5,xpd=FALSE)
abline(v=16.5,xpd=FALSE)
legend(x=17,y=150, sort(pl),fill=collist,cex=tam)
imprimirfin()

unique(td98$type)
td98=droplevels(td98)
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
td98bar=ddply(.data=td98p, c("com","dom","planner"),  summarise, Solved.Count=sum(Solved.Count) )
td98bar$xval=paste0(td98bar$dom,'-',td98bar$planner)
myv=td98bar$Solved.Count
names(myv)=td98bar$xval
#par(mar=c(11,5,3,3),xpd=FALSE)
imprimirini(typ='eps',name=paste0("Layers/","barplt_All_GDomain_Planner_Solved"),12,7.25)
barplot(myv,ylab = "Solved instancies",xlab = "Domain",xaxt="n", las=2,col=collist, cex=tam, cex.lab=tam,cex.axis=tam)
abline(v=4.9,xpd=FALSE)
abline(v=9.7,xpd=FALSE)
abline(v=14.5,xpd=FALSE)
abline(v=19.3,xpd=FALSE)
legend(x=19.5,y=29, sort(pl),fill=collist,cex=tam)
axis(side=1, labels=dl, cex.axis=tam, at=seq(2.5,22.5,4.8) )
imprimirfin()

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
