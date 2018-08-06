source("connections.r")
source("parameters.r")
source("rfunctions.r")
library(gridExtra)
library(grid)
library(dunn.test)
library(ggplot2)
library(wesanderson)

compresultsraw=data.frame(grcon$find())
exeres= read.csv("executionresults.csv")
exeres$pkey=paste(exeres$com,"-",exeres$dom,'-', exeres$gn, sep='')
compresultexec=merge(compresultsraw, exeres[,c("pkey","fap","pl","gl")], by="pkey", all.x=TRUE)
compresultexec=compresultexec[!duplicated(compresultexec),]
compresultexec$gcomp=ifelse(compresultexec$pl>0,1,0)
compresultexec$parallel=ifelse(compresultexec$pl>compresultexec$gl,1,0)
compresultsgraphsolved=compresultexec[compresultexec$solved==1&compresultexec$graph==1&compresultexec$gcomp==1,]
crgs=compresultsgraphsolved
compresultsgraphsolved$LogTime=log(compresultsgraphsolved$Time+1)
compresultsgraphsolved$Class=0
compresultsgraphsolved$R2=0
compresultsgraphsolved$Dist=0
compresultsgraphsolved$MahaDist=0
compresultsgraphsolved$CookDist=0
compresultsgraphsolved$Disc=0
compresultsgraphsolved$MahaOut=FALSE
compresultsgraphsolved$CookOut=FALSE
 
#typu="eps"
#prin=TRUE
if(FALSE){
    allclassplanpar=createDataSetbyPlanWithClassification(compresultsgraphsolved[compresultsgraphsolved$parallel==1,],"allclassifications-parallelbyplan.csv",prin,"ParByPlan" )
    allclassplannpar=createDataSetbyPlanWithClassification(compresultsgraphsolved[compresultsgraphsolved$parallel==0,],"allclassifications-noparallelbyplan.csv",prin,"NoParByPlan" )
    allclassplanpar$type="Parallel"
    allclassplannpar$type="NoParallel"
    allclasscom=rbind(allclassplanpar,allclassplannpar)
}else{
    if(bycom==T){
        allclasscompar=createDataSetbyComWithClassification(compresultsgraphsolved[compresultsgraphsolved$parallel==1,],"allclassifications-parallelbycomimg.csv",prin,"ParByCom")
        allclasscomnpar=createDataSetbyComWithClassification(compresultsgraphsolved[compresultsgraphsolved$parallel==0,],"allclassifications-noparallelbycomimg.csv",prin,"NoParByCom")
        allclasscompar$type="Parallel"
        allclasscomnpar$type="NoParallel"
        allclasscom=rbind(allclasscompar,allclasscomnpar)    
    }else{
        allclassdompar=createDataSetbyDomWithClassification(compresultsgraphsolved[compresultsgraphsolved$parallel==1,],"allclassifications-parallelbydom.csv",prin,"ParByDom" )
        allclassdomnpar=createDataSetbyDomWithClassification(compresultsgraphsolved[compresultsgraphsolved$parallel==0,],"allclassifications-noparallelbydom.csv",prin,"NoParByDom" )
        allclassdompar$type="Parallel"
        allclassdomnpar$type="NoParallel"
        allclasscom=rbind(allclassdompar,allclassdomnpar)  
    }
}



bdf=allclasscom[between(allclasscom$R2,0.85,0.98),]
mddf=aggregate(abs(Dist)~com+dom+planner+type,bdf,FUN=max)
names(mddf)= c("com","dom","planner","type","maxDist")
sddf=aggregate(abs(Dist)~com+dom+planner+type,bdf,FUN=sum)
names(sddf)= c("com","dom","planner","type","sumDist")
tddf=aggregate(Dist~com+dom+planner+type,bdf,FUN=length)
names(tddf)= c("com","dom","planner","type","totDist")
bdf= merge(bdf,mddf, by=c("com","dom","planner","type"))
bdf= merge(bdf,sddf, by=c("com","dom","planner","type"))
bdf= merge(bdf,tddf, by=c("com","dom","planner","type"))

td3=droplevels(aggregate(planner~Disc+Dist+sumDist+R2+type+Class+com+dom+gn, bdf, FUN=length))
td3tg=ddply(.data=td3, c("type","com","dom","gn"),  summarise, tp=length(planner) )
td3=merge(td3, td3tg, by=c("type","com","dom","gn"))
td3[td3$Disc==2,]$Disc=2
td3[td3$Disc==0,]$Disc=1
td3$Diff=NA
td3$Ndist=(abs(td3$Dist))/td3$sumDist

td3[td3$Class==0,]$Diff="Fit"
td3[td3$Class==1,]$Diff="Easy"
td3[td3$Class==2,]$Diff="Hard"
td3ND=ddply(.data=td3, c("type","com","dom","gn","Diff","tp"),  summarise, planner=sum(planner), DistV=mean(Ndist) )
td3ND[td3ND$Diff=="Easy",]$planner=-td3ND[td3ND$Diff=="Easy",]$planner
diffic="Easy"
td3ND$score=td3ND$DistV*(td3ND$planner/td3ND$tp)
td3ND$coun= round(td3ND$score,2)
#tdf=ddply(.data=td3ND[td3ND$Diff==diffic,], c("coun"), summarise, t=length(gn))
prest=td3ND[,c("type","com","dom","gn","Diff","score")]
#prest$score=prest$score*ifelse(prest$Diff==diffic,-1,1)
tdfe=ddply(.data=prest, c("type","com","dom","gn"), summarise, easyScore= min(score), hardScore= max(score))
tdfe[tdfe$easyScore>=0,]$easyScore=0
tdfe[tdfe$hardScore<0,]$hardScore=0
tdfe$Class="4 Not Class"
tdfe[(tdfe$easyScore==0&tdfe$hardScore>0.001)|(abs(tdfe$easyScore)>0.001&tdfe$hardScore>0.001&tdfe$hardScore>(3/2*abs(tdfe$easyScore)) ),]$Class="3 Hard"
tdfe[(tdfe$hardScore==0&abs(tdfe$easyScore)>0.001)|(abs(tdfe$easyScore)>0.001&tdfe$hardScore>0.001&abs(tdfe$easyScore)>(3/2*tdfe$hardScore) ),]$Class="1 Easy"
tdfe[(tdfe$hardScore<0.001&abs(tdfe$easyScore)<0.001),]$Class="2 Fit"


baseres=ddply(compresultexec,c("com","dom","gn"), summarise, minSteps=min(Steps,na.rm=T), solved=max(solved), graph=min(graph), fap=min(fap), graphSize=min(gl), parallel=min(parallel) )
tdfec=tdfe[,c("type","com","dom","gn","Class")]
testres=merge(baseres,tdfec, by=c("com","dom","gn"),all.x=TRUE)
testres[is.na(testres$Class)&testres$solved==1&testres$graph==1,]$Class="6 Not Proc"
testres[is.na(testres$Class)&testres$solved==1&testres$graph==0,]$Class="6 Not Processed NG"
testres[is.na(testres$Class)&testres$solved==0&testres$graph==1,]$Class="5 Not Solv"
testres[is.na(testres$Class)&testres$solved==0&testres$graph==0,]$Class="5 Not Solved NG"

propraw=data.frame(prcon$find())
graphraw=data.frame(graphc$find(fields='{"_id":1 , "gn":1, "dom":1, "pn":1, "com":1,"cdkey":1, "pkey":1}'))
colnames(graphraw)=c("gid","gn", "dom", "pn", "com", "cdkey", "pkey")
propgres=merge(propraw,graphraw, by="gid")
testres=merge(testres,propgres, by=c("com","dom","gn"), all.x=T)
write.csv(testres,"propertiesresultsbyplan.csv")
#testres= as.data.frame(read.csv("propertiesresultsbyplan.csv"))
#head(testres)

preinf=ddply(.data=testres,c("Class","com","dom","gn"), summarise, countn=length(Class))
ppieinfo=ddply(.data=preinf,c("Class", "com"), summarise, countn=length(gn))
totinf=ddply(.data=ppieinfo,c("com"), summarise, total=sum(countn))
pieinfo=merge(ppieinfo,totinf, by="com")
pieinfo$percentage=pieinfo$countn/pieinfo$total*100
pieinfo$Class=factor(pieinfo$Class, levels=levels(pieinfo$Class)[order(levels(pieinfo$Class), decreasing=T)])
piplot=ggplot(pieinfo[order(pieinfo$Class,decreasing=T),], aes(x="", y=percentage, fill=Class)) + geom_bar(width = 1, stat="identity", position="fill") + facet_wrap(~com) + scale_fill_brewer(palette="RdYlGn") + coord_polar(theta="y") + guides(fill = guide_legend(reverse = T))
                                        #+ geom_text(aes(y = c(0,cumsum(percentage)[-length(percentage)]), label = percentage), size=5)  
ggsave(filename=paste0(gpath,"PropertiesAnalysis/piechartbyplan",".",typu), device=typu, width=12,height=7.25, plot=piplot)
