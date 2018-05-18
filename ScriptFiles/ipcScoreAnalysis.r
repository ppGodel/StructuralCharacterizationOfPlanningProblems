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

bdf=allclasscompar[between(allclasscompar$R2,0.85,0.98),]
mddf=aggregate(abs(Dist)~com+dom+planner,bdf,FUN=max)
names(mddf)= c("com","dom","planner","maxDist")
sddf=aggregate(abs(Dist)~com+dom+planner,bdf,FUN=sum)
names(sddf)= c("com","dom","planner","sumDist")
tddf=aggregate(Dist~com+dom+planner,bdf,FUN=length)
names(tddf)= c("com","dom","planner","totDist")
bdf= merge(bdf,mddf, by=c("com","dom","planner"))
bdf= merge(bdf,sddf, by=c("com","dom","planner"))
bdf= merge(bdf,tddf, by=c("com","dom","planner"))

td3=droplevels(aggregate(planner~Disc+MahaOut+CookOut+Dist+sumDist+MahaDist+CookDist+R2+Class+com+dom+gn, bdf, FUN=length))
td3tg=ddply(.data=td3, c("com","dom","gn"),  summarise, tp=length(planner) )
td3=merge(td3, td3tg, by=c("com","dom","gn"))
td3[td3$Disc==2,]$Disc=2
td3[td3$Disc==0,]$Disc=1
td3$Diff=NA
td3$Ndist=(abs(td3$Dist))/td3$sumDist
td3$MahaDiff="Average"
td3$CookDiff="Average"
td3[td3$Class==0,]$Diff="Average"
td3[td3$Class==1,]$Diff="Easy"
td3[td3$Class==2,]$Diff="Hard"
td3[td3$Disc==1 & td3$MahaOut==1,]$MahaDiff="Easy"
td3[td3$Disc==2 & td3$MahaOut==1,]$MahaDiff="Hard"
td3[td3$Disc==1 & td3$CookOut==1,]$CookDiff="Easy"
td3[td3$Disc==2 & td3$CookOut==1,]$CookDiff="Hard"

#td3ND=aggregate(planner~dom+gn+Diff, td3,FUN=sum)
td3ND=ddply(.data=td3, c("com","dom","gn","Diff","tp"),  summarise, planner=sum(planner), DistV=mean(Ndist) )
td3ND$score=td3ND$planner*td3ND$DistV/td3ND$tp
td3MD=ddply(.data=td3, c("com","dom","gn","MahaDiff"),  summarise, planner=sum(planner), DistV=mean(Ndist) )
td3CD=ddply(.data=td3, c("com","dom","gn","CookDiff"),  summarise, planner=sum(planner), DistV=mean(CookDist) )

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





diffic="Easy"
td3ND$score=td3ND$DistV*(td3ND$planner/td3ND$tp)
td3ND$coun= round(td3ND$score,2)
tdf=ddply(.data=td3ND[td3ND$Diff==diffic,], c("coun"), summarise, t=length(gn))

plot(x=abs(tdf$coun),y=log(tdf$t), type='l', xlab="Score", ylab="freq", main=diffic )

pres=td3ND[td3ND$Diff!="Average",c("com","dom","gn","Diff","score")]

#merge(x=pres,y=compresultsgraphsolved, by=)
prest=pres
prest$score=prest$score*ifelse(prest$Diff==diffic,-1,1)
#position = position_dodge()
ggplot(data =prest[!between(prest$score,-0.001,0.001),], aes(x=factor(gn), y=score, fill=as.factor(Diff) )) +geom_bar(stat="identity", position=position_stack(reverse = TRUE)) + theme(text = element_text(size=10)) + labs(x="Instance", y="score", fill="Difficulty") + coord_flip() + scale_fill_manual(values=c("skyblue", "orange1", "red")) + scale_x_discrete( limits=rev(levels(prest$gn)))
#ggsave(paste0(gpath,"Layers/","ClassByMahaDistParallelbyComR2Int.",typu), device=typu, width=12,height=7.25)


head(prest)
hist(prest$score,breaks=seq(-1, 1, 0.01))
dim(prest)
dim(prest[!between(prest$score,-0.09,0.092),])


tdfe=ddply(.data=prest, c("com","dom","gn"), summarise, easyScore= min(score), hardScore= max(score))
tdfe[tdfe$easyScore>0,]$easyScore=0
tdfe[tdfe$hardScore<0,]$hardScore=0
dim(tdfe)
dim(tdfe[tdfe$easyScore==0|tdfe$hardScore==0,])
dim(tdfe[((tdfe$easyScore==0&tdfe$hardScore<0.001)|(tdfe$hardScore==0&tdfe$easyScore>(-0.001))),])
tdfe1=tdfe[tdfe$easyScore<(-0.001)&tdfe$hardScore>0.001,]
tdfe1$sumScore=tdfe1$hardScore+abs(tdfe1$easyScore)
tdfe1$difScore=tdfe1$hardScore+tdfe1$easyScore
tdfe1$minScore=ifelse(tdfe1$hardScore<abs(tdfe1$easyScore),tdfe1$hardScore,tdfe1$easyScore)
tdfe1$maxScore=ifelse(tdfe1$hardScore>abs(tdfe1$easyScore),tdfe1$hardScore,tdfe1$easyScore)
tdfe1$Dmdcore=abs(tdfe1$minScore)-abs(tdfe1$difScore)
tdfe1$pdif=tdfe1$difScore/(tdfe1$maxScore)
dim(tdfe1)
tdfe1[which.min(tdfe1$easyScore),]
tdfe1[which.max(tdfe1$hardScore),]
tdfe1[which.max(abs(tdfe1$sumScore)),]
tdfe1[which.max(abs(tdfe1$difScore)),]
tdfe1[which.min(abs(tdfe1$difScore)),]
tdfe1[which.max(abs(tdfe1$maxScore)),]
#[!between(prest$score,-0.001,0.001),]
hist(c(tdfe1$easyScore,tdfe1$hardScore),breaks=seq(-1, 1, 0.01))


tdfe2=tdfe1[tdfe1$Dmdcore>0,]
tdfe1[tdfe1$pdif>0.33&abs(tdfe1$difScore)>0.001,]
dim(tdfe1[tdfe1$pdif>0.33&abs(tdfe1$difScore)>0.001,])

tdfe1$difScore/abs(tdfe1$maxScore)
tdfe2[which.min(tdfe2$Dmdcore),]

tdfe1$rscore=round(tdfe1$score,2)
110 T
56 NC                                  54 PC
24 PC             32 CN                21 CN           33 C
5CN     19 C
