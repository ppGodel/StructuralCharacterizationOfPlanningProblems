source("rfunctions.r")

grcon= mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="graphIPCResComp")
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

typu="eps"
prin=TRUE
allclasscompar=createDataSetbyComWithClassification(compresultsgraphsolved[compresultsgraphsolved$parallel==1,],"allclassifications-parallelbycom.csv",prin,"ParByCom")
allclasscomnpar=createDataSetbyComWithClassification(compresultsgraphsolved[compresultsgraphsolved$parallel==0,],"allclassifications-noparallelbycom.csv",prin,"NoParByCom")

allclassdompar=createDataSetbyDomWithClassification(compresultsgraphsolved[compresultsgraphsolved$parallel==1,],"allclassifications-parallelbydom.csv",prin,"ParByDom" )
allclassdomnpar=createDataSetbyDomWithClassification(compresultsgraphsolved[compresultsgraphsolved$parallel==0,],"allclassifications-noparallelbydom.csv",prin,"NoParByDom" )

prcon= mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="summNLbyGC")
graphc=mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="graphs")
propraw=data.frame(prcon$find())
graphraw=data.frame(graphc$find(fields='{"_id":1 , "gn":1, "dom":1, "pn":1, "com":1,"cdkey":1, "pkey":1}'))
colnames(graphraw)=c("gid","gn", "dom", "pn", "com", "cdkey", "pkey")

allclasscompar$type="Parallel"
allclasscomnpar$type="NoParallel"
allclasscom=rbind(allclasscompar,allclasscomnpar)
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
tdf=ddply(.data=td3ND[td3ND$Diff==diffic,], c("coun"), summarise, t=length(gn))
pres=td3ND[td3ND$Diff!="Average",c("com","dom","gn","Diff","score")]
prest=pres
prest$score=prest$score*ifelse(prest$Diff==diffic,-1,1)
tdfe=ddply(.data=prest, c("com","dom","gn"), summarise, easyScore= min(score), hardScore= max(score))
tdfe[tdfe$easyScore>0,]$easyScore=0
tdfe[tdfe$hardScore<0,]$hardScore=0



dim(graphraw)
dim(propraw)
propgres=merge(propraw,graphraw, by="gid")
testres=merge(propgres,tdfe, by=c("com","dom","gn"))
write.csv(testres,"propertiesnpresults.csv")


analisisfn(data=testres, diff=Diffy, type="action")

testres$Class="4 ND"
testres[(testres$easyScore==0&testres$hardScore>0.001)|(testres$easyScore>0.001&testres$hardScore>0.001&testres$hardScore>(3/2*testres$easyScore) ),]$Class="3 Hard"
testres[(testres$hardScore==0&testres$easyScore>0.001)|(testres$easyScore>0.001&testres$hardScore>0.001&testres$easyScore>(3/2*testres$hardScore) ),]$Class="1 Easy"
testres[(testres$hardScore<0.001&testres$easyScore<0.001),]$Class="2 Fit"

ptype="parallel"
diffl=c("PDE","POE","PME")
propl=c("PDE","POE","PME")
sdisl=c("mean","max","sd","kurt","skew")
typel=c("facts","actions")
for(type in typel){
    for(p in propl){
        for(s in sdisl){
            imprimirini(typ=typu,name=paste0("PropertiesAnalysis/",ptype,type,s,p),12,7.25)
            boxplot(testres[,paste0(s,p)]~testres$Class, ylab=paste0(s,p),xlab=)
            imprimirfin()
        }
    }
}


