source("rfunctions.r")
library(gridExtra)
library(grid)
library(dunn.test)
library(ggplot2)

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
propgres=merge(propraw,graphraw, by="gid")

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

testres=merge(testres,propgres, by=c("com","dom","gn"), all.x=T)

write.csv(testres,"propertiesresults.csv")

testres= as.data.frame(read.csv("propertiesresults.csv"))

preinf=ddply(.data=testres,c("Class","com","dom","gn"), summarise, countn=length(Class))
ppieinfo=ddply(.data=preinf,c("Class", "com"), summarise, countn=length(gn))
totinf=ddply(.data=ppieinfo,c("com"), summarise, total=sum(countn))
pieinfo=merge(ppieinfo,totinf, by="com")
pieinfo$percentage=pieinfo$countn/pieinfo$total*100
piplot=ggplot(pieinfo, aes(x="", y=percentage, fill=Class))+  geom_bar(width = 1, stat="identity")+  coord_polar("y",direction=-1) + facet_wrap(~com)
                                        #+ geom_text(aes(y = c(0,cumsum(percentage)[-length(percentage)]), label = percentage), size=5)  
ggsave(filename=paste0(gpath,"PropertiesAnalysis/piechartbycom",".",typu), device=typu, width=12,height=7.25, plot=piplot)


head(testres)
npcon= mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="nodePercentageEdges")
ptypel=c("Parallel","NoParallel")
typel=c("facts","actions")
propl=c("PDE","POE","PME")
sdisl=c("mean","max","sd","kurt","skew")

for(ptype in ptypel){
    for(type in typel){
        tin=testres[testres$graph==1&(testres$type==ptype|is.na(testres$type))&testres$Y==strtrim(type,1),c("Class", "minPOE", "maxPOE",  "meanPOE", "sdPOE", "kurtPOE", "skewPOE", "minPDE", "maxPDE", "meanPDE", "sdPDE", "kurtPDE", "skewPDE", "minPME", "maxPME", "meanPME", "sdPME", "kurtPME", "skewPME")]
        tin$easy=tin$Class=="1 Easy"
        tin$fit=tin$Class=="2 Fit"
        tin$hard=tin$Class=="3 Hard"
        tin$NoClass=tin$Class=="4 Not Class"
        tin$NoSolv=tin$Class=="5 Not Solv"
        
        
        tin$Class=NULL
        nco=dim(tin)[2]
        rco=nco-4
        methl=c("spearman","kendall")
        for(meth in methl){
            imprimirini(typ=typu,name=paste0("PropertiesAnalysis/",ptype,type,"Classification",meth),12,7.25)
            corrplot(cor(tin, method=meth, use="complete.obs")[rco:nco,1:(rco-1)], method="number", p.mat=cor.mtest(tin, method=meth, use="complete.obs")$p[rco:nco,1:(rco-1)])
            imprimirfin()
        }
        
        info=testres[testres$graph==1&(testres$type==ptype|is.na(testres$type))&testres$Y==strtrim(type,1),]
        if(dim(info)[1]>0){
            gids=integer()
            for(lv in levels(info$Class)){
                tinf=info[info$Class==lv,]
                if(as.numeric(strtrim(lv,1))==1|as.numeric(strtrim(lv,1))==3 ){
                    tgids=tinf$gid
                }else{
                    tgids=sample(tinf$gid,dim(tinf)[1]/10)
                }
                gids=union(gids,tgids)
            }
            for(g in gids){
                ginfo=info[info$gid==g,]
                graphdistvalues=data.frame(npcon$find(query=paste0('{"gid":{"$oid":"',g,'"}}')))
                graphdistvalues$LPOE=round(graphdistvalues$POE+0.5, 0)
                graphdistvalues$LPDE=round(graphdistvalues$PDE+0.5, 0)
                graphdistvalues$LPME=round(graphdistvalues$PME+0.5, 0)
                graphdistvaluesbyl=ddply(.data=graphdistvalues, c("gid","T","Y"), summarise, TNodesL=length(PDE) )
                graphdistvaluesbyppoe=ddply(.data=graphdistvalues, c("gid","T","Y","LPOE"), summarise, TNodes=length(POE) )
                graphdistvaluesbyppde=ddply(.data=graphdistvalues, c("gid","T","Y","LPDE"), summarise, TNodes=length(PDE) )
                graphdistvaluesbyppme=ddply(.data=graphdistvalues, c("gid","T","Y","LPME"), summarise, TNodes=length(PME) )
                #graphdistvaluesbylgp=merge(graphdistvaluesbyppoe,graphdistvaluesbyppde, c("gid","T","Y"))
                #graphdistvaluesbylgp=merge(graphdistvaluesbylgp,graphdistvaluesbyppme, c("gid","T","Y"))
                #head(graphdistvaluesbylgp)
                graphdistvaluesbylgpoe=merge(graphdistvaluesbyppoe,graphdistvaluesbyl, by=c("gid","T","Y"))
                graphdistvaluesbylgpde=merge(graphdistvaluesbyppde,graphdistvaluesbyl, by=c("gid","T","Y"))
                graphdistvaluesbylgpme=merge(graphdistvaluesbyppme,graphdistvaluesbyl, by=c("gid","T","Y"))
                
                head(graphdistvaluesbylgpde)
                graphdistvaluesbylgpoe$PNodesbyL=graphdistvaluesbylgpoe$TNodes/graphdistvaluesbylgpoe$TNodesL*100
                graphdistvaluesbylgpde$PNodesbyL=graphdistvaluesbylgpde$TNodes/graphdistvaluesbylgpde$TNodesL*100
                graphdistvaluesbylgpme$PNodesbyL=graphdistvaluesbylgpme$TNodes/graphdistvaluesbylgpme$TNodesL*100

                poeplot=ggplot(data=graphdistvaluesbylgpoe, aes(x=LPOE, y=PNodesbyL, fill=factor(Y)))+ scale_y_sqrt() + geom_bar(stat = "identity")+ facet_wrap(Y~T,nrow=2) + ggtitle(paste(ginfo$gn,ginfo$Class)) + theme(strip.text.x = element_blank()) + guides(fill=FALSE)
                pdeplot= ggplot(data=graphdistvaluesbylgpde, aes(x=LPDE, y=PNodesbyL, fill=factor(Y))) + geom_bar(stat = "identity")+ facet_wrap(Y~T,nrow=2) + theme(strip.text.x = element_blank()) + guides(fill=FALSE)+ scale_y_sqrt()
                pmeplot= ggplot(data=graphdistvaluesbylgpme, aes(x=LPME, y=PNodesbyL, fill=factor(Y))) + geom_bar(stat = "identity")+ facet_wrap(Y~T,nrow=2) + theme(strip.text.x = element_blank()) + guides(fill=FALSE)+ scale_y_sqrt()
                
                #poeplot= qplot(POE, data=graphdistvalues, geom='histogram', fill=factor(Y))+ facet_wrap(Y~T,nrow=2)+ theme(strip.text.x = element_blank())+ guides(fill=FALSE)
                #pdeplot= qplot(PDE, data=graphdistvalues, geom='histogram', fill=factor(Y))+ facet_wrap(Y~T,nrow=2)+ theme(strip.text.x = element_blank())+ guides(fill=FALSE)
                #pmeplot=qplot(PME, data=graphdistvalues, geom='histogram', fill=factor(Y))+ facet_wrap(Y~T,nrow=2)+ theme(strip.text.x = element_blank())+ guides(fill=FALSE)

                graphdistvaluesbyl=ddply(.data=graphdistvalues,c ("gid","T","Y"), summarise, TNodes=length(PDE))
                lvlplot=qplot(x=T,y=TNodes, data=graphdistvaluesbyl, fill=factor(Y))+geom_area()
                
                imprimirini(typ=typu,name=paste0("PropertiesAnalysis/hist",ptype,type,ginfo$Class,ginfo$gn),12,7.25)
                grid.arrange(poeplot, pdeplot, pmeplot,lvlplot, ncol=1)
                imprimirfin()
            }
            tin=info[,c("Class", "minPOE", "maxPOE",  "meanPOE", "sdPOE", "kurtPOE", "skewPOE", "minPDE", "maxPDE", "meanPDE", "sdPDE", "kurtPDE", "skewPDE", "minPME", "maxPME", "meanPME", "sdPME", "kurtPME", "skewPME")]
            tin$easy=tin$Class=="1 Easy"
            tin$fit=tin$Class=="2 Fit"
            tin$hard=tin$Class=="3 Hard"
            tin$NoClass=tin$Class=="4 Not Class"
            tin$NoSolv=tin$Class=="5 Not Solv"
            
            
            tin$Class=NULL
            nco=dim(tin)[2]
            rco=nco-4
            methl=c("spearman","kendall")
            for(meth in methl){
                imprimirini(typ=typu,name=paste0("PropertiesAnalysis/",ptype,type,"Classification",meth),12,7.25)
                corrplot(cor(tin, method=meth, use="complete.obs")[rco:nco,1:(rco-1)], method="number", p.mat=cor.mtest(tin, method=meth, use="complete.obs")$p[rco:nco,1:(rco-1)])
                imprimirfin()
            }
            
            for(p in propl){
                for(s in sdisl){
                                        #imprimirini(typ=typu,name=paste0("PropertiesAnalysis/",ptype,type,s,p),12,7.25)
                                        #boxplot(info[,paste0(s,p)]~info[,"Class"], ylab=paste0(s,p),xlab="Class")
                    linM=lm(info[,paste0(s,p)]~info[,"Class"])
                    residuales<-resid(linM)
                    sht=shapiro.test(residuales)
                                        #qqnorm(residuales,col=rgb(0,1,0,0.5))
                                        #qqline(residuales,col="red")
                    if(sht$p.value>0.05){                   
                        rno="residuals are from a normal dist"                    
                        anov=aov(info[,paste0(s,p)]~info[,"Class"])
                        if(summary.aov(anov)[[1]][["Pr(>F)"]][1]<0.05){
                            me="means are different"
                            TukeyHSD(anov)
                        }else{
                            me="means are equal"
                    }
                    }else{          
                        rno="resid arent from a norm dist"   
                        krus=kruskal.test(info[,paste0(s,p)]~info[,"Class"])
                        if(krus$p.value<0.05){
                            me="means are different"
                            dn=dunn.test(x=info[,paste0(s,p)], g=info[,"Class"])
                        }else{
                            me="means are equal"
                        }
                    }
                                        #print(paste0(rno,", ",me))
                                        #print(dn$comparisons[dn$P.adjusted<0.005])
                    collaps="\n"
                    dr=paste(dn$comparisons[dn$P.adjusted<0.005],collapse=collaps)
                    labrc=c(rno,me,"for: ", dr )
                    labr=paste(labrc,collapse=collaps)
                    pg=ggplot(data = info, aes(x=factor(Class), y=info[,paste0(s,p)]), log="y") + geom_violin(fill="orange", color="red")   + theme(text = element_text(size=15), plot.margin = unit(c(1, 12.5, 1.5, 2), "lines") )+labs(x="Difficulty Set", y=paste0(s))+ geom_boxplot(width=0.03, fill="blue", color="white")+ggtitle(paste(type,p)) + annotation_custom(grob = textGrob(labr), xmin = 7, xmax = 8, ymin = round(max(info[,paste0(s,p)], na.rm=T),2)*.9, ymax = round(max(info[,paste0(s,p)], na.rm=T),2)*.8)
                    
                    
                    gt <- ggplot_gtable(ggplot_build(pg))
                    gt$layout$clip[gt$layout$name=="panel"] <- "off"
                    grid.draw(gt)
                                        #+facet_wrap(~dom)
                                        #imprimirfin()
                    ggsave(filename=paste0(gpath,"PropertiesAnalysis/Boxplot",ptype,type,s,p,".",typu), device=typu, width=12,height=7.25, plot=gt)
                }
            }
        }
    }
}

