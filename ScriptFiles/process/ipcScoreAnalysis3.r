library(gridExtra)
library(grid)
library(dunn.test)
library(ggplot2)
library(plyr)
source('parameters.r')
source('rfunctions.r')
source('connections.r')
testres= as.data.frame(read.csv("propertiesresultsbycom.csv"))

preinf=ddply(.data=testres,c("Class","com","dom","gn"), summarise, countn=length(Class))
ppieinfo=ddply(.data=preinf,c("Class", "com"), summarise, countn=length(gn))
totinf=ddply(.data=ppieinfo,c("com"), summarise, total=sum(countn))
pieinfo=merge(ppieinfo,totinf, by="com")
pieinfo$percentage=pieinfo$countn/pieinfo$total*100
piplot=ggplot(pieinfo, aes(x="", y=percentage, fill=Class))+  geom_bar(width = 1, stat="identity")+  coord_polar("y",direction=-1) + facet_wrap(~com)+ theme(text = element_text(size=20))
                                        #+ geom_text(aes(y = c(0,cumsum(percentage)[-length(percentage)]), label = percentage), size=5)  
ggsave(filename=paste0(gpath,"PropertiesAnalysis/piechartbycom",".",typu), device=typu, width=12,height=7.25, plot=piplot)


preinf=ddply(.data=testres,c("solved","com","dom","gn"), summarise, countn=length(solved))
ppieinfo=ddply(.data=preinf,c("solved", "com"), summarise, countn=length(gn))
totinf=ddply(.data=ppieinfo,c("com"), summarise, total=sum(countn))
pieinfo=merge(ppieinfo,totinf, by="com")
pieinfo$percentage=pieinfo$countn/pieinfo$total*100
pieinfo$Status="Not Solved"
pieinfo[pieinfo$solved==1,]$Status="Solved"
#pieinfo[pieinfo$solved==0,]$solved="Not Solved"
piplot=ggplot(pieinfo, aes(x="", y=percentage, fill=Status))+  geom_bar(width = 1, stat="identity")+  coord_polar("y",direction=-1) + facet_wrap(~com) + theme(text = element_text(size=20))
                                        # + geom_text(aes(y = c(0,cumsum(percentage)[-length(percentage)]), label = percentage), size=5)                     
ggsave(filename=paste0(gpath,"PropertiesAnalysis/piechartbycomsolv",".",typu), device=typu, width=12,height=7.25, plot=piplot)


preinf=ddply(.data=testres[testres$solved==1&!is.na(testres$parallel),],c("parallel","com","dom","gn"), summarise, countn=length(solved))
ppieinfo=ddply(.data=preinf,c("parallel", "com"), summarise, countn=length(gn))
totinf=ddply(.data=ppieinfo,c("com"), summarise, total=sum(countn))
pieinfo=merge(ppieinfo,totinf, by="com")
pieinfo$percentage=pieinfo$countn/pieinfo$total*100
pieinfo$Status="Not Parallel"
pieinfo[pieinfo$parallel==1&!is.na(pieinfo$parallel),]$Status="Parallel"
pieinfo[is.na(pieinfo$parallel),]$Status="Not Solved"
#pieinfo[pieinfo$solved==0,]$solved="Not Solved"                                                                                                               
piplot=ggplot(pieinfo, aes(x="", y=percentage, fill=Status))+  geom_bar(width = 1, stat="identity")+  coord_polar("y",direction=-1) + facet_wrap(~com)+ theme(text = element_text(size=20))
                                        #+ geom_text(aes(y = c(0,cumsum(percentage)[-length(percentage)]), label = percentage), size=5)                        
ggsave(filename=paste0(gpath,"PropertiesAnalysis/piechartbycomparallel",".",typu), device=typu, width=12,height=7.25, plot=piplot)

preinf=ddply(.data=testres[testres$solved==1&!is.na(testres$graph),],c("graph","com","dom","gn"), summarise, countn=length(solved))
ppieinfo=ddply(.data=preinf,c("graph", "com"), summarise, countn=length(gn))
totinf=ddply(.data=ppieinfo,c("com"), summarise, total=sum(countn))
pieinfo=merge(ppieinfo,totinf, by="com")
pieinfo$percentage=pieinfo$countn/pieinfo$total*100
pieinfo$Status="No Graph"
pieinfo[pieinfo$graph==1&!is.na(pieinfo$graph),]$Status="Graph"
#pieinfo[is.na(pieinfo$parallel),]$Status="Not Solved"
#pieinfo[pieinfo$solved==0,]$solved="Not Solved"                                                                                                                                                                                                                              
piplot=ggplot(pieinfo, aes(x="", y=percentage, fill=Status))+  geom_bar(width = 1, stat="identity")+  coord_polar("y",direction=-1) + facet_wrap(~com)+ theme(text = element_text(size=20))
                                        #+ geom_text(aes(y = c(0,cumsum(percentage)[-length(percentage)]), label = percentage), size=5)                                                                                                                                       
ggsave(filename=paste0(gpath,"PropertiesAnalysis/piechartbycomgraph",".",typu), device=typu, width=12,height=7.25, plot=piplot)

 
createCorrelationImages <- function(testres, ptype, type,p){
    tin=testres[testres$graph==1&(testres$type==ptype|is.na(testres$type))&testres$Y==strtrim(type,1)&testres$M==p,c("Class", "min", "max",  "mean", "sd", "kurt", "skew")]
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
        imprimirini(typ=typu,name=paste0("PropertiesAnalysis/",ptype,type,p,"Classification",meth),12,7.25)
        corrplot(cor(tin, method=meth, use="complete.obs")[1:(rco-1),rco:nco], method="number", p.mat=cor.mtest(tin, method=meth, use="complete.obs")$p[1:(rco-1),rco:nco], title=paste(type,p), mar=c(0,0,2,0))
        imprimirfin()
    }
}

getgids<- function(info){
    gids=integer()
    for(lv in levels(info$Class)){
        tinf=info[info$Class==lv,]
        if(as.numeric(strtrim(lv,1))==1|as.numeric(strtrim(lv,1))==3 ){
            tgids=tinf$gid
        }else{
            tv=dim(tinf)[1]/10 
            selinsta=ifelse(tv>5,5,round(tv))
            tgids=sample(tinf$gid,selinsta)
        }
        gids<-union(gids,tgids)
    }
    return(gids)
}

exploregraphold<-function(ginfo){
    graphdistvalues=data.frame(dpcon$find(query=paste0('{"gid":{"$oid":"',ginfo$gid,'"}}')))
    graphdistvalues[is.nan(graphdistvalues$PDE),]$PDE=0
    
    #graphdistvalues$LPOE=round(graphdistvalues$POE+0.5, 0)
    graphdistvaluesbyl=ddply(.data=graphdistvalues, c("gid","T","Y"), summarise, TNodesL=length(gid) )
    graphdistvaluesbyppoe=ddply(.data=graphdistvalues, c("gid","T","Y","POE"), summarise, TNodes=length(POE) )
    graphdistvaluesbyppde=ddply(.data=graphdistvalues, c("gid","T","Y","PDE"), summarise, TNodes=length(PDE) )
    graphdistvaluesbyppme=ddply(.data=graphdistvalues, c("gid","T","Y","PME"), summarise, TNodes=length(PME) )
                                        #graphdistvaluesbylgp=merge(graphdistvaluesbyppoe,graphdistvaluesbyppde, c("gid","T","Y"))
                                        #graphdistvaluesbylgp=merge(graphdistvaluesbylgp,graphdistvaluesbyppme, c("gid","T","Y"))
                                        #head(graphdistvaluesbylgp)
    graphdistvaluesbylgpoe=merge(graphdistvaluesbyppoe,graphdistvaluesbyl, by=c("gid","T","Y"))
    graphdistvaluesbylgpde=merge(graphdistvaluesbyppde,graphdistvaluesbyl, by=c("gid","T","Y"))
    graphdistvaluesbylgpme=merge(graphdistvaluesbyppme,graphdistvaluesbyl, by=c("gid","T","Y"))
    
                                        #head(graphdistvaluesbylgpde)
    graphdistvaluesbylgpoe$PNodesbyL=graphdistvaluesbylgpoe$TNodes/graphdistvaluesbylgpoe$TNodesL*100
    graphdistvaluesbylgpde$PNodesbyL=graphdistvaluesbylgpde$TNodes/graphdistvaluesbylgpde$TNodesL*100
    graphdistvaluesbylgpme$PNodesbyL=graphdistvaluesbylgpme$TNodes/graphdistvaluesbylgpme$TNodesL*100
    
                                        #poeplot=ggplot(data=graphdistvaluesbylgpoe, aes(x=LPOE, y=PNodesbyL, fill=factor(Y)))+ scale_y_sqrt() + geom_bar(stat = "identity")+ facet_wrap(Y~T,nrow=2) + ggtitle(paste(ginfo$gn,ginfo$Class)) + theme(strip.text.x = element_blank()) + guides(fill=FALSE)
                                        #pdeplot= ggplot(data=graphdistvaluesbylgpde, aes(x=LPDE, y=PNodesbyL, fill=factor(Y))) + geom_bar(stat = "identity")+ facet_wrap(Y~T,nrow=2) + theme(strip.text.x = element_blank()) + guides(fill=FALSE)+ scale_y_sqrt()
                                        #pmeplot= ggplot(data=graphdistvaluesbylgpme, aes(x=LPME, y=PNodesbyL, fill=factor(Y))) + geom_bar(stat = "identity")+ facet_wrap(Y~T,nrow=2) + theme(strip.text.x = element_blank()) + guides(fill=FALSE)+ scale_y_sqrt()
    
    poeplot= qplot(POE, data=graphdistvalues, geom='histogram', fill=factor(Y))+ facet_wrap(Y~T,nrow=2)+ theme(strip.text.x = element_blank())+ guides(fill=FALSE)+ ggtitle(paste(ginfo$gn,ginfo$Class, ptype, ginfo$com))
    pdeplot= qplot(PDE, data=graphdistvalues, geom='histogram', fill=factor(Y))+ facet_wrap(Y~T,nrow=2)+ theme(strip.text.x = element_blank())+ guides(fill=FALSE)
    pmeplot=qplot(PME, data=graphdistvalues, geom='histogram', fill=factor(Y))+ facet_wrap(Y~T,nrow=2)+ theme(strip.text.x = element_blank())+ guides(fill=FALSE)
    
    graphdistvaluesbyl=ddply(.data=graphdistvalues,c ("gid","T","Y"), summarise, TNodes=length(PDE))
    lvlplot=qplot(x=T,y=TNodes, data=graphdistvaluesbyl, fill=factor(Y))+geom_area()
    
    imprimirini(typ=typu,name=paste0("PropertiesAnalysis/hist",ptype,type,ginfo$Class,ginfo$gn),12,7.25)
    grid.arrange(poeplot, pdeplot, pmeplot,lvlplot, ncol=1)
    imprimirfin()
   # ggsave(plot=poeplot,filename=paste0(gpath,"PropertiesAnalysis/POEplot",ptype,type,ginfo$Class,ginfo$gn,".",typu), device=typu, width=12,height=7.25)
    #ggsave(plot=pdeplot,filename=paste0(gpath,"PropertiesAnalysis/PDEplot",ptype,type,ginfo$Class,ginfo$gn,".",typu), device=typu, width=12,height=7.25)
    #ggsave(plot=pmeplot,filename=paste0(gpath,"PropertiesAnalysis/PMEplot",ptype,type,ginfo$Class,ginfo$gn,".",typu), device=typu, width=12,height=7.25)
    #ggsave(plot=lvlplot,filename=paste0(gpath,"PropertiesAnalysis/Lvlplot",ptype,type,ginfo$Class,ginfo$gn,".",typu), device=typu, width=12,height=7.25)
}

exploregraph<-function(ginfo){
    graphdistvalues=data.frame(dpcon$find(query=paste0('{"gid":{"$oid":"',ginfo$gid,'"}}')))
    graphredvalues=graphdistvalues[,c("gid" , "T",  "Y", "TN" )]
    joinlist=function(list, met,df){
        cn=colnames(df)
        for(rn in 1:length(list)){
            tdf=cbind(met[rn,],list[[rn]])
            df=rbind(tdf,df)
        }
        colnames(df)=cn
        return(df)
    }
    
    vdf=joinlist(list=graphdistvalues$PPME,met=cbind(graphredvalues,M="PME"),df=data.frame(gid=character(),T=numeric(),Y=numeric(),TN=numeric(),M=character(),MP=numeric(), NP=numeric()))
    vdf=rbind(vdf,joinlist(list=graphdistvalues$PPDE,met=cbind(graphredvalues,M="PDE"),df=data.frame(gid=character(),T=numeric(),Y=numeric(),TN=numeric(),M=character(),MP=numeric(), NP=numeric())))
    vdf=rbind(vdf,joinlist(list=graphdistvalues$PPOE,met=cbind(graphredvalues,M="POE"),df=data.frame(gid=character(),T=numeric(),Y=numeric(),TN=numeric(),M=character(),MP=numeric(), NP=numeric())))
    vdf$MV=vdf$MP*vdf$NP*0.01
    mdf=data.frame(gid=character(),Y=numeric(),M=character(),MV=numeric())
    for(t in  levels(as.factor(graphdistvalues$Y))){
        for(m in levels(ginfo$M)){
            fvdf=vdf[vdf$T==max(vdf$T)&vdf$Y==t&vdf$M==m,]
           # print(fvdf[which.max(fvdf$MV),c("gid" , "Y",  "M", "MV" )])
            mdf=rbind(mdf,fvdf[which.max(fvdf$MV),c("gid" , "Y",  "M", "MV" )])
        }
    }
    graphdistvaluesbyg=ddply(.data=graphredvalues,c ("gid"), summarise, TNodes=sum(TN), T=max(T))
    mdf$Class=ginfo$Class
    mdf$TN=graphdistvaluesbyg$TNodes
    mdf$T=graphdistvaluesbyg$T
    
       
    poeplot=ggplot(data=vdf[vdf$M=="POE",], aes(x=ceiling(MP), y=NP, fill=factor(Y))) +geom_bar(stat="identity")+ facet_wrap(Y~T,nrow=2)+ theme(strip.text.x = element_blank()) + guides(fill=FALSE)+ggtitle(paste(ginfo$gn,ginfo$Class, ptype, ginfo$com))+ labs(x = "", y="")#+scale_y_sqrt()
    pdeplot=ggplot(data=vdf[vdf$M=="PDE",], aes(x=ceiling(MP), y=NP, fill=factor(Y))) +geom_bar(stat="identity")+ facet_wrap(Y~T,nrow=2)+ theme(strip.text.x = element_blank()) + guides(fill=FALSE)+ labs(x = "", y="Percentage of total nodes in level") #+ scale_y_sqrt()
    pmeplot=ggplot(data=vdf[vdf$M=="PME",], aes(x=ceiling(MP), y=NP, fill=factor(Y))) +geom_bar(stat="identity")+ facet_wrap(Y~T,nrow=2)+ theme(strip.text.x = element_blank()) + guides(fill=FALSE)+ labs(x = "Percentage of mutex nodes", y="")#+ scale_y_sqrt()    
    
    graphdistvaluesbyl=ddply(.data=graphredvalues,c ("gid","T","Y"), summarise, TNodes=sum(TN))
    lvlplot=qplot(x=T,y=TNodes, data=graphdistvaluesbyl, fill=factor(Y))+geom_area()+labs(x="Levels", y="Nodes", fill="Type")
    
    imprimirini(typ=typu,name=paste0("PropertiesAnalysis/hist",ptype,type,ginfo$Class,ginfo$gn),12,7.25)
    grid.arrange(poeplot, pdeplot, pmeplot,lvlplot, ncol=1)
    imprimirfin()
    
    ggsave(plot=poeplot,filename=paste0(gpath,"PropertiesAnalysis/POEplot",ptype,type,ginfo$Class,ginfo$gn,".",typu), device=typu, width=12,height=7.25)
    ggsave(plot=pdeplot,filename=paste0(gpath,"PropertiesAnalysis/PDEplot",ptype,type,ginfo$Class,ginfo$gn,".",typu), device=typu, width=12,height=7.25)
    ggsave(plot=pmeplot,filename=paste0(gpath,"PropertiesAnalysis/PMEplot",ptype,type,ginfo$Class,ginfo$gn,".",typu), device=typu, width=12,height=7.25)
    ggsave(plot=lvlplot,filename=paste0(gpath,"PropertiesAnalysis/Lvlplot",ptype,type,ginfo$Class,ginfo$gn,".",typu), device=typu, width=12,height=7.25)
}


compareClassAndMeasure<- function(pinfo, s){
                                        #imprimirini(typ=typu,name=paste0("PropertiesAnalysis/",ptype,type,s,p),12,7.25)
                                        #boxplot(info[,paste0(s,p)]~info[,"Class"], ylab=paste0(s,p),xlab="Class")
    linM=lm(pinfo[,s]~pinfo[,"Class"])
    residuales<-resid(linM)
    sht=shapiro.test(residuales)
                                        #qqnorm(residuales,col=rgb(0,1,0,0.5))
                                        #qqline(residuales,col="red")
    if(sht$p.value>0.05){                   
        rno="residuals are from a normal dist"                    
        anov=aov(pinfo[,s]~pinfo[,"Class"])
        if(summary.aov(anov)[[1]][["Pr(>F)"]][1]<0.05){
            me="means are different"
            TukeyHSD(anov)
        }else{
            me="means are equal"
        }
    }else{          
        rno="residuals not normal"   
        krus=kruskal.test(pinfo[,s]~as.factor(pinfo[,"Class"]))
        if(krus$p.value<0.05){
            me="means are different"
            dn=dunn.test(x=pinfo[,s], g=pinfo[,"Class"])
        }else{
            me="means are equal"
        }
    }
                                        #print(paste0(rno,", ",me))
                                        #print(dn$comparisons[dn$P.adjusted<0.005])
    collaps="\n"
    dr=paste(dn$comparisons[dn$P.adjusted<0.05],collapse=collaps)
    labrc=c(rno,me,"for: ", dr )
    labr=paste(labrc,collapse=collaps)
    pg=ggplot(data = pinfo, aes(x=factor(Class), y=pinfo[,s]), log="y") + geom_violin(fill="orange", color="red") + scale_y_sqrt()  + theme(text = element_text(size=15), plot.margin = unit(c(1, 12.5, 1.5, 2), "lines") )+labs(x="Difficulty Set", y=paste0(s))+ geom_boxplot(width=0.03, fill="blue", color="white")#+ggtitle(paste(ptype,type,s)) #+ annotation_custom(grob = textGrob(labr), xmin = 6, xmax = 7) #, ymin = round(max(pinfo[,s], na.rm=T),2)*.9, ymax = round(max(pinfo[,s], na.rm=T),2)*.8
    
    
    #gt <- ggplot_gtable(ggplot_build(pg))
    #gt$layout$clip[gt$layout$name=="panel"] <- "off"
    #grid.draw(gt)
                                        #+facet_wrap(~dom)
                                        #imprimirfin()
    ggsave(filename=gsub(" ","",paste0(gpath,"PropertiesAnalysis/Boxplot",ptype,type,s,".",typu)), device=typu, width=12,height=7.25, plot=pg)
}



statsData= function(datares){
    preinf=ddply(.data=datares,c("Class","com","dom","gn"), summarise, countn=length(Class))
    ppieinfo=ddply(.data=preinf,c("Class", "com"), summarise, countn=length(gn))
    totinf=ddply(.data=ppieinfo,c("com"), summarise, total=sum(countn))
    pieinfo=merge(ppieinfo,totinf, by="com")
    pieinfo$percentage=pieinfo$countn/pieinfo$total*100
    piplot=ggplot(pieinfo, aes(x="", y=percentage, fill=Class))+  geom_bar(width = 1, stat="identity")+  coord_polar("y",direction=-1) + facet_wrap(~com)+ theme(text = element_text(size=20))
                                        #+ geom_text(aes(y = c(0,cumsum(percentage)[-length(percentage)]), label = percentage), size=5)  
    ggsave(filename=paste0(gpath,"PropertiesAnalysis/piechartbycom",".",typu), device=typu, width=12,height=7.25, plot=piplot)

}

Correlate=function(ninfo){
    tin=ninfo[,c("Class",measuresNames)]
    colnames(tin)
    tin$Easy=tin$Class=="1 Easy"
    tin$Fit=tin$Class=="2 Fit"
    tin$Hard=tin$Class=="3 Hard"
    tin$NoClass=tin$Class=="4 Not Class"
    tin$NoSolv=tin$Class=="5 Not Solv"
    tin$Class=NULL
    nco=dim(tin)[2]
    rco=nco-4
    methl=c("spearman")#,"kendall")
    for(meth in methl){
       colnames( tin)=c(measuresNames,"Easy"," Fit","Hard","Not classified","Not solved")
        cormat=cor(tin, method=meth, use="complete.obs")[1:(rco-1),rco:nco]
        cormatp=cor.mtest(tin, method=meth, use="complete.obs")$p[1:(rco-1),rco:nco]
        imprimirini(typ=typu,name=paste0("PropertiesAnalysis/",ptype,type,"Classification",meth),12,7.25)
        corrplot(cormat, method="number", p.mat=cormatp, mar=c(0,0,2,0))
        imprimirfin()
    }    
}

#head(testres)
                                        #npcon= mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="nodePercentageEdges")
mpcon= mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="graphMetric")
ptypel=c("Parallel","NoParallel")
typel=c("facts","actions")
propl=levels(testres$M) #c("PDE","POE","PME")
sdisl=c("mean","max","sd","kurt","skew")
explore=FALSE
compare=TRUE


if(explore){
    info=testres[complete.cases(testres$Class),]
    if(dim(info)[1]>0){
        gids=getgids(info)
        for(g in gids){
            ginfo=head(info[info$gid==g,],1)
            exploregraph(ginfo)              
        }
    }
}

if(compare){
    binfo=unique( testres[,c("com","dom","gn","gid", "minSteps" ,"solved", "graph","fap","parallel","type","Class","pn"  )])
    minfo=data.frame(mpcon$find())
    #minfo[is.infinite( minfo$minSteps),]$minSteps=NA
    minfo$PG=minfo$TNL/minfo$TNF
    minfo$RG=(minfo$TNL-minfo$TNF)/minfo$MT
    #minfo$GG=atan(minfo$RG)*(2/pi)
    #minfo$MVPOE=minfo$MPOE$p*(minfo$MPOE$pc*0.01)
    #minfo$MVPDE=minfo$MPDE$p*(minfo$MPDE$pc*0.01)
    #minfo$MVPME=minfo$MPME$p*(minfo$MPME$pc*0.01)
    #minfo$MVPOE=(minfo$MPOE$p+minfo$MPOE$pc)*0.5
    #minfo$MVPDE=(minfo$MPDE$p+minfo$MPDE$pc)*0.5
    #minfo$MVPME=(minfo$MPME$p+minfo$MPME$pc)*0.5
    minfo[,"POE mode"]=minfo$MPOE$p
    minfo[,"PDE mode"]=minfo$MPDE$p
    minfo[,"PME mode"]=minfo$MPME$p
    minfo[,"Node percentage of POE equal mode"]=minfo$MPOE$pc
    minfo[,"Node percentage of PDE equal mode"]=minfo$MPDE$pc
    minfo[,"Node percentage of PME equal mode"]=minfo$MPME$pc
    ninfo=merge(minfo,binfo,all.x=TRUE, by="gid")
    classificationNames= levels(ninfo$Class)
    classificationLabels= c("Easy", "Fit","Hard", "Not classified", "Not solved", "Not solved no graph", "Not processed", "Not processed no graph")
    measures=c("TN","TNL","PG","RG","POE mode","PDE mode","PME mode","Node percentage of POE equal mode","Node percentage of PDE equal mode","Node percentage of PME equal mode")
     measuresNames=c("Total nodes","Nodes in last level","Growth percentage","Growth rate","POE mode","PDE mode","PME mode","Node percentage of POE equal mode","Node percentage of PDE equal mode","Node percentage of PME equal mode")
    for(ptype in ptypel){
        for(type in typel){
            tninfo=ninfo[complete.cases(ninfo$Class)&(ninfo$type==ptype|is.na(ninfo$type))&ninfo$com!="IPC2002"&ninfo$Y==strtrim(type,1)&ninfo$Class!="6 Not Proc"&ninfo$Class!="6 Not Processed NG",]
            #levels(tninfo)=classificationLabels
            for(i in 1: length(measures)){
                names(tninfo)[names(tninfo) == measures[i]] <- measuresNames[i]
            }
            Correlate(ninfo=tninfo)
            for(meas in measuresNames){
                print(paste(meas,type, ptype))
                levels(tninfo$Class)=classificationLabels
                compareClassAndMeasure(pinfo=tninfo,s=meas)
            }
       }     
    }
}
