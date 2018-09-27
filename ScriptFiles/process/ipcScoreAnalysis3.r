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

exploregraph<-function(ginfo){
    graphdistvalues=data.frame(npcon$find(query=paste0('{"gid":{"$oid":"',ginfo$gid,'"}}')))
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
    
                                        #head(graphdistvaluesbylgpde)
    graphdistvaluesbylgpoe$PNodesbyL=graphdistvaluesbylgpoe$TNodes/graphdistvaluesbylgpoe$TNodesL*100
    graphdistvaluesbylgpde$PNodesbyL=graphdistvaluesbylgpde$TNodes/graphdistvaluesbylgpde$TNodesL*100
    graphdistvaluesbylgpme$PNodesbyL=graphdistvaluesbylgpme$TNodes/graphdistvaluesbylgpme$TNodesL*100
    
                                        #poeplot=ggplot(data=graphdistvaluesbylgpoe, aes(x=LPOE, y=PNodesbyL, fill=factor(Y)))+ scale_y_sqrt() + geom_bar(stat = "identity")+ facet_wrap(Y~T,nrow=2) + ggtitle(paste(ginfo$gn,ginfo$Class)) + theme(strip.text.x = element_blank()) + guides(fill=FALSE)
                                        #pdeplot= ggplot(data=graphdistvaluesbylgpde, aes(x=LPDE, y=PNodesbyL, fill=factor(Y))) + geom_bar(stat = "identity")+ facet_wrap(Y~T,nrow=2) + theme(strip.text.x = element_blank()) + guides(fill=FALSE)+ scale_y_sqrt()
                                        #pmeplot= ggplot(data=graphdistvaluesbylgpme, aes(x=LPME, y=PNodesbyL, fill=factor(Y))) + geom_bar(stat = "identity")+ facet_wrap(Y~T,nrow=2) + theme(strip.text.x = element_blank()) + guides(fill=FALSE)+ scale_y_sqrt()
    
    poeplot= qplot(POE, data=graphdistvalues, geom='histogram', fill=factor(Y))+ facet_wrap(Y~T,nrow=2)+ theme(strip.text.x = element_blank())+ guides(fill=FALSE)
    pdeplot= qplot(PDE, data=graphdistvalues, geom='histogram', fill=factor(Y))+ facet_wrap(Y~T,nrow=2)+ theme(strip.text.x = element_blank())+ guides(fill=FALSE)
    pmeplot=qplot(PME, data=graphdistvalues, geom='histogram', fill=factor(Y))+ facet_wrap(Y~T,nrow=2)+ theme(strip.text.x = element_blank())+ guides(fill=FALSE)
    
    graphdistvaluesbyl=ddply(.data=graphdistvalues,c ("gid","T","Y"), summarise, TNodes=length(PDE))
    lvlplot=qplot(x=T,y=TNodes, data=graphdistvaluesbyl, fill=factor(Y))+geom_area()
    
    imprimirini(typ=typu,name=paste0("PropertiesAnalysis/hist",ptype,type,ginfo$Class,ginfo$gn),12,7.25)
    grid.arrange(poeplot, pdeplot, pmeplot,lvlplot, ncol=1)
    imprimirfin()
    ggsave(plot=poeplot,filename=paste0(gpath,"PropertiesAnalysis/POEplot",ptype,type,ginfo$Class,ginfo$gn,".",typu), device=typu, width=12,height=7.25)
    ggsave(plot=pdeplot,filename=paste0(gpath,"PropertiesAnalysis/PDEplot",ptype,type,ginfo$Class,ginfo$gn,".",typu), device=typu, width=12,height=7.25)
    ggsave(plot=pmeplot,filename=paste0(gpath,"PropertiesAnalysis/PMEplot",ptype,type,ginfo$Class,ginfo$gn,".",typu), device=typu, width=12,height=7.25)
    ggsave(plot=lvlplot,filename=paste0(gpath,"PropertiesAnalysis/Lvlplot",ptype,type,ginfo$Class,ginfo$gn,".",typu), device=typu, width=12,height=7.25)
}


compareClassAndMeasure<- function(info,p, s){
                                        #imprimirini(typ=typu,name=paste0("PropertiesAnalysis/",ptype,type,s,p),12,7.25)
                                        #boxplot(info[,paste0(s,p)]~info[,"Class"], ylab=paste0(s,p),xlab="Class")
    pinfo=info[info$M==p,]
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
    dr=paste(dn$comparisons[dn$P.adjusted<0.005],collapse=collaps)
    labrc=c(rno,me,"for: ", dr )
    labr=paste(labrc,collapse=collaps)
    pg=ggplot(data = pinfo, aes(x=factor(Class), y=pinfo[,s]), log="y") + geom_violin(fill="orange", color="red")   + theme(text = element_text(size=15), plot.margin = unit(c(1, 12.5, 1.5, 2), "lines") )+labs(x="Difficulty Set", y=paste0(s))+ geom_boxplot(width=0.03, fill="blue", color="white")+ggtitle(paste(ptype,type,p)) + annotation_custom(grob = textGrob(labr), xmin = 7, xmax = 8, ymin = round(max(pinfo[,s], na.rm=T),2)*.9, ymax = round(max(pinfo[,s], na.rm=T),2)*.8)
    
    
    gt <- ggplot_gtable(ggplot_build(pg))
    gt$layout$clip[gt$layout$name=="panel"] <- "off"
    grid.draw(gt)
                                        #+facet_wrap(~dom)
                                        #imprimirfin()
    ggsave(filename=paste0(gpath,"PropertiesAnalysis/Boxplot",ptype,type,s,p,".",typu), device=typu, width=12,height=7.25, plot=gt)
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

head(testres)
npcon= mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="nodePercentageEdges")
ptypel=c("Parallel","NoParallel")
typel=c("facts","actions")
propl=c("PDE","POE","PME")
sdisl=c("mean","max","sd","kurt","skew")
explore=FALSE
compare=TRUE
for(ptype in ptypel){
    for(type in typel){
        info=testres[testres$graph==1&testres$Y==strtrim(type,1)&(testres$type==ptype|is.na(testres$type)),]
        if(dim(info)[1]>0){
            if(explore){
                gids=getgids(info)
                for(g in gids){
                    ginfo=info[info$gid==g,]
                    exploregraph(ginfo)              
                }
            }
            
            for(p in propl){
                createCorrelationImages(testres,ptype,type,p)
                for(s in sdisl){
                    compareClassAndMeasure(info,p,s)
                }
            }
        }
    }
}
