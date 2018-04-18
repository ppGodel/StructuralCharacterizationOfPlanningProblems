library('mongolite')
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


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

reg.conf.intervals <- function(x, y, prnt=TRUE) {
  n <- length(y) # Find length of y to use as sample size
  lmmodel <- lm(y ~ x) # Fit linear model
#  lines(lm.model, col="red")
  
  # Extract fitted coefficients from model object
  b0 <- lmmodel$coefficients[1]
  b1 <- lmmodel$coefficients[2]
  
  # Find SSE and MSE
#  sse <- sum((y - lmmodel$fitted.values)^2)
#  mse <- sse / (n - 2)
  
  t.val <- qt(0.975, n - 2) # Calculate critical t-value
  
  # Fit linear model with extracted coefficients
  #x_new <- seq(min(x),max(x), length.out=length(x))
  y.fit <- b1 * x + b0
  
  # Find the standard error of the regression line
  se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
  
  # Fit a new linear model that extends past the given data points (for plotting)
  x_int <- max(x)-min(x)
  #print(paste("minx:",min(x),"maxx:",max(x),"x_int:",x_int,"intx:", intx))
  x_new2 <- unique(c(x-x_int,x,x+x_int)) #round(seq(min(x)-x_int,max(x)+x_int, 10**intx),abs(intx))
  #x_new2 <- 0:max(x + 100)
  y.fit2 <- b1 * x_new2 + b0
  
  # Warnings of mismatched lengths are suppressed
  slope.upper <- suppressWarnings(y.fit2 + t.val * se)
  slope.lower <- suppressWarnings(y.fit2 - t.val * se)
  
  # Collect the computed confidence bands into a data.frame and name the colums
  bands <- data.frame(cbind(x_new2,slope.lower, y.fit2, slope.upper))
  colnames(bands) <- c('xval','LowBand','regval', 'UpperBand')
  if(prnt){
  #Plot the fitted linear regression line and the computed confidence bands
  #plot(x, y, cex = 1.75, pch = 21, bg = 'gray')
      xpol=c(x_new2,rev(x_new2))
      ypol=c(slope.lower, rev(slope.upper))
      polygon(x=xpol, y=ypol, border=NA, col="antiquewhite2")
      lines(x=x_new2, y=y.fit2, col = 'black', lwd = 2)
      lines(x=x_new2, y=slope.upper, col = 'brown', lty = 2, lwd = 2)
      lines(x=x_new2, y=slope.lower, col = 'brown', lty = 2, lwd = 2)
  }
  return(bands)
}

choose.lm <-function(px,py){
    inter=c(seq(-10,-2,1),seq(-1,1,0.1),seq(2,10,1))
    result=data.frame(tx=numeric(),ty=numeric(),r2=numeric(),m=numeric(),b=numeric(),u=numeric())
    for(i in inter){
        for(j in inter){
            x=tukeyLadder(px,i)
            y=tukeyLadder(py,j)
            #print(paste(i,j))
            #print(cbind(x,y))
            lmmodel <- lm(y ~ x)
            n=length(y)
            t.val <- qt(0.975, n - 2)
            m=lmmodel$coefficients[2]
            b=lmmodel$coefficients[1]
            #x_int <- max(x)-min(x)
            #x_new2 <- unique(c(x-x_int,x,x+x_int))
            y.fit <- m * x + b
            se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
            result=rbind(result,c(tx=i,ty=j,r2=summary(lmmodel)$r.squared,m=m, b=b, u=t.val*se))
        }
    }
    colnames(result)=c("tx","ty","r2","m","b","u")
    return( result[which.max(result$r2),] )
}

tukeyLadder = function(x, q = NULL) {
    if (is.null(q)) {
        return(x)
    }
    if (q == 0) {
        x.new = log(x+1)
    } else {
        if (q < 0) {
            x.new = (x+0.0000001)^q
        } else {
            x.new = x^q
        }
    }
    return(x.new)
}

pointlinedist<-function(m,b,x,y){
    dis=abs(-m*x+y-b)/sqrt(m**2+1)
    return(dis)
}

allplanners = function(nx,ny, data, prnt=TRUE){
    pchs=c(1:15,17,18,20,22,26,27)
    apl=unique(data$planner)
    labls=unique(apply(data[,c("com","dom")],1, function(item){paste0(item["com"],'-',item["dom"])}))
    cl= unique(data$com)
    colores=rainbow(length(labls))
    data$MaxDist=0
    
    for(p in 1:length(apl)){
        auxre=data[data$planner==apl[p],]
        #labls=unique(apply(auxres[,c("com","dom")],1, function(item){paste0(item["com"],'-',item["dom"])}))
        cl= unique(auxre$com)
        #colores=rainbow(length(labls))
        for(c in 1:length(cl)){
            auxres=auxre[auxre$com==cl[c],]
            dlaux=c()
            dcaux=c()
            fitval=choose.lm(px=auxres[,nx],py=auxres[,ny])
            tranx=tukeyLadder(auxres[,nx],fitval$tx)
            trany=tukeyLadder(auxres[,ny],fitval$ty)
            lx=nx
            ly=ny
            if(fitval$tx!=1){
                lx=paste0(nx,"^",fitval$tx)  
            }
            if(fitval$ty!=1){
                ly=paste0(ny,"^",fitval$ty)  
            }
            
            if(fitval$tx==0){
                lx=paste0('log(',nx,")")  
            }
            if(fitval$ty==0){
                ly=paste0('log(',ny,")")  
            }
            maxVx= max(tranx)
            minVx= min(tranx)
            maxVy= max(trany)
            minVy= min(trany)
            
            ranx=maxVx-minVx
            rany=maxVy-minVy
            dpx=log(ranx,10)
            dpy=log(rany,10)
            if(dpx>1.6&&dpx<4){
                dpx=0
            }else{
                dpx=floor(log(ranx,10))-1
            }
            if(dpy>1.6){
                dpy=0
            }else{
                dpy=floor(log(rany,10))-1
            }
            if(prnt){
                imprimirini(typ=typu,name=paste0("Layers/",cl[c],"_", apl[p],"_",ny,"vs",nx),12,7.25)
                par(mar=c(5,5,3,9),xpd=FALSE)
                plot(0,type='n', xlim=c(minVx,maxVx), ylim=c(minVy,maxVy), xlab=lx, ylab=ly, main=paste("Com:", cl[c],"Planner:", apl[p],"R^2:",round(fitval$r2*100,2)) )
            }
                                        #alm=lm(log(Time+1)~log(TE),data=auxres)
                                        #abline(alm)
            aval=auxres[,c(nx, ny,"gn","com","dom","planner")]
            aval=cbind(rn=rownames(aval),aval)
            aval$xval= tranx
            aval$yval= trany
            aval$lab=0
            #auxdist=mean(bands$regval-bands$LowBand)
            #aval$dist=apply(X=aval, MARGIN=1, FUN=function(item){
            #    dtl=pointlinedist(m=fitval$m,b=fitval$b, x=as.numeric(item["xval"]),y=as.numeric(item["yval"]))/auxdist#(fitval$u)
                #if(dtl>-0.01&dtl<0){dtl=round(dtl)}
                #if(dtl<0){dtl=0}
            #    return(dtl)
            #})
            bands= reg.conf.intervals(x=aval$xval, y=aval$yval, prnt)
            plbls= merge(x=aval, y=bands, by=c("xval"), all.x=TRUE)
            plbls=plbls[order(plbls$rn),]
            plbls$dwn=plbls$yval<plbls$LowBand
            plbls$upp=plbls$yval>plbls$UpperBand
            plbls$dist=0
            if(sum(plbls$dwn)>0){
                plbls[plbls$dwn,]$lab=1
                plbls[plbls$dwn,]$dist=plbls[plbls$dwn,]$LowBand-plbls[plbls$dwn,]$yval
            }
            if(sum(plbls$upp)>0){
                plbls[plbls$upp,]$lab=2
                plbls[plbls$upp,]$dist=plbls[plbls$upp,]$yval-plbls[plbls$upp,]$UpperBand
            }
            data[rownames(data[data$planner==apl[p]&data$com==cl[c],]),]$Class= plbls$lab
            data[data$planner==apl[p]&data$com==cl[c],]$R2= rep(x=fitval$r2,times=dim(data[data$planner==apl[p]&data$com==cl[c],])[1])
            #data[data$planner==apl[p]&data$com==cl[c],]$MaxDist= rep(x=max(plbls$dist),times=dim(data[data$planner==apl[p]&data$com==cl[c],])[1])
            data[rownames(data[data$planner==apl[p]&data$com==cl[c],]),]$Dist = plbls$dist/fitval$m
        
            if(prnt){
            #for(c in 1:length(cl)){
                ares=aval[auxres$com==cl[c],]
                dl=unique(ares$dom)
                for(d in 1:length(dl)){
                    an=paste0(cl[c],'-',dl[d])
                    ni=match(an,labls)
                    dcaux=c(ni,dcaux)
                    dlaux=c(an,dlaux)
                    points(x=ares[ares$dom==dl[d],"xval"], y=ares[ares$dom==dl[d],"yval"], col=colores[ni], pch=19)
                }
            #}        
                par(xpd=TRUE)
                if(sum(plbls$dwn)>0||sum(plbls$upp)>0){
                    text(x=plbls[plbls$lab>0,"xval"],y=(plbls[plbls$lab>0,"yval"]+ifelse(dpy==0,0.25,0.25*10**dpy)), labels=substrRight(plbls[plbls$lab>0,]$gn,5) )
                }
        
                                        #    legend(maxTN*1.06, log(maxT)*0.6, legend=apl, pch=pchs)
                legend(maxVx*1.06, maxVy*1, legend=dlaux,col=colores[dcaux], pch=19)
                imprimirfin()
            }
        }
    }
    #cfactor=paste0('C',nx,'v',ny)
    #names(data)[names(data) == 'Class'] <- cfactor
    data$Cfactor=nx#paste0('C',nx),'v',ny)
    data$Cresp=ny
    data$Ctran=paste0(lx,'v',ly)
    
    return(data[,c("com","planner","dom","gn","Class","Cfactor","Cresp","Ctran","R2","Dist")]) 
}


allplannersbydom = function(nx,ny, data, prnt=TRUE){
    pchs=c(1:15,17,18,20,22,26,27)
    apl=unique(data$planner)
    labls=unique(apply(data[,c("com","dom")],1, function(item){paste0(item["com"],'-',item["dom"])}))
    cl= unique(data$com)
    colores=rainbow(length(labls))
    #data$MaxDist=0
    
    for(p in 1:length(apl)){
        auxre=data[data$planner==apl[p],]
        cl= unique(auxre$com)
        for(c in 1:length(cl)){
            auxred=auxre[auxre$com==cl[c],]
            dl=unique(auxred$dom)
            for(d in 1:length(dl)){
                auxres=auxred[auxred$dom==dl[d],]
                dlaux=c()
                dcaux=c()
                fitval=choose.lm(px=auxres[,nx],py=auxres[,ny])
                tranx=tukeyLadder(auxres[,nx],fitval$tx)
                trany=tukeyLadder(auxres[,ny],fitval$ty)
                lx=nx
                ly=ny
                if(fitval$tx!=1){
                    lx=paste0(nx,"^",fitval$tx)  
                }
                if(fitval$ty!=1){
                    ly=paste0(ny,"^",fitval$ty)  
                }
                
                if(fitval$tx==0){
                    lx=paste0('log(',nx,")")  
                }
                if(fitval$ty==0){
                    ly=paste0('log(',ny,")")  
                }
                maxVx= max(tranx)
                minVx= min(tranx)
                maxVy= max(trany)
                minVy= min(trany)
                
                ranx=maxVx-minVx
                rany=maxVy-minVy
                dpx=log(ranx,10)
                dpy=log(rany,10)
                if(dpx>1.6&&dpx<4){
                    dpx=0
                }else{
                    dpx=floor(log(ranx,10))-1
                }
                if(dpy>1.6){
                    dpy=0
                }else{
                    dpy=floor(log(rany,10))-1
                }
                if(prnt){
                    imprimirini(typ=typu,name=paste0("Layers/",cl[c],"_", apl[p],"_", dl[d],"_",ny,"vs",nx),12,7.25)
                    par(mar=c(5,5,3,9),xpd=FALSE)
                    plot(0,type='n', xlim=c(minVx,maxVx), ylim=c(minVy,maxVy), xlab=lx, ylab=ly, main=paste("Com:", cl[c],"Planner:", apl[p], "Domain:", dl[d],"R^2:",round(fitval$r2*100,2)) )
            }
                                        #alm=lm(log(Time+1)~log(TE),data=auxres)
                                        #abline(alm)
                aval=auxres[,c(nx, ny,"gn","com","dom","planner")]
                aval=cbind(rn=rownames(aval),aval)
                aval$xval= tranx
                aval$yval= trany
                aval$lab=0
                bands= reg.conf.intervals(x=aval$xval, y=aval$yval, prnt)
                plbls= merge(x=aval, y=bands, by=c("xval"), all.x=TRUE)
                plbls=plbls[order(plbls$rn),]
                plbls$dwn=plbls$yval<plbls$LowBand
                plbls$upp=plbls$yval>plbls$UpperBand
                plbls$dist=0
                if(is.na(sum(plbls$dwn))&&sum(plbls$dwn)>0){
                    plbls[plbls$dwn,]$lab=1
                    plbls[plbls$dwn,]$dist=plbls[plbls$dwn,]$LowBand-plbls[plbls$dwn,]$yval
                }
                if(is.na(sum(plbls$dwn))&&sum(plbls$upp)>0){
                    plbls[plbls$upp,]$lab=2
                    plbls[plbls$upp,]$dist=plbls[plbls$upp,]$yval-plbls[plbls$upp,]$UpperBand
                }
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$dom==dl[d],]),]$Class= plbls$lab
                data[data$planner==apl[p]&data$com==cl[c]&data$dom==dl[d],]$R2= rep(x=fitval$r2,times=dim(data[data$planner==apl[p]&data$com==cl[c]&data$dom==dl[d],])[1])
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$dom==dl[d],]),]$Dist = plbls$dist/fitval$m
        
            if(prnt){
            #for(c in 1:length(cl)){
                #ares=aval[auxres$com==cl[c],]
                #dl=unique(aval$dom)
                #for(d in 1:length(dl)){
                an=paste0(cl[c],'-',dl[d])
                ni=match(an,labls)
                dcaux=c(ni,dcaux)
                dlaux=c(an,dlaux)
                points(x=aval[aval$dom==dl[d]&aval$com==cl[c],"xval"], y=aval[aval$dom==dl[d]&aval$com==cl[c],"yval"], col=colores[ni], pch=19)
                #}
            #}        
                par(xpd=TRUE)
                if(sum(plbls$lab>0)>0){
                    text(x=plbls[plbls$lab>0,"xval"],y=(plbls[plbls$lab>0,"yval"]+ifelse(dpy==0,0.25,0.25*10**dpy)), labels=substrRight(plbls[plbls$lab>0,]$gn,5) )
                }
        
                                        #    legend(maxTN*1.06, log(maxT)*0.6, legend=apl, pch=pchs)
                legend(maxVx*1.06, maxVy*1, legend=dlaux,col=colores[dcaux], pch=19)
                imprimirfin()
            }
            }
        }
    }
    #cfactor=paste0('C',nx,'v',ny)
    #names(data)[names(data) == 'Class'] <- cfactor
    data$Cfactor=nx#paste0('C',nx),'v',ny)
    data$Cresp=ny
    data$Ctran=paste0(lx,'v',ly)
    
    return(data[,c("com","planner","dom","gn","Class","Cfactor","Cresp","Ctran","R2","Dist")]) 
}

grcon= mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="graphIPCResComp")
compresultsraw=data.frame(grcon$find())
compresultsgraphsolved=compresultsraw[compresultsraw$solved==1&compresultsraw$graph==1,]
compresultsgraphsolved$LogTime=log(compresultsgraphsolved$Time+1)
compresultsgraphsolved$Class=0
compresultsgraphsolved$R2=0
compresultsgraphsolved$Dist=0
                           
clas1=allplannersbydom(nx="D" ,ny="Time", data=compresultsgraphsolved)
clas2=allplannersbydom(nx="DM",ny="Time", data=compresultsgraphsolved)
clas3=allplannersbydom(nx="TN",ny="Time", data=compresultsgraphsolved)
clas4=allplannersbydom(nx="TE",ny="Time", data=compresultsgraphsolved)
clas5=allplannersbydom(nx="TME",ny="Time", data=compresultsgraphsolved)

allclass=rbind(clas1,clas2,clas3,clas4,clas5)
boxplot(R2~Cresp+Cfactor, data=allclass)
ggplot(data = allclass[allclass$R2>=0.85&allclass$R2<0.95,], aes(x=factor(Cfactor), y=log(abs(Dist)+1))) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=30))#+facet_wrap(~dom)
ggsave(paste0(gpath,"Layers/","DistanceDistribution.",typu), device=typu, width=12,height=7.25)

sqv=seq(0,1,0.05)
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
ggplot(data=td, aes(x=x, y=count, fill=value) )+geom_bar(stat="identity")+theme(axis.text=element_text(size=16), axis.title=element_text(size=20, face="bold"))+labs(x = "R^2", y="Model Count", fill="Value")
ggsave(paste0(gpath,"Layers/","R2Dist.",typu), device=typu, width=12,height=7.25)

td2=aggregate(planner~Class+Cfactor, allclass[allclass$R2>=0.90,], FUN=length)#
td2$Diff=""
td2[td2$Class==0,]$Diff="1 Norm"
td2[td2$Class==1,]$Diff="2 Easy"
td2[td2$Class==2,]$Diff="3 Hard"
ggplot(data=td2, aes(x=(td2$Cfactor), y=planner, fill=as.factor(Diff)) )+geom_bar(stat="identity")+labs(x = "Property", y="Count", fill="Difficulty")
ggsave(paste0(gpath,"Layers/","InstanceDifficultyatR20.9.",typu), device=typu, width=12,height=7.25)

lr2=0.9
td3=aggregate(abs(Dist)~Class+Cfactor+gn, allclass[allclass$R2>=lr2,], FUN=sum)
names(td3)[names(td3) == 'abs(Dist)'] <- 'Dist'
dtp=log(td3[td3$Class>0,]$Dist+1)#allclass$Cfactor=="DM"&
#mdtp=1 / max(dtp)
#dtp=dtp*mdtp
imprimirini(typ=typu,name=paste0("Layers/","HistDistanceR2",lr2,"All"),12,7.25)
hist(dtp, xlab="Normalized Distance", main=paste("Histogram of Distance for R2 >",lr2))
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
