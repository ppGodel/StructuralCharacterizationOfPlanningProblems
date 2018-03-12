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

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

reg.conf.intervals <- function(x, y) {
  n <- length(y) # Find length of y to use as sample size
  lmmodel <- lm(y ~ x) # Fit linear model
#  lines(lm.model, col="red")
  
  # Extract fitted coefficients from model object
  b0 <- lmmodel$coefficients[1]
  b1 <- lmmodel$coefficients[2]
  
  # Find SSE and MSE
  sse <- sum((y - lmmodel$fitted.values)^2)
  mse <- sse / (n - 2)
  
  t.val <- qt(0.975, n - 2) # Calculate critical t-value
  
  # Fit linear model with extracted coefficients
  x_new <- seq(min(x),max(x), length.out=length(x))
  y.fit <- b1 * x_new + b0
  
  # Find the standard error of the regression line
  se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
  
  # Fit a new linear model that extends past the given data points (for plotting)
  x_int <- max(x)-min(x)
  x_new2 <- round(seq(min(x)-x_int,max(x)+x_int, 0.01),2)
  #x_new2 <- 0:max(x + 100)
  y.fit2 <- b1 * x_new2 + b0
  
  # Warnings of mismatched lengths are suppressed
  slope.upper <- suppressWarnings(y.fit2 + t.val * se)
  slope.lower <- suppressWarnings(y.fit2 - t.val * se)
  
  # Collect the computed confidence bands into a data.frame and name the colums
  bands <- data.frame(cbind(x_new2,slope.lower, y.fit2, slope.upper))
  colnames(bands) <- c('xval','LowBand','fitval', 'UpperBand')
  
  # Plot the fitted linear regression line and the computed confidence bands
  #plot(x, y, cex = 1.75, pch = 21, bg = 'gray')
  xpol=c(x_new2,rev(x_new2))
  ypol=c(slope.lower, rev(slope.upper))
  polygon(x=xpol, y=ypol, border=NA, col="antiquewhite2")
  lines(x=x_new2, y=y.fit2, col = 'black', lwd = 2)
  lines(x=x_new2, y=slope.upper, col = 'brown', lty = 2, lwd = 2)
  lines(x=x_new2, y=slope.lower, col = 'brown', lty = 2, lwd = 2)
  return(bands)
}


grcon= mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="graphIPCRes")
compresultsraw=data.frame(grcon$find())
compresultsraw$Diff=NA
pchs=c(1:15,17,18,20,22,26,27)
minTN=min(compresultsraw$TN)
minTE=min(compresultsraw$TE)
minT=min(compresultsraw$Time)
maxTN=max(compresultsraw$TN)
maxTE=max(compresultsraw$TE)
maxT=max(compresultsraw$Time)
labls=unique(apply(compresultsraw[,c("comp","Dom")],1, function(item){paste0(item["comp"],'-',item["Dom"])}))
cl= unique(compresultsraw$com)
apl=unique(compresultsraw$Planner)
colores=rainbow(length(labls))
#all comp and planners
imprimirini(typ=typu,name=paste0("Layers/CompleteTvsN"),12,7.25)
par(mar=c(5,5,3,9),xpd=FALSE)
plot(0,type='n', xlim=c(0,maxTN), ylim=c(0,log(maxT)), xlab="Nodes", ylab="Time", main="All Planners" )


for(c in 1:length(cl)){
    auxres=compresultsraw[compresultsraw$com==cl[c],]
    pl= unique(auxres$Planner)
    for(p in 1:length(pl)){
        ares=auxres[auxres$Planner==pl[p],]
        dl=unique(ares$Dom)
        pi=match(pl[p],apl)
        for(d in 1:length(dl)){
            an=paste0(cl[c],'-',dl[d])
            ni=match(an,labls)
            points(log(Time)~TN, col=colores[ni], pch=pchs[pi], data=ares[ares$Dom==dl[d],])
        }
    }
}
par(xpd=TRUE)
legend(maxTN*1.06, log(maxT)*1.1, legend=labls,col=colores, pch=19)
legend(maxTN*1.06, log(maxT)*0.6, legend=apl, pch=pchs)
imprimirfin()


imprimirini(typ=typu,name=paste0("Layers/CompleteTvslogE"),12,7.25)
par(mar=c(5,5,3,9),xpd=FALSE)
plot(0,type='n', xlim=c(log(minTE),log(maxTE)), ylim=c(0,log(maxT)), xlab="Edges", ylab="Time", main="All Planners" )


for(c in 1:length(cl)){
    auxres=compresultsraw[compresultsraw$com==cl[c],]
    pl= unique(auxres$Planner)
    for(p in 1:length(pl)){
        ares=auxres[auxres$Planner==pl[p],]
        dl=unique(ares$Dom)
        pi=match(pl[p],apl)
        for(d in 1:length(dl)){
            an=paste0(cl[c],'-',dl[d])
            ni=match(an,labls)
            points(log(Time)~log(TE), col=colores[ni], pch=pchs[pi], data=ares[ares$Dom==dl[d],])
        }
    }
}
par(xpd=TRUE)
legend(log(maxTE)*1.06, log(maxT)*1.1, legend=labls,col=colores, pch=19)
legend(log(maxTE)*1.06, log(maxT)*0.6, legend=apl, pch=pchs)
imprimirfin()

#files by comp
for(c in 1:length(cl)){
    auxres=compresultsraw[compresultsraw$com==cl[c],]
    pl= unique(auxres$Planner)
    adl=unique(auxres$Dom)
    colores=rainbow(length(adl))
    maxTN=max(auxres$TN)
    maxT=max(auxres$Time)
    imprimirini(typ=typu,name=paste0("Layers/", cl[c],"TvsN"),12,7.25)
    par(mar=c(5,5,3,9),xpd=FALSE)
    plot(0,type='n', xlim=c(0,maxTN), ylim=c(0,log(maxT)), xlab="Nodes", ylab="Time", main=paste0("Comp: ", cl[c]) )
    for(p in 1:length(pl)){
        ares=auxres[auxres$Planner==pl[p],]
        dl=unique(ares$Dom)
        for(d in 1:length(dl)){
            di=match(dl[d],adl)
            points(log(Time)~TN, col=colores[di], pch=pchs[p], data=ares[ares$Dom==dl[d],])
        }
    }
    par(xpd=TRUE)
    legend(maxTN*1.06, log(maxT)*0.6, legend=pl, pch=pchs)
    legend(maxTN*1.06, log(maxT)*1, legend=unique(auxres$Dom),col=colores, pch=19)
    imprimirfin()
}

for(c in 1:length(cl)){
    auxres=compresultsraw[compresultsraw$com==cl[c],]
    pl= unique(auxres$Planner)
    adl=unique(auxres$Dom)
    colores=rainbow(length(adl))
    maxTN=max(auxres$TN)
    minTN=min(auxres$TN)
    maxT=max(auxres$Time)
    imprimirini(typ=typu,name=paste0("Layers/", cl[c],"TvslogN"),12,7.25)
    par(mar=c(5,5,3,9),xpd=FALSE)
    plot(0,type='n', xlim=c(log(minTN),log(maxTN)), ylim=c(0,log(maxT)), xlab="Nodes", ylab="Time", main=paste0("Comp: ", cl[c]) )
    for(p in 1:length(pl)){
        ares=auxres[auxres$Planner==pl[p],]
        dl=unique(ares$Dom)
        for(d in 1:length(dl)){
            di=match(dl[d],adl)
            points(log(Time)~log(TN), col=colores[di], pch=pchs[p], data=ares[ares$Dom==dl[d],])
        }
    }
    par(xpd=TRUE)
    legend(log(maxTN)*1.06, log(maxT)*0.6, legend=pl, pch=pchs)
    legend(log(maxTN)*1.06, log(maxT)*1, legend=unique(auxres$Dom),col=colores, pch=19)
    imprimirfin()
}

for(c in 1:length(cl)){
    auxres=compresultsraw[compresultsraw$com==cl[c],]
    pl= unique(auxres$Planner)
    adl=unique(auxres$Dom)
    colores=rainbow(length(adl))
    maxTE=max(auxres$TE)
    maxT=max(auxres$Time)
    imprimirini(typ=typu,name=paste0("Layers/", cl[c],"TvsE"),12,7.25)
    par(mar=c(5,5,3,9),xpd=FALSE)
    plot(0,type='n', xlim=c(0,maxTE), ylim=c(0,log(maxT)), xlab="Edges", ylab="Time", main=paste0("Comp: ", cl[c]) )
    for(p in 1:length(pl)){
        ares=auxres[auxres$Planner==pl[p],]
        dl=unique(ares$Dom)
        for(d in 1:length(dl)){
            di=match(dl[d],adl)
            points(log(Time)~TE, col=colores[di], pch=pchs[p], data=ares[ares$Dom==dl[d],])
        }
    }
    par(xpd=TRUE)
    legend(maxTE*1.06, log(maxT)*0.6, legend=pl, pch=pchs)
    legend(maxTE*1.06, log(maxT)*1, legend=unique(auxres$Dom), pch=19, col=colores)
    imprimirfin()
}

for(c in 1:length(cl)){
    auxres=compresultsraw[compresultsraw$com==cl[c],]
    pl= unique(auxres$Planner)
    adl=unique(auxres$Dom)
    colores=rainbow(length(adl))
    maxTE=max(auxres$TE)
    minTE=min(auxres$TE)
    maxT=max(auxres$Time)
    imprimirini(typ=typu,name=paste0("Layers/", cl[c],"TvslogE"),12,7.25)
    par(mar=c(5,5,3,9),xpd=FALSE)
    plot(0,type='n', xlim=c(log(minTE),log(maxTE)), ylim=c(0,log(maxT)), xlab="Edges", ylab="Time", main=paste0("Comp: ", cl[c]) )
    for(p in 1:length(pl)){
        ares=auxres[auxres$Planner==pl[p],]
        dl=unique(ares$Dom)
        for(d in 1:length(dl)){
            di=match(dl[d],adl)
            points(log(Time)~log(TE), col=colores[di], pch=pchs[p], data=ares[ares$Dom==dl[d],])
        }
    }
    par(xpd=TRUE)
    legend(log(maxTE)*1.06, log(maxT)*0.6, legend=pl, pch=pchs)
    legend(log(maxTE)*1.06, log(maxT)*1, legend=unique(auxres$Dom),col=colores, pch=19)
    imprimirfin()
}

for(c in 1:length(cl)){
    auxres=compresultsraw[compresultsraw$com==cl[c],]
    pl= unique(auxres$Planner)
    adl=unique(auxres$Dom)
    colores=rainbow(length(adl))
    maxMT=max(auxres$MT)
    maxT=max(auxres$Time)
    imprimirini(typ=typu,name=paste0("Layers/", cl[c],"TvsML"),12,7.25)
    par(mar=c(5,5,3,9),xpd=FALSE)
    plot(0,type='n', xlim=c(0,maxMT), ylim=c(0,log(maxT)), xlab="Levels", ylab="Time", main=paste0("Comp: ", cl[c]) )
    for(p in 1:length(pl)){
        ares=auxres[auxres$Planner==pl[p],]
        dl=unique(ares$Dom)
        for(d in 1:length(dl)){
            di=match(dl[d],adl)
            points(log(Time)~MT, col=colores[di], pch=pchs[p], data=ares[ares$Dom==dl[d],])
        }
    }
    par(xpd=TRUE)
    legend(maxMT*1.06, log(maxT)*0.6, legend=pl, pch=pchs)
    legend(maxMT*1.06, log(maxT)*1, legend=unique(auxres$Dom),col=colores, pch=19)
    imprimirfin()
}
                                        #file by planner


for(p in 1:length(apl)){
    auxres=compresultsraw[compresultsraw$Planner==apl[p],]
    labls=unique(apply(compresultsraw[,c("comp","Dom")],1, function(item){paste0(item["comp"],'-',item["Dom"])}))
    dlaux=c()
    dcaux=c()
    cl= unique(auxres$comp)
    colores=rainbow(length(labls))
    maxTN=max(auxres$TN)
    minTN=min(auxres$TN)
    maxT=max(auxres$Time)
    imprimirini(typ=typu,name=paste0("Layers/", apl[p],"TvsN"),12,7.25)
    par(mar=c(5,5,3,9),xpd=FALSE)
    plot(0,type='n', xlim=c(0,maxTN), ylim=c(0,log(maxT)), xlab="Nodes", ylab="Time", main=paste0("Planner: ", apl[p]) )
    #alm=lm(log(Time+1)~log(TE),data=auxres)
    #abline(alm)
    bands= reg.conf.intervals(auxres$TN,log(auxres$Time+1))
    aval=auxres[,c("Time", "TN","gn")]
    aval$xval= round(aval$TN,2)
    aval$yval= log(aval$Time+1)
    aval$lab=0
    plbls= merge(x=aval, y=bands, by=c("xval"), all.x=TRUE)
    dwn=plbls$yval<plbls$LowBand
    upp=plbls$yval>plbls$UpperBand
    if(sum(dwn)>0){ plbls[dwn,]$lab=1 }
    if(sum(upp)>0){ plbls[upp,]$lab=2 }
    compresultsraw[compresultsraw$Planner==apl[p],]$Diff= plbls$lab
    for(c in 1:length(cl)){
        ares=auxres[auxres$comp==cl[c],]
        dl=unique(ares$Dom)
        for(d in 1:length(dl)){
            an=paste0(cl[c],'-',dl[d])
            dlaux=c(an,dlaux)
            ni=match(an,labls)
            dcaux=c(ni,dcaux)
            points(log(Time+1)~TN, col=colores[ni], pch=19, data=ares[ares$Dom==dl[d],])
        }
    }    
    if(sum(dwn)>0||sum(upp)>0){
        text(x=plbls[plbls$lab>0,]$xval,y=(plbls[plbls$lab>0,]$yval+0.25), labels=substrRight(plbls[plbls$lab>0,]$gn,5) )
    }
    par(xpd=TRUE)
#    legend(maxTN*1.06, log(maxT)*0.6, legend=apl, pch=pchs)
    legend(maxTN*1.06, log(maxT)*1, legend=dlaux,col=colores[dcaux], pch=19)
    imprimirfin()
}

for(p in 1:length(apl)){
    auxres=compresultsraw[compresultsraw$Planner==apl[p],]
    labls=unique(apply(compresultsraw[,c("comp","Dom")],1, function(item){paste0(item["comp"],'-',item["Dom"])}))
    dlaux=c()
    dcaux=c()
    cl= unique(auxres$comp)
    colores=rainbow(length(labls))
    maxTE=max(auxres$TE)
    minTE=min(auxres$TE)
    maxT=max(auxres$Time)
    imprimirini(typ=typu,name=paste0("Layers/", apl[p],"TvslogE"),12,7.25)
    par(mar=c(5,5,3,9),xpd=FALSE)
    plot(0,type='n', xlim=c(log(minTE),log(maxTE)), ylim=c(0,log(maxT)), xlab="Edges", ylab="Time", main=paste0("Planner: ", apl[p]) )
    #alm=lm(log(Time+1)~log(TE),data=auxres)
    #abline(alm)
    bands= reg.conf.intervals(log(auxres$TE),log(auxres$Time+1))
    aval=auxres[,c("Time", "TE","gn")]
    aval$xval= round(log(aval$TE),2)
    aval$yval= log(aval$Time+1)
    aval$lab=0
    plbls= merge(x=aval, y=bands, by=c("xval"), all.x=TRUE)
    dwn=plbls$yval<plbls$LowBand
    upp=plbls$yval>plbls$UpperBand
    if(sum(dwn)>0){ plbls[dwn,]$lab=1 }
    if(sum(upp)>0){ plbls[upp,]$lab=2 }
    compresultsraw[compresultsraw$Planner==apl[p],]$Diff= plbls$lab
    for(c in 1:length(cl)){
        ares=auxres[auxres$comp==cl[c],]
        dl=unique(ares$Dom)
        for(d in 1:length(dl)){
            an=paste0(cl[c],'-',dl[d])
            dlaux=c(an,dlaux)
            ni=match(an,labls)
            dcaux=c(ni,dcaux)
            points(log(Time+1)~log(TE), col=colores[ni], pch=19, data=ares[ares$Dom==dl[d],])
        }
    }
    par(xpd=TRUE)
    if(sum(dwn)>0||sum(upp)>0){
        text(x=plbls[plbls$lab>0,]$xval,y=(plbls[plbls$lab>0,]$yval+0.25), labels=substrRight(plbls[plbls$lab>0,]$gn,5) )
    }
#    legend(maxTN*1.06, log(maxT)*0.6, legend=apl, pch=pchs)
    legend(log(maxTE)*1.06, log(maxT)*1, legend=dlaux,col=colores[dcaux], pch=19)
    imprimirfin()
}


for(p in 1:length(apl)){
    auxres=compresultsraw[compresultsraw$Planner==apl[p],]
    labls=unique(apply(compresultsraw[,c("comp","Dom")],1, function(item){paste0(item["comp"],'-',item["Dom"])}))
    dlaux=c()
    dcaux=c()
    cl= unique(auxres$comp)
    colores=rainbow(length(labls))
    maxD=max(auxres$D)
    minD=min(auxres$D)
    maxT=max(auxres$Time)
    imprimirini(typ=typu,name=paste0("Layers/", apl[p],"TvsD"),12,7.25)
    par(mar=c(5,5,3,9),xpd=FALSE)
    plot(0,type='n', xlim=c(0,maxD), ylim=c(0,log(maxT)), xlab="Density", ylab="Time", main=paste0("Planner: ", apl[p]) )
    #alm=lm(log(Time+1)~log(TE),data=auxres)
    #abline(alm)
    bands= reg.conf.intervals(auxres$D,log(auxres$Time+1))
    aval=auxres[,c("Time", "D","gn")]
    aval$xval= round(aval$D,2)
    aval$yval= log(aval$Time+1)
    aval$lab=0
    plbls= merge(x=aval, y=bands, by=c("xval"), all.x=TRUE)
    dwn=plbls$yval<plbls$LowBand
    upp=plbls$yval>plbls$UpperBand
    if(sum(dwn)>0){ plbls[dwn,]$lab=1 }
    if(sum(upp)>0){ plbls[upp,]$lab=2 }
    compresultsraw[compresultsraw$Planner==apl[p],]$Diff= plbls$lab
    for(c in 1:length(cl)){
        ares=auxres[auxres$comp==cl[c],]
        dl=unique(ares$Dom)
        for(d in 1:length(dl)){
            an=paste0(cl[c],'-',dl[d])
            dlaux=c(an,dlaux)
            ni=match(an,labls)
            dcaux=c(ni,dcaux)
            points(log(Time+1)~D, col=colores[ni], pch=19, data=ares[ares$Dom==dl[d],])
        }
    }    
    par(xpd=TRUE)
    if(sum(dwn)>0||sum(upp)>0){
        text(x=plbls[plbls$lab>0,]$D,y=(log(plbls[plbls$lab>0,]$Time)+0.25), labels=substrRight(plbls[plbls$lab>0,]$gn,5) )
    }
#    legend(maxTN*1.06, log(maxT)*0.6, legend=apl, pch=pchs)
    legend(maxD*1.06, log(maxT)*1, legend=dlaux,col=colores[dcaux], pch=19)
    imprimirfin()
}
