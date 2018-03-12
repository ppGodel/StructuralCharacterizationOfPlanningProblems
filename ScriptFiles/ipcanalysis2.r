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

reg.conf.intervals <- function(x, y, intx) {
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
  x_new2 <- round(seq(min(x)-x_int,max(x)+x_int, 10**intx),abs(intx))
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

allplanners = function(x,y, data, logx=FALSE, logy=FALSE){
    fy=ifelse(logy,paste0('log(',y,')'),y)#paste0('data$',y))
    fx=ifelse(logx,paste0('log(',x,')'),x)#paste0('data$',x))
    frml=as.formula(paste0(fy,'~',fx))
    #print(frml)
    pchs=c(1:15,17,18,20,22,26,27)
    apl=unique(data$Planner)
    labls=unique(apply(data[,c("comp","Dom")],1, function(item){paste0(item["comp"],'-',item["Dom"])}))
    cl= unique(data$com)
    colores=rainbow(length(labls))

    for(p in 1:length(apl)){
        auxres=data[data$Planner==apl[p],]
        labls=unique(apply(data[,c("comp","Dom")],1, function(item){paste0(item["comp"],'-',item["Dom"])}))
        dlaux=c()
        dcaux=c()
        cl= unique(auxres$comp)
        colores=rainbow(length(labls))
        maxVx=ifelse(logx,log(max(auxres[,x]+1)),max(auxres[,x]))
        minVx=ifelse(logx,log(min(auxres[,x]+1)),min(auxres[,x]))
        maxVy=ifelse(logy,log(max(auxres[,y]+1)),max(auxres[,y]))
        minVy=ifelse(logy,log(min(auxres[,y]+1)),min(auxres[,y]))
        ranx=maxVx-minVx
        rany=maxVy-minVy
        dpx=log(ranx,10)
        dpy=log(rany,10)
        if(dpx>1.6){
            dpx=0
        }else{
            dpx=floor(log(ranx,10))-1
        }
        if(dpy>1.6){
            dpy=0
        }else{
            dpy=floor(log(rany,10))-1
        }
        imprimirini(typ=typu,name=paste0("Layers/", apl[p],y,"vs",x),12,7.25)
        par(mar=c(5,5,3,9),xpd=FALSE)
        plot(0,type='n', xlim=c(0,maxVx), ylim=c(0,maxVy), xlab=fx, ylab=fy, main=paste0("Planner: ", apl[p]) )
                                        #alm=lm(log(Time+1)~log(TE),data=auxres)
                                        #abline(alm)
        aval=auxres[,c(x, y,"gn")]
        if(logx){aval$xval=log(aval[,x]+1)}else{aval$xval=aval[,x]}
        #aval$xval= ifelse(logx,log(aval[,x]+1),aval[,x])
        if(logy){aval$yval=log(aval[,y]+1)}else{aval$yval=aval[,y]}
        #aval$yval= ifelse(logy,log(aval[,y]+1),aval[,y])
        aval$lab=0
        bands= reg.conf.intervals(x=aval$xval, y=aval$yval, intx=dpx)
        aval$xval=round(aval$xval,abs(dpx))
        plbls= merge(x=aval, y=bands, by=c("xval"), all.x=TRUE)
        dwn=plbls$yval<plbls$LowBand
        upp=plbls$yval>plbls$UpperBand
        if(sum(dwn)>0){ plbls[dwn,]$lab=1 }
        if(sum(upp)>0){ plbls[upp,]$lab=2 }
        data[data$Planner==apl[p],]$Diff= plbls$lab
        for(c in 1:length(cl)){
            ares=auxres[auxres$comp==cl[c],]
            dl=unique(ares$Dom)
            for(d in 1:length(dl)){
                an=paste0(cl[c],'-',dl[d])
                dlaux=c(an,dlaux)
                ni=match(an,labls)
                dcaux=c(ni,dcaux)
                if(logx){ppx=log(ares[ares$Dom==dl[d],x]+1)}else{ppx=ares[ares$Dom==dl[d],x]}
                if(logy){ppy=log(ares[ares$Dom==dl[d],y]+1)}else{ppy=ares[ares$Dom==dl[d],y]}
                points(x=ppx, y=ppy, col=colores[ni], pch=19)
            }
        }    
        par(xpd=TRUE)
        if(sum(dwn)>0||sum(upp)>0){
            text(x=plbls[plbls$lab>0,"xval"],y=(plbls[plbls$lab>0,"yval"]+ifelse(dpy==0,0.25,0.25*10**dpy)), labels=substrRight(plbls[plbls$lab>0,]$gn,5) )
        }
                                        #    legend(maxTN*1.06, log(maxT)*0.6, legend=apl, pch=pchs)
        legend(maxVx*1.06, maxVy*1, legend=dlaux,col=colores[dcaux], pch=19)
        imprimirfin()
    }
    names(data)[names(data) == 'Diff'] <- paste0('C',x,'v',y)
    return(data)
}

clas1=allplanners(x="D",y="Time", data=compresultsraw, logx=FALSE,logy=TRUE)
clas2=allplanners(x="TN",y="Time", data=compresultsraw, logx=FALSE,logy=TRUE)
clas3=allplanners(x="TE",y="Time", data=compresultsraw, logx=TRUE,logy=TRUE)
clas4=allplanners(x="PM",y="Time", data=compresultsraw, logx=TRUE,logy=TRUE)
