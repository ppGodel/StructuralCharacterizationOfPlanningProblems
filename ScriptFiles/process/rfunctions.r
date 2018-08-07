library('mongolite')
#library('psych')
library('ggplot2')
library('plyr')
library('corrplot')
library("colorRamps")

#typu="png"
typg<<-"eps"
"/" <- function(x,y) {ifelse(y==0,0,base:::"/"(x,y))}
gpath="images/"
imprimirini= function(typ, name, w=32, h=18){
    typg<<-typ
    if(typg=="eps"){
        postscript(paste(gpath,name,".eps", sep=""), width=w,height=h)
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

between <- function(x,a,b){
    if(length(x)>1){
        return(sapply(x,FUN=function(i){between(i,a,b)}))
    }else{
        return(x>=a&&x<=b)
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
  cookds =cooks.distance(lmmodel)  
  mdf=cbind(x,y)
  cvmdf=cov(mdf)
  m_dist=x*0
  tryCatch({ m_dist <- sqrt(mahalanobis(mdf, colMeans(mdf), cvmdf))},error=function(erm){ m_dist=x*0 })
  
  # Fit linear model with extracted coefficients
  #x_new <- seq(min(x),max(x), length.out=length(x))
  y.fit <- b1 * x + b0
  
  # Find the standard error of the regression line
  se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
  
  # Fit a new linear model that extends past the given data points (for plotting)
  x_int <- max(x)-min(x)
  x_new2 <- unique(c(x-x_int,x,x+x_int))
  y.fit2 <- b1 * x_new2 + b0
  
  # Warnings of mismatched lengths are suppressed
  slope.upper <- suppressWarnings(y.fit + (t.val * se))
  slope.lower <- suppressWarnings(y.fit - (t.val * se))
  # Collect the computed confidence bands into a data.frame and name the colums
  mdist <- rep(0,n)
  mdist <- ifelse(y>slope.upper,y-slope.upper,mdist)
  mdist <- ifelse(y<slope.lower,slope.lower-y,mdist)
  bands <- data.frame(cbind(x,slope.lower, y.fit, slope.upper, abs(mdist), mdist>0, cookds, cookds>4*mean(cookds), m_dist, m_dist>sqrt(qchisq(0.995,2)), ifelse(y>y.fit,2,ifelse(y<y.fit,0,1)) ))
  colnames(bands) <- c('xval','LowBand','regval', 'UpperBand', 'Dist', 'Out', 'CookDist', 'CookOut', "MahaDist","MahaOut", "Disc")
  if(prnt){
  #Plot the fitted linear regression line and the computed confidence bands
  #plot(x, y, cex = 1.75, pch = 21, bg = 'gray')
      p.slope.upper <- suppressWarnings(y.fit2 + t.val * se)
      p.slope.lower <- suppressWarnings(y.fit2 - t.val * se)
      xpol=c(x_new2,rev(x_new2))
      ypol=c(p.slope.lower, rev(p.slope.upper))
      polygon(x=xpol, y=ypol, border=NA, col="antiquewhite2")
      lines(x=x_new2, y=y.fit2, col = 'black', lwd = 2)
      lines(x=x_new2, y=p.slope.upper, col = 'brown', lty = 2, lwd = 2)
      lines(x=x_new2, y=p.slope.lower, col = 'brown', lty = 2, lwd = 2)
  }
  return(bands)
}

choose.lm <-function(px,py){
    inter=c(seq(-10,-2,1),seq(-1,1,0.1),seq(2,10,1))
    result=data.frame(tx=numeric(),ty=numeric(),r2=numeric(),m=numeric(),b=numeric(),u=numeric())
    for(i in inter){
        x=tukeyLadder(px,i)
        for(j in inter){
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
    fres=result[result$r2<=0.95,]
    return( fres[which.max(fres$r2),] )
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

allplanners = function(nx,ny, data, prnt=TRUE, prefn=""){
    pchs=c(1:15,17,18,20,22,26,27)
    apl=unique(data$planner)
    labls=unique(apply(data[,c("com","dom")],1, function(item){paste0(item["com"],'-',item["dom"])}))
    cl= unique(data$com)
    colores=rainbow(length(labls))
    for(p in 1:length(apl)){
        auxre=data[data$planner==apl[p],]
        cl= unique(auxre$com)
        for(c in 1:length(cl)){
            auxres=auxre[auxre$com==cl[c],]
            if(dim(auxres)[1]<4){
                next
            }
            dlaux=c()
            dcaux=c()
            fitval=choose.lm(px=auxres[,nx],py=auxres[,ny])
            tranx=tukeyLadder(auxres[,nx],fitval$tx)
            trany=tukeyLadder(auxres[,ny],fitval$ty)
            lx=nx
            ly=ny
            if(fitval$tx!=1){
                lx=bquote(.(nx)^.(fitval$tx))
            }
            if(fitval$ty!=1){
                ly=bquote(.(ny)^.(fitval$ty))  
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
                imprimirini(typ=typu,name=paste0("Layers/",prefn,cl[c],"_", apl[p],"_",ny,"vs",nx),12,7.25)
                par(mar=c(5,5,3,9),xpd=FALSE)
                #mlab=paste("Com:", cl[c],"Planner:", apl[p],expression(R^2),":",round(fitval$r2*100,2))
                mlab=bquote("Com:"~ .(cl[c]) ~"Planner:"~ .(apl[p]) ~ R^2 ~ ":" ~ .(round(fitval$r2*100,2)) )
                plot(0,type='n', xlim=c(minVx,maxVx), ylim=c(minVy,maxVy), xlab=lx, ylab=ly, main=mlab ) 
            }
                                        #alm=lm(log(Time+1)~log(TE),data=auxres)
                                        #abline(alm)
            aval=auxres[,c(nx, ny,"gn","com","dom","planner")]
            aval=cbind(rn=rownames(aval),aval)
            aval$xval= tranx
            aval$yval= trany
            aval$lab=0
            bands= reg.conf.intervals(x=aval$xval, y=aval$yval, prnt=prnt)
            plbls= merge(x=aval, y=bands, by=c("xval"), all.x=TRUE)
            plbls=plbls[!duplicated(plbls$rn),]            
            plbls=plbls[order(plbls$rn),]
            plbls$dwn=plbls$yval<plbls$LowBand
            plbls$upp=plbls$yval>plbls$UpperBand
            plbls$dist=0
            plbls$tdist=0
            if(!is.na(sum(plbls$dwn))&&sum(plbls$dwn)>0){
                plbls[plbls$dwn,]$lab=1
                #plbls[plbls$dwn,]$dist=plbls[plbls$dwn,]$LowBand-plbls[plbls$dwn,]$yval
                if(fitval$ty!=0){
                    plbls[plbls$dwn,]$tdist=tukeyLadder(abs(plbls[plbls$dwn,]$LowBand),1/fitval$ty)-plbls[plbls$dwn,ny]
                }else{
                    plbls[plbls$dwn,]$tdist=plbls[plbls$dwn,ny]-exp(plbls[plbls$dwn,]$yval)
                }                
            }
            if(!is.na(sum(plbls$upp))&&sum(plbls$upp)>0){
                plbls[plbls$upp,]$lab=2
                #plbls[plbls$upp,]$dist=plbls[plbls$upp,]$yval-plbls[plbls$upp,]$UpperBand
                if(fitval$ty!=0){
                    plbls[plbls$upp,]$tdist=plbls[plbls$upp,ny]-tukeyLadder(abs(plbls[plbls$upp,]$UpperBand),1/fitval$ty)
                }else{
                    plbls[plbls$upp,]$tdist=plbls[plbls$upp,ny]-exp(plbls[plbls$upp,]$UpperBand)
                }
            }
            data[rownames(data[data$planner==apl[p]&data$com==cl[c],]),]$Class= plbls$lab
            data[data$planner==apl[p]&data$com==cl[c],]$R2= rep(x=fitval$r2,times=dim(data[data$planner==apl[p]&data$com==cl[c],])[1])
            #data[data$planner==apl[p]&data$com==cl[c],]$MaxDist= rep(x=max(plbls$dist),times=dim(data[data$planner==apl[p]&data$com==cl[c],])[1])
            data[rownames(data[data$planner==apl[p]&data$com==cl[c],]),]$Dist = plbls$tdist
            data[rownames(data[data$planner==apl[p]&data$com==cl[c],]),]$MahaDist = plbls$MahaDist
            data[rownames(data[data$planner==apl[p]&data$com==cl[c],]),]$MahaOut = plbls$MahaOut
            data[rownames(data[data$planner==apl[p]&data$com==cl[c],]),]$CookDist = plbls$CookDist
            data[rownames(data[data$planner==apl[p]&data$com==cl[c],]),]$CookOut = plbls$CookOut
            data[rownames(data[data$planner==apl[p]&data$com==cl[c],]),]$Disc = plbls$Disc
        
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
    
    return(data[,c("com","planner","dom","gn","Class","Cfactor","Cresp","Ctran","R2","Dist","MahaDist","MahaOut","CookDist","CookOut","Disc")]) 
}


allplannersbydom = function(nx,ny, data, prnt=TRUE, prefn=""){
    pchs=c(1:15,17,18,20,22,26,27)
    apl=unique(data$planner)
    labls=unique(apply(data[,c("com","dom")],1, function(item){paste0(item["com"],'-',item["dom"])}))
    cl= unique(data$com)
    colores=rainbow(length(labls))
    for(p in 1:length(apl)){
        auxre=data[data$planner==apl[p],]
        cl= unique(auxre$com)
        for(c in 1:length(cl)){
            auxred=auxre[auxre$com==cl[c],]
            dl=unique(auxred$dom)
            for(d in 1:length(dl)){
                auxres=auxred[auxred$dom==dl[d],]
                if(dim(auxres)[1]<4){
                    next
                }
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
                    imprimirini(typ=typu,name=paste0("Layers/",prefn,cl[c],"_", apl[p],"_", dl[d],"_",ny,"vs",nx),12,7.25)
                    par(mar=c(5,5,3,9),xpd=FALSE)
                    plot(0,type='n', xlim=c(minVx,maxVx), ylim=c(minVy,maxVy), xlab=lx, ylab=ly, main=paste("Com:", cl[c],"Planner:", apl[p], "Domain:", dl[d],expression(R^2),":",round(fitval$r2*100,2)) )
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
                plbls=plbls[!duplicated(plbls$rn),]            
                plbls=plbls[order(plbls$rn),]
                plbls$dwn=plbls$yval<plbls$LowBand
                plbls$upp=plbls$yval>plbls$UpperBand
                plbls$dist=0
                plbls$tdist=0
                if(!is.na(sum(plbls$dwn))&&sum(plbls$dwn)>0){
                    plbls[plbls$dwn,]$lab=1
                    plbls[plbls$dwn,]$dist=plbls[plbls$dwn,]$LowBand-plbls[plbls$dwn,]$yval
                    if(fitval$ty!=0){
                        plbls[plbls$dwn,]$tdist=tukeyLadder(abs(plbls[plbls$dwn,]$LowBand),1/fitval$ty)-plbls[plbls$dwn,ny]
                    }else{
                        plbls[plbls$dwn,]$tdist=plbls[plbls$dwn,ny]-exp(plbls[plbls$dwn,]$yval)
                    }                
                }
                if(!is.na(sum(plbls$upp))&&sum(plbls$upp)>0){
                    plbls[plbls$upp,]$lab=2
                    plbls[plbls$upp,]$dist=plbls[plbls$upp,]$yval-plbls[plbls$upp,]$UpperBand
                    if(fitval$ty!=0){
                        plbls[plbls$upp,]$tdist=plbls[plbls$upp,ny]-tukeyLadder(abs(plbls[plbls$upp,]$UpperBand),1/fitval$ty)
                    }else{
                        plbls[plbls$upp,]$tdist=plbls[plbls$upp,ny]-exp(plbls[plbls$upp,]$UpperBand)
                    }
                }
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$dom==dl[d],]),]$Class= plbls$lab
                data[data$planner==apl[p]&data$com==cl[c]&data$dom==dl[d],]$R2= rep(x=fitval$r2,times=dim(data[data$planner==apl[p]&data$com==cl[c]&data$dom==dl[d],])[1])
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$dom==dl[d],]),]$Dist = plbls$tdist
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$dom==dl[d],]),]$MahaDist = plbls$MahaDist
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$dom==dl[d],]),]$MahaOut = plbls$MahaOut
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$dom==dl[d],]),]$CookDist = plbls$CookDist
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$dom==dl[d],]),]$CookOut = plbls$CookOut
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$dom==dl[d],]),]$Disc = plbls$Disc
        
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
    data$Cfactor=nx#paste0('C',nx),'v',ny)
    data$Cresp=ny
    data$Ctran=paste0(lx,'v',ly)
    
    return(data[,c("com","planner","dom","gn","Class","Cfactor","Cresp","Ctran","R2","Dist","MahaDist","MahaOut","CookDist","CookOut","Disc")]) 
}


allplannersbyplan = function(nx,ny, data, prnt=TRUE, prefn=""){
    pchs=c(1:15,17,18,20,22,26,27)
    apl=unique(data$planner)
    labls=unique(apply(data[,c("com","dom")],1, function(item){paste0(item["com"],'-',item["dom"])}))
    cl= unique(data$com)
    colores=rainbow(length(labls))
    for(p in 1:length(apl)){
        auxre=data[data$planner==apl[p],]
        cl= unique(auxre$com)
        for(c in 1:length(cl)){
            auxred=auxre[auxre$com==cl[c],]
            sl=unique(auxred$Steps)
            for(s in 1:length(sl)){
                auxres=auxred[auxred$Steps==sl[s],]
                if(dim(auxres)[1]<=4){
                    next
                }
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
                    imprimirini(typ=typu,name=paste0("Layers/",prefn,cl[c],"_", apl[p],"_Plan", sl[s],"_",ny,"vs",nx),12,7.25)
                    par(mar=c(5,5,3,9),xpd=FALSE)
                    plot(0,type='n', xlim=c(minVx,maxVx), ylim=c(minVy,maxVy), xlab=lx, ylab=ly, main=paste("Com:", cl[c],"Planner:", apl[p], "Plan Lenght:", sl[s],expression(R^2),":",round(fitval$r2*100,2)) )
            }
                                        #alm=lm(log(Time+1)~log(TE),data=auxres)
                                        #abline(alm)
                aval=auxres[,c(nx, ny,"gn","com","dom","planner","Steps")]
                aval=cbind(rn=rownames(aval),aval)
                aval$xval= tranx
                aval$yval= trany
                aval$lab=0
                bands= reg.conf.intervals(x=aval$xval, y=aval$yval, prnt)
                plbls= merge(x=aval, y=bands, by=c("xval"), all.x=TRUE)
                plbls=plbls[!duplicated(plbls$rn),]            
                plbls=plbls[order(plbls$rn),]
                plbls$dwn=plbls$yval<plbls$LowBand
                plbls$upp=plbls$yval>plbls$UpperBand
                plbls$dist=0
                plbls$tdist=0
                if(!is.na(sum(plbls$dwn))&&sum(plbls$dwn)>0){
                    plbls[plbls$dwn,]$lab=1
                    plbls[plbls$dwn,]$dist=plbls[plbls$dwn,]$LowBand-plbls[plbls$dwn,]$yval
                    if(fitval$ty!=0){
                        plbls[plbls$dwn,]$tdist=tukeyLadder(abs(plbls[plbls$dwn,]$LowBand),1/fitval$ty)-plbls[plbls$dwn,ny]
                    }else{
                        plbls[plbls$dwn,]$tdist=plbls[plbls$dwn,ny]-exp(plbls[plbls$dwn,]$yval)
                    }                
                }
                if(!is.na(sum(plbls$upp))&&sum(plbls$upp)>0){
                    plbls[plbls$upp,]$lab=2
                    plbls[plbls$upp,]$dist=plbls[plbls$upp,]$yval-plbls[plbls$upp,]$UpperBand
                    if(fitval$ty!=0){
                        plbls[plbls$upp,]$tdist=plbls[plbls$upp,ny]-tukeyLadder(abs(plbls[plbls$upp,]$UpperBand),1/fitval$ty)
                    }else{
                        plbls[plbls$upp,]$tdist=plbls[plbls$upp,ny]-exp(plbls[plbls$upp,]$UpperBand)
                    }
                }
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$Steps==sl[s],]),]$Class= plbls$lab
                data[data$planner==apl[p]&data$com==cl[c]&data$Steps==sl[s],]$R2= rep(x=fitval$r2,times=dim(data[data$planner==apl[p]&data$com==cl[c]&data$Steps==sl[s],])[1])
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$Steps==sl[s],]),]$Dist = plbls$tdist
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$Steps==sl[s],]),]$MahaDist = plbls$MahaDist
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$Steps==sl[s],]),]$MahaOut = plbls$MahaOut
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$Steps==sl[s],]),]$CookDist = plbls$CookDist
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$Steps==sl[s],]),]$CookOut = plbls$CookOut
                data[rownames(data[data$planner==apl[p]&data$com==cl[c]&data$Steps==sl[s],]),]$Disc = plbls$Disc
        
            if(prnt){
            #for(c in 1:length(cl)){
                #ares=aval[auxres$com==cl[c],]
                dl=unique(aval$dom)
                for(d in 1:length(dl)){
                an=paste0(cl[c],'-',dl[d])
                ni=match(an,labls)
                dcaux=c(ni,dcaux)
                dlaux=c(an,dlaux)
                }
                points(x=aval[aval$Steps==sl[s]&aval$com==cl[c],"xval"], y=aval[aval$Steps==sl[s]&aval$com==cl[c],"yval"], pch=19, col=colores[ni])
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
    data$Cfactor=nx#paste0('C',nx),'v',ny)
    data$Cresp=ny
    data$Ctran=paste0(lx,'v',ly)
    
    return(data[,c("com","planner","dom","gn","Class","Cfactor","Cresp","Ctran","R2","Dist","MahaDist","MahaOut","CookDist","CookOut","Disc")]) 
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
        basedataframe$LogTime=log(basedataframe$Time+1)
        basedataframe$Class=0
        basedataframe$R2=0
        basedataframe$Dist=0
        basedataframe$MahaDist=0
        basedataframe$CookDist=0
        basedataframe$Disc=0
        basedataframe$MahaOut=FALSE
        basedataframe$CookOut=FALSE
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
        basedataframe$LogTime=log(basedataframe$Time+1)
        basedataframe$Class=0
        basedataframe$R2=0
        basedataframe$Dist=0
        basedataframe$MahaDist=0
        basedataframe$CookDist=0
        basedataframe$Disc=0
        basedataframe$MahaOut=FALSE
        basedataframe$CookOut=FALSE
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

createDataSetbyPlanWithClassification <- function(basedataframe, filename, prin, prefn){
    if(!file.exists(filename)){
        c1=allplannersbyplan(nx="D" ,ny="Time", data=basedataframe,prnt=prin, prefn=prefn)
        c2=allplannersbyplan(nx="DM",ny="Time", data=basedataframe,prnt=prin, prefn=prefn)
        c3=allplannersbyplan(nx="TN",ny="Time", data=basedataframe,prnt=prin, prefn=prefn)
        c4=allplannersbyplan(nx="TE",ny="Time", data=basedataframe,prnt=prin, prefn=prefn)
        c5=allplannersbyplan(nx="TME",ny="Time", data=basedataframe,prnt=prin, prefn=prefn)
        rdf=rbind(c1,c2,c3,c4,c5)
        write.csv(rdf, filename)
    }else{
        rdf = read.csv(filename)
    }
    return(rdf)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
