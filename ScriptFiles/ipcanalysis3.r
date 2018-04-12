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
    if(typg=="eps"){118, units = 'in', res= 300)
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

reg.conf.intervals <- function(x, y, intx, prnt=TRUE) {
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
            m=lmmodel$coefficients[1]
            b=lmmodel$coefficients[2]
            y.fit <- b1 * x + b0
            se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
            result=rbind(result,c(tx=i,ty=j,r2=summary(lmmodel)$r.squared),m=m, b=b, u=t.val*se)
        }
    }
    colnames(result)=c("tx","ty","r2","m","b","u")
    return( result[which.max(result$r2),] )
}


allplanners = function(nx,ny, data, prnt=TRUE){
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
        fitval=choose.lm(auxres[,nx],auxres[,ny])
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
        #aqui voee
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
            imprimirini(typ=typu,name=paste0("Layers/", apl[p],ny,"vs",nx),12,7.25)
            par(mar=c(5,5,3,9),xpd=FALSE)
            plot(0,type='n', xlim=c(0,maxVx), ylim=c(0,maxVy), xlab=lx, ylab=ly, main=paste("Planner:", apl[p],"R^2:",round(fitval$r2*100,2)) )
        }
                                        #alm=lm(log(Time+1)~log(TE),data=auxres)
                                        #abline(alm)
        aval=auxres[,c(nx, ny,"gn","comp","Dom","Planner")]
        aval$xval= tranx
        aval$yval= trany
        aval$lab=0
        aval$dist=pointlinedist(m=fitval$m,b=fitval$b, fitval$)
        #print(paste("minx:",min(aval$xval),"maxx:",max(aval$xval),"x_int:",max(aval$xval)-min(aval$xval),"intx:", dpx))
        bands= reg.conf.intervals(x=aval$xval, y=aval$yval, intx=dpx, prnt)
        #aval$xval=round(aval$xval,-dpx)
        plbls= merge(x=aval, y=bands, by=c("xval"), all.x=TRUE)
        dwn=plbls$yval<plbls$LowBand
        upp=plbls$yval>plbls$UpperBand
        if(sum(dwn)>0){ plbls[dwn,]$lab=1 }
        if(sum(upp)>0){ plbls[upp,]$lab=2 }
        data[data$Planner==apl[p],]$Class= plbls$lab
        data[data$Planner==apl[p],]$R2= rep(x=fitval$r2,times=dim(data[data$Planner==apl[p],])[1])
        
        if(prnt){
            for(c in 1:length(cl)){
                ares=aval[auxres$comp==cl[c],]
                dl=unique(ares$Dom)
                for(d in 1:length(dl)){
                    an=paste0(cl[c],'-',dl[d])
                    dlaux=c(an,dlaux)
                    ni=match(an,labls)
                    dcaux=c(ni,dcaux)
                    points(x=ares[ares$Dom==dl[d],"xval"], y=ares[ares$Dom==dl[d],"yval"], col=colores[ni], pch=19)
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
    }
    cfactor=paste0('C',nx,'v',ny)
    names(data)[names(data) == 'Class'] <- cfactor
    return(data[,c("comp","Planner","Dom","gn",cfactor,"R2")]) #
}


grcon= mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="graphIPCRes")
compresultsraw=data.frame(grcon$find())
compresultsraw$LogTime=log(compresultsraw$Time+1)
compresultsraw$Class=NA
compresultsraw$R2=NA
                           
clas1=allplanners(nx="D" ,ny="Time", data=compresultsraw)
clas2=allplanners(nx="TN",ny="Time", data=compresultsraw)
clas3=allplanners(nx="TE",ny="Time", data=compresultsraw)
clas4=allplanners(nx="PM",ny="Time", data=compresultsraw)

clas5=allplanners(nx="D" ,ny="LogTime", data=compresultsraw)
clas6=allplanners(nx="TN",ny="LogTime", data=compresultsraw)
clas7=allplanners(nx="TE",ny="LogTime", data=compresultsraw)
clas8=allplanners(nx="PM",ny="LogTime", data=compresultsraw)

allclass= join_all(dfs=list(clas1,clas2,clas3,clas4,clas5,clas6,clas7,clas8), by=c("comp","Planner","Dom","gn"),type="left")

#write.csv(allclass,'allclass.csv')
