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

grcon= mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="graphIPCRes")
compresultsraw=data.frame(grcon$find())
compresultsbase=aggregate(cbind(ts,fa)~probname+comp+dom, compresultsraw, max)
compresultstan=aggregate(cbind(stantime=ptime,stansteps=psteps)~probname+comp+dom, compresultsraw[compresultsraw$planner=="STAN",], max)
compresultsbb=aggregate(cbind(bbtime=ptime,bbsteps=psteps)~probname+comp+dom, compresultsraw[compresultsraw$planner=="BLACKBOX",], max)
compresultsipp=aggregate(cbind(ipptime=ptime,ippsteps=psteps)~probname+comp+dom, compresultsraw[compresultsraw$planner=="IPP",], max)
compresultshsp=aggregate(cbind(hsptime=ptime,hspsteps=psteps)~probname+comp+dom, compresultsraw[compresultsraw$planner=="HSP",], max)
compresultMIN=aggregate(cbind(mintime=ptime,minsteps=psteps)~probname+comp+dom, compresultsraw, function(x) {min(x[x > 0], na.rm=T)})
