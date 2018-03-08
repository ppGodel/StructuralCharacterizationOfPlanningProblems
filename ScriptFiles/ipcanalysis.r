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

grcon= mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="graphIPCResComp")
compresultsraw=data.frame(grcon$find())

cl= unique(compresultsraw$com)
pchs=c(20,21,3,4,2,18,10)
for(c in 1:length(cl)){
    auxres=compresultsraw[compresultsraw$com==cl[c],]
    pl= unique(auxres$Planner)
    colores=rainbow(length(pl))
    maxTN=max(auxres$TN)
    maxT=max(auxres$Time)
    imprimirini(typ=typu,name=paste0("Layers/", cl[c],"TvsN"),12,7.25)
    par(mar=c(5,5,3,9),xpd=FALSE)
    plot(0,type='n', xlim=c(0,maxTN), ylim=c(0,log(maxT)), xlab="Nodes", ylab="Time", main=paste0("Comp: ", cl[c]) )
    for(p in 1:length(pl)){
        ares=auxres[auxres$Planner==pl[p],]
        dl=unique(ares$Dom)
        for(d in 1:length(dl)){
            points(log(Time)~TN, col=colores[p], pch=pchs[d], data=ares[ares$Dom==dl[d],])
        }
    }
    par(xpd=TRUE)
    legend(maxTN*1.06, log(maxT)*0.6, legend=pl,col=colores, pch=19)
    legend(maxTN*1.06, log(maxT)*1, legend=unique(auxres$Dom), pch=pchs)
    imprimirfin()
}

for(c in 1:length(cl)){
    auxres=compresultsraw[compresultsraw$com==cl[c],]
    pl= unique(auxres$Planner)
    colores=rainbow(length(pl))
    maxTN=max(auxres$TN)
    maxT=max(auxres$Time)
    imprimirini(typ=typu,name=paste0("Layers/", cl[c],"TvslogN"),12,7.25)
    par(mar=c(5,5,3,9),xpd=FALSE)
    plot(0,type='n', xlim=c(0,log(maxTN)), ylim=c(0,log(maxT)), xlab="Nodes", ylab="Time", main=paste0("Comp: ", cl[c]) )
    for(p in 1:length(pl)){
        ares=auxres[auxres$Planner==pl[p],]
        dl=unique(ares$Dom)
        for(d in 1:length(dl)){
            points(log(Time)~log(TN), col=colores[p], pch=pchs[d], data=ares[ares$Dom==dl[d],])
        }
    }
    par(xpd=TRUE)
    legend(log(maxTN)*1.06, log(maxT)*0.6, legend=pl,col=colores, pch=19)
    legend(log(maxTN)*1.06, log(maxT)*1, legend=unique(auxres$Dom), pch=pchs)
    imprimirfin()
}

for(c in 1:length(cl)){
    auxres=compresultsraw[compresultsraw$com==cl[c],]
    pl= unique(auxres$Planner)
    colores=rainbow(length(pl))
    maxTE=max(auxres$TE)
    maxT=max(auxres$Time)
    imprimirini(typ=typu,name=paste0("Layers/", cl[c],"TvsE"),12,7.25)
    par(mar=c(5,5,3,9),xpd=FALSE)
    plot(0,type='n', xlim=c(0,maxTE), ylim=c(0,log(maxT)), xlab="Edges", ylab="Time", main=paste0("Comp: ", cl[c]) )
    for(p in 1:length(pl)){
        ares=auxres[auxres$Planner==pl[p],]
        dl=unique(ares$Dom)
        for(d in 1:length(dl)){
            points(log(Time)~TE, col=colores[p], pch=pchs[d], data=ares[ares$Dom==dl[d],])
        }
    }
    par(xpd=TRUE)
    legend(maxTE*1.06, log(maxT)*0.6, legend=pl,col=colores, pch=19)
    legend(maxTE*1.06, log(maxT)*1, legend=unique(auxres$Dom), pch=pchs)
    imprimirfin()
}

for(c in 1:length(cl)){
    auxres=compresultsraw[compresultsraw$com==cl[c],]
    pl= unique(auxres$Planner)
    colores=rainbow(length(pl))
    maxTE=max(auxres$TE)
    maxT=max(auxres$Time)
    imprimirini(typ=typu,name=paste0("Layers/", cl[c],"TvslogE"),12,7.25)
    par(mar=c(5,5,3,9),xpd=FALSE)
    plot(0,type='n', xlim=c(0,log(maxTE)), ylim=c(0,log(maxT)), xlab="Edges", ylab="Time", main=paste0("Comp: ", cl[c]) )
    for(p in 1:length(pl)){
        ares=auxres[auxres$Planner==pl[p],]
        dl=unique(ares$Dom)
        for(d in 1:length(dl)){
            points(log(Time)~log(TE), col=colores[p], pch=pchs[d], data=ares[ares$Dom==dl[d],])
        }
    }
    par(xpd=TRUE)
    legend(log(maxTE)*1.06, log(maxT)*0.6, legend=pl,col=colores, pch=19)
    legend(log(maxTE)*1.06, log(maxT)*1, legend=unique(auxres$Dom), pch=pchs)
    imprimirfin()
}

for(c in 1:length(cl)){
    auxres=compresultsraw[compresultsraw$com==cl[c],]
    pl= unique(auxres$Planner)
    colores=rainbow(length(pl))
    maxMT=max(auxres$MT)
    maxT=max(auxres$Time)
    imprimirini(typ=typu,name=paste0("Layers/", cl[c],"TvsML"),12,7.25)
    par(mar=c(5,5,3,9),xpd=FALSE)
    plot(0,type='n', xlim=c(0,maxMT), ylim=c(0,log(maxT)), xlab="Levels", ylab="Time", main=paste0("Comp: ", cl[c]) )
    for(p in 1:length(pl)){
        ares=auxres[auxres$Planner==pl[p],]
        dl=unique(ares$Dom)
        for(d in 1:length(dl)){
            points(log(Time)~MT, col=colores[p], pch=pchs[d], data=ares[ares$Dom==dl[d],])
        }
    }
    par(xpd=TRUE)
    legend(maxMT*1.06, log(maxT)*0.6, legend=pl,col=colores, pch=19)
    legend(maxMT*1.06, log(maxT)*1, legend=unique(auxres$Dom), pch=pchs)
    imprimirfin()
}
