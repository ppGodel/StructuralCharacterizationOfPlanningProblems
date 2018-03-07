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
pl= unique(compresultsraw$Planner)
pchs=c(3,4,1,0,2,20,21,18,10)
for(p in pl){
    auxres=compresultsraw[compresultsraw$Planner==p,]
    cl= unique(auxres$com)
    for(c in cl){
        ares=compresultsraw[compresultsraw$com==c,]
        dl=unique(ares$Dom)
        colores=rainbow(length(dl))
        imprimirini(typ=typu,name=paste0("Layers/", p, c),12,7.25)
        plot(0,type='n', xlim=c(0,max(ares$TN)), ylim=c(0,max(ares$Time)), xlab="Nodes", ylab="Time", main=paste0("Planner: ",p) )
        for(d in range(1,length(dl))){
            points(Time~TN, col=colores[d], pch=20, data=ares[ares$Dom==dl[d],])
        }
    }
}


