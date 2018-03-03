library(mongolite)
library('plyr')
#install.packages('mongolite')
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
typu="png"
gpath="images/"

gcon = mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="graphs")
ncon = mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="nodes")
necon = mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="nodescedges")
glcon= mongo(db="planninggraphs", url="mongodb://ppgodel:123abc@192.168.47.10:27017",collection="summNodesbyLevels")
graphsds=gcon$find( fields='{ "_id":1, "gn":1, "dom":1, "pn":1}' )
names(graphsds)[names(graphsds) == '_id'] <- 'id'
summbyLds=data.frame(glcon$find())
colpal=c("blue","red","skyblue","orange")

xtype="m"
gtype="n"
#for
for(j in 1:nrow(graphsds)){
    wg=graphsds[j,]
    ag=summbyLds[summbyLds$gid==wg$id,]
    ag=ag[order(ag$T),]
    limval=ddply(ag, c("gid"),  summarise, MT=max(T), MN=max(TN) )
    hor=limval$MT
    vert=limval$MN
    if(gtype=="l"){
        imprimirini(typ=typu,name=paste0("nodeDistLog", wg$gn),12,7.25)
        par(mar=c(3,3,3,9),xpd=FALSE)        
        plot(TFNMN~T, data=ag, type='l', ylim=c(0, limval$MN))
        lines(TFNMN+TANMN~T, data=ag)
        lines(TFNMN+TANMN+TFMN~T, data=ag)
        lines(TFNMN+TANMN+TFMN+TAMN~T, data=ag)
        polygon(x=c(ag$T, rev(ag$T)), y=c(ag$TFNMN, ag$TFNMN*0), col=colpal[1], border=NA)
        polygon(x=c(ag$T, rev(ag$T)), y=c(ag$TFNMN, rev(ag$TFNMN+ag$TANMN)), col=colpal[2], border=NA)
        polygon(x=c(ag$T, rev(ag$T)), y=c(ag$TFNMN+ag$TANMN, rev(ag$TFNMN+ag$TANMN+ag$TFMN)), col=colpal[3], border=NA)
        polygon(x=c(ag$T, rev(ag$T)), y=c(ag$TFNMN+ag$TANMN+ag$TFMN, rev(ag$TFNMN+ag$TANMN+ag$TFMN+ag$TAMN)), col=colpal[4], border=NA)
        par(xpd=TRUE)
        legend(hor*1.06, vert*0.8, legend=c("Facts No Mut","Action No Mut","Facts Mutex","Action Mutex"),col=colpal, pch=19)
        imprimirfin()
        
    }else{
    
                                        #bylog
        if(xtype=="l"){
            vert=100*round(log(vert+1,10),2)
            zona <- matrix(rep(0, (vert * hor)), nrow = vert, ncol = hor)

            for(l in 0:(hor-1)){
                fnmn=ag[ag$T==l,]$TFNMN
                anmn=ag[ag$T==l,]$TANMN
                fmn=ag[ag$T==l,]$TFMN
                amn=ag[ag$T==l,]$TAMN

                fnmnm=100*round(log(1+fnmn,10),2)
                anmnm=100*round(log(1+fnmn+anmn,10),2)
                fmnm=100*round(log(1+fnmn+anmn+fmn,10),2)
                amnm=100*round(log(1+fnmn+anmn+fmn+amn,10),2)
                
                zona[0:fnmnm,l+1]=1
                plim=fnmnm
                zona[(plim+1):(anmnm),l+1]=2
                plim=anmnm
                zona[(plim+1):(fmnm),l+1]=3
                plim=fmnm
                zona[(plim+1):(amnm),l+1]=4
            }
            
            imprimirini(typ=typu,name=paste0("nodeDistLog", wg$gn),12,7.25)
            par(mar=c(3,3,3,9),xpd=FALSE)
            image(t(zona), col=c("white", colpal), xlim=c(-0.05,1.05), xaxt='n', yaxt='n', main=wg$gn)
            axis(side=1,at=(seq(0,hor,hor/5)/hor), labels=round(seq(0,hor,hor/5)))
            axis(side=2,at=(seq(0,vert,vert/5)/vert), labels=round(seq(0,vert,vert/5))/100)
            par(xpd=TRUE)
            legend(1.06, 0.8, legend=c("Facts No Mut","Action No Mut","Facts Mutex","Action Mutex"),col=colpal, pch=19)
            imprimirfin()
        }else{
            vert=max(ag$TANMN+ag$TAMN,ag$TFNMN+ag$TFMN)
            zona <- matrix(rep(0, (2 * vert * hor)), nrow = vert, ncol = 2 * hor)
            for(l in 0:(hor-1)){
                anmnm=ag[ag$T==l,]$TANMN
                amnm=ag[ag$T==l,]$TAMN
                fnmnm=ag[ag$T==l,]$TFNMN
                fmnm=ag[ag$T==l,]$TFMN
                
                #zona[0:fnmnm,l+1]=1
                #nlim=fnmnm
                #zona[(nlim+1):(nlim+anmnm),l+1]=2
                #nlim=nlim+anmnm
                #zona[(nlim+1):(nlim+fmnm),l+1]=3
                #nlim=nlim+fmnm
                #zona[(nlim+1):(nlim+amnm),l+1]=4

                zona[0:fnmnm,(2*l)+1]=1                
                zona[(fnmnm+1):(fnmnm+fmnm),(2*l)+1]=2
             
                zona[0:(anmnm),(2*l)+2]=3
                zona[(anmnm+1):(anmnm+amnm),(2*l)+2]=4

            }
            
            imprimirini(typ=typu,name=paste0("nodeDist", wg$gn),12,7.25)
            par(mar=c(3,3,3,9),xpd=FALSE)
            image(t(zona), col=c("white", colpal), xlim=c(-0.05,1.05), xaxt='n', yaxt='n', main=wg$gn)
            axis(side=1,at=(seq(0,hor,hor/5)/hor), labels=round(seq(0,hor,hor/5)))
            axis(side=2,at=(seq(0,vert,vert/5)/vert), labels=round(seq(0,vert,vert/5)))
            par(xpd=TRUE)
            legend(1.06, 0.8, legend=c("Facts No Mut","Action No Mut","Facts Mutex","Action Mutex"),col=colpal, pch=19)
            imprimirfin()
        }
    }
}
