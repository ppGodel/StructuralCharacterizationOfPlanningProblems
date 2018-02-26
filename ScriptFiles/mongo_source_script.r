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
colpal=c("blue","skyblue","red","orange")

#for
for(j in 1:nrow(graphsds)){
    wg=graphsds[j,]
    ag=summbyLds[summbyLds$gid==wg$id,]
    ag=ag[order(ag$T),]
    limval=ddply(ag, c("gid"),  summarise, MT=max(T), MN=max(TN) )
    hor=limval$MT
    vert=limval$MN
                                        #lines
                                        #plot(TAN~T, data=ag, type='l', ylim=c(0, limval$MN))
                                        #lines(TAN+TFN~T, data=ag)
                                        #matrix
    zona <- matrix(rep(0, vert * hor), nrow = vert, ncol = hor)


    for(l in 0:(hor-1)){
        anmnm=ag[ag$T==l,]$TANMN
        amnm=ag[ag$T==l,]$TAMN
        fnmnm=ag[ag$T==l,]$TFNMN
        fmnm=ag[ag$T==l,]$TFMN
        zona[0:fnmnm,l+1]=1
        nlim=fnmnm
        zona[(nlim+1):(nlim+anmnm),l+1]=2
        nlim=nlim+anmnm
        zona[(nlim+1):(nlim+fmnm),l+1]=3
        nlim=nlim+fmnm
        zona[(nlim+1):(nlim+amnm),l+1]=4
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
