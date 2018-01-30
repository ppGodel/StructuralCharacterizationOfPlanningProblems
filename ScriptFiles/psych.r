library('psych')

typg<<-"eps"
imprimirini= function(typ, name){
    typg<<-typ
    if(typg=="eps"){
        postscript(paste(name,".eps", sep=""))
    }
    else
    {
        png(paste(name,".eps", sep=""))
    }
    
}
imprimirfin= function(){
    if(typg=="eps"){
        dev.off()
    }
    else
    {
        graphics.off()
    }
    
}

#library('plyr')
#fn="gripper"
fns=c("gripper", "log","mprime","movie")
for(fn in fns){
    dataraw = read.file(f=paste("strips-",fn,"-all.csv", sep=''))
    
    imprimirini(typ="eps",name=paste(fn,"inexclusiveMeanByLevel",sep=""))
    tst=aggregate(ixd~gn+type+time, dataraw, mean)
    boxplot(tst$ixd~tst$time)
    imprimirfin()
    
    imprimirini(typ="eps",name=paste(fn,"outexclusiveMAXByLevel",sep=""))
    tst=aggregate(oxd~gn+type+time, dataraw, max)
    boxplot(tst$oxd~tst$time )
    imprimirfin()
}
