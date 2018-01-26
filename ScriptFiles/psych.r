library('psych')
#library('plyr')
fn="gripper"
dataraw = read.file(f=paste("strips-",fn,"-all.csv", sep=''))
#describe(dataraw)
#tst=aggregate(id~gn+type+time, dataraw, count)
#boxplot(tst$id~tst$time)

#aggregate(ixd~gn+type+time, dataraw, sum)
png(paste(fn,"inexclusiveMeanByLevel.png", sep=''))
tst=aggregate(ixd~gn+type+time, dataraw, mean)
boxplot(tst$ixd~tst$time)
graphics.off()
#aggregate(oxd~gn+type+time, dataraw, sum)

png(paste(fn,"outexclusiveMAXByLevel.png", sep=''))
tst=aggregate(oxd~gn+type+time, dataraw, max)
boxplot(tst$oxd~tst$time )
graphics.off()
