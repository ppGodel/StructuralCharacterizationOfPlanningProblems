library('psych')
library('plyr')
dataraw = read.file(f="strips-gripper-all.csv")
#describe(dataraw)
tst=aggregate(id~gn+type+time, dataraw, count)
boxplot(tst$id~tst$time)

#aggregate(ixd~gn+type+time, dataraw, sum)
tst=aggregate(ixd~gn+type+time, dataraw, mean)
boxplot(tst$ixd~tst$time)

#aggregate(oxd~gn+type+time, dataraw, sum)
tst=aggregate(oxd~gn+type+time, dataraw, max)
boxplot(tst$oxd~tst$time )

