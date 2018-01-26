library('psych')
dataraw = read.file(f="strips-gripper-all.csv")
#describe(dataraw)
aggregate(cbind(iid,odd)~gn, dat1, mean)


library('plyr')
