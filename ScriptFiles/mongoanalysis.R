#install.packages('mongolite')
#install.packages('jsonlite')
library(jsonlite)
library(mongolite)
gcon <- mongo(url = "mongodb://ppgodel:123abc@192.168.47.10:27017",db="planninggraphs", collection="graphs")
ncon <- mongo(url = "mongodb://ppgodel:123abc@192.168.47.10:27017",db="planninggraphs", collection="nodes")


glist=gcon$find( query='{"dom":"log"}', fields='{ "_id":1, "gn":1, "dom":1, "pn":1}' )
ncon$find(query='{"gid" : {"$oid":"5a8c273a9ec8e62688f97602"} }',fields={}, limit=1)
