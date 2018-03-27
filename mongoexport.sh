#agregar atributos a el grafo (como dominio y competencia)

mongoimport --db planninggraphs --collection IPC98 --drop --file ~/pyprojects/StructuralCharacterizationOfPlanningProblems/strips-gripper-x-3.json 


#js
var graphs=db.IPC98.find({},{"GN":1})
#remove repetead
db.IPC98.updateMany( {}, { $push: { 'VL._id':ObjectId() } })



var vertlist=db.IPC98.find().forEach(function(graph){ graph.VL.forEach(function(v){v.GN=graph.GN; v._id=v.i})}).VL
vertlist.forEach(function(item){item.gn=gid; item._id=item.i;})


data_file = open(nf)        
GNV= ijson.items(data_file, 'GN')
for gnv in GNV:
    gname=gnv
    break
    data_file = open(nf)
    vl=ijson.items(data_file, 'VL.item')
client=pymongo.MongoClient('mongodb://ppgodel:123abc@192.168.47.10:27017')
pgdb=client.planninggraphs
gm=pgdb.graphs
nm=pgdb.nodes
gn=gname.split("-")
gjson=gm.find_one({"gn":gname})
if not gobj :
    gjson={"_id":ObjectId(),"gn":gname,"dom":sgn[1], "pn":sgn[3]}
    gm.insert_one(gjson)
var oen=db.nodes.aggregate({$lookup:{from:"nodes", localField:"oe",foreignField:"_id",as:"oe_nodes"}})
oen.forEach(function(v){ var cnt=0;v.oe_nodes.forEach(function(voe){if(voe.y=='f'){ cnt++; print(v._id+','+cnt);}}); })
