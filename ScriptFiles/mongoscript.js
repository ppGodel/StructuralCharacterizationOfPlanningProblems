{
	"_id" : ObjectId("5a8db9089ec8e65018a57553"),
	"N" : "at_truck29_city1-2",
	"i" : "0_7524350",
	"h" : "7524350",
	"T" : 0,
	"t" : 1,
	"m" : 16,
	"b" : 6,
	"n" : 0,
	"y" : "f",
	"oe" : [
		"0_3500988",
		"0_4942048",
		"0_494856"
	],
	"ie" : [ ],
	"de" : [ ],
	"xe" : [ ],
	"x1e" : [ ],
	"gid" : ObjectId("5a8db9079ec8e65018a57552")
}


// group projection
db.nodes.aggregate([{$project: { _id:"$_id", gid:"$gid", i:"$i", h:"$h", T:"$T", N:"$N", t:"$t", m:"$m", b:"$b", "n":"$n", y:"$y",coe:{$size:"$oe"},cie:{$size:"$ie"},cde:{$size:"$de"},cxe:{$size:"$xe"},cx1e:{$size:"$x1e"}}}])

// create view
db.createView("nodescedges","nodes",[{$lookup:{from:"graphs", localField:"gid", foreignField:"_id", as:"gnodes"}}, {$project: { _id:"$_id", gid:"$gid", i:"$i", h:"$h", T:"$T", N:"$N", t:"$t", m:"$m", b:"$b", "n":"$n", y:"$y", oe:"$oe", ie:"$ie", de:"$de", xe:"$xe", x1e:"$x1e",coe:{$size:"$oe"},cie:{$size:"$ie"},cde:{$size:"$de"},cxe:{$size:"$xe"},cx1e:{$size:"$x1e"}, gn:"$gnodes.gn", dom:"$gnodes.dom" }}, { $addFields:{ te:{$add:["$coe","$cie","$cde","$cxe","$cx1e"]}, im:{ $cond: [{$gt:[ "$cxe",0 ] }, 1,0 ] } }  }])

// view graphs
db.graphs.aggregate([  { $project:{ _id:"$_id", gn:"$gn", dom:"$dom", pn:"$pn" } }  ])

// group aggregate 
db.nodes.aggregate([{$group: { _id:"$gid", Total:{$sum:1} } }, {$sort: {"Total":-1}}])
db.graphs.aggregate([{ $group: { _id:null, Total:{$sum:1} } }] )

// 
db.nodescedges.aggregate([{$group: { _id:{ gid:"$gid", dom:"$dom", T:"$T"}, TN:{$sum:1}, TAN:{$sum:{$cond :[{$eq: ["$y", "a"]},1,0] }}, TFN:{$sum:{$cond :[{$eq: ["$y", "f"]},1,0] }} } }, {$project:{ _id:0, gid:"$_id.gid", dom:"$_id.dom", T:"$_id.T", TN:1, TAN:1, TFN:1}}])
//
db.createView("summNodesbyLevels", "nodescedges", [     {$group:      {  _id:{ gid:"$gid", dom:"$dom", T:"$T"}  , TN:{$sum:1}  , TANMN:{$sum:{$cond :[{$and:[{$eq: ["$y", "a"]},{$eq:["$im",0]}]},1,0] }}  , TAMN: {$sum:{$cond :[{$and:[{$eq: ["$y", "a"]},{$eq:["$im",1]}]},1,0] }}  , TFNMN:{$sum:{$cond :[{$and:[{$eq: ["$y", "f"]},{$eq:["$im",0]}]},1,0] }}  , TFMN: {$sum:{$cond :[{$and:[{$eq: ["$y", "f"]},{$eq:["$im",1]}]},1,0] }}      }     }, { $project:{ _id:0, gid:"$_id.gid", dom:"$_id.dom", T:"$_id.T", TN:1, TANMN:1,TAMN:1, TFNMN:1, TFMN:1}} ])

db.nodescedges.aggregate([     {$group:      {  _id:{ gid:"$gid", dom:"$dom", T:"$T"}  , TN:{$sum:1}  , TANMN:{$sum:{$cond :[{$and:[{$eq: ["$y", "a"]},{$eq:["$im",0]}]},1,0] }}  , TAMN: {$sum:{$cond :[{$and:[{$eq: ["$y", "a"]},{$eq:["$im",1]}]},1,0] }}  , TFNMN:{$sum:{$cond :[{$and:[{$eq: ["$y", "f"]},{$eq:["$im",0]}]},1,0] }}  , TFMN: {$sum:{$cond :[{$and:[{$eq: ["$y", "f"]},{$eq:["$im",1]}]},1,0] }}      }     }, { $project:{ _id:0, gid:"$_id.gid", dom:"$_id.dom", T:"$_id.T", TN:1, TANMN:1,TAMN:1, TFNMN:1, TFMN:1}} ])

//map reduce
// Need 2 map and the reduce functions
// map function. need to return 2 values the key and the value
var mf= function(){emit(this.dom, 1)} 
// reduce function. uses the 2 values from the map function and summarices them
var rf= function(key,val){return Array.sum(val)}
//finaly join into a new collection
db.graphs.mapReduce(mf,rf, "mrt")


var mf= function(){
    tanmn=0;
    tfnmn=0;
    tamn=0;
    tfmn=0;
    if(this.y=="f"){
	if(this.xe.length>1){
	    tfmn++;
	}else{
	    tfnmn++;
	}
    }else{
	if(this.xe.length>1){
	    tamn++;
	}else{
	    tanmn++;
	}
    }
    emit({ gid:this.gid, dom:this.dom, T:this.T}, { TN:1, TANMN:tanmn,TAMN:tamn, TFNMN:tfnmn, TFMN:tfmn})
}
var rf= function(key,val){
    rval = { TN:0, TANMN:0,TAMN:0, TFNMN:0, TFMN:0 };
    for (var idx = 0; idx < val.length; idx++) {
        rval.TN += val[idx].TN;
        rval.TANMN += val[idx].TANMN;
        rval.TAMN += val[idx].TAMN;
        rval.TFNMN += val[idx].TFNMN;
        rval.TFMN += val[idx].TFMN;
    }
    return rval
}
db.nodes.mapReduce(mf,rf, "summnodesbyL")

db.summnodesbyL.aggregate({$project:{ _id:0, gid:"$_id.gid", T:"$_id.T", TN:"$value.TN",TANMN:"$value.TANMN",TAMN:"$value.TAMN",TFNMN:"$value.TFNMN",TFMN:"$value.TFMN"}},{$out:"summNodesByLevel"})
