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


db.IPC00Res.aggregate([{$group:{_id:"$Problem", mTime:{$min:"$Time"}, mSteps:{$min:"$Steps"} }}, {$lookup:{from:"graphs", localField:"_id", foreignField:"gn", as:"graphwres"}}, {$project:{_id:"$graphwres._id", gn:"$_id",dom:"$graphwres.dom", pn:"$graphwres.pn", com:"$graphwres.com", mTime:"$mTime", mSteps:"$mSteps" }},{$unwind:"$_id"},{$unwind:"$gn"},{$unwind:"$dom"},{$unwind:"$pn"}, {$unwind:"$com"}])

db.createView("graphRes","IPC00Res",[{$group:{_id:"$Problem", mTime:{$min:"$Time"}, mSteps:{$min:"$Steps"} }}, {$lookup:{from:"graphs", localField:"_id", foreignField:"gn", as:"graphwres"}}, {$project:{_id:"$graphwres._id", gn:"$_id",dom:"$graphwres.dom", pn:"$graphwres.pn", com:"$graphwres.com", mTime:"$mTime", mSteps:"$mSteps" }},{$unwind:"$_id"},{$unwind:"$gn"},{$unwind:"$dom"},{$unwind:"$pn"}, {$unwind:"$com"}])

db.graphRes.aggregate([{$lookup:{from:"summNodesByLevel", localField:"_id", foreignField:"gid",as:"graphResByLevel"}}]).pretty()

db.graphResByLevel.insertMany(db.summNodesByLevel.aggregate([{$lookup:{from:"graphRes", localField:"gid", foreignField:"_id",as:"graphResByLevel"}},{$match:{"graphResByLevel":{$ne:[]}}},{$project:{_id:1,gid:1,T:1,TN:1,TANMN:1,TAMN:1,TFNMN:1,TFMN:1, gn:"$graphResByLevel.gn",dom:"$graphResByLevel.dom", com:"$graphResByLevel.com", mTime:"$graphResByLevel.mTime", mSteps:"$graphResByLevel.dom"}}, {$unwind: "$gn"},{$unwind: "$dom"},{$unwind: "$com"}, {$unwind: "$mTime"},{$unwind: "$mSteps"}]).toArray())


db.nodes.aggregate([{$group: {_id:"$gid", TN:{$sum:1}, MT:{$max:"$T"}, TE:{$sum:{$add:[{$size:"$oe"},{$size:"$ie"},{$size:"$de"},{$size:"$xe"},{$size:"$x1e"} ]}} }}])

// map reduce graph summ
var mf= function(){
    emit({ gid:this.gid}, { TN:1, T:this.T,TE:this.oe.length+this.ie.length+this.de.length+this.xe.length+this.x1e.length})
}
var rf= function(key,val){
    rval = { TN:0, MT:0,TE:0 };
    mv=0
    for (var idx = 0; idx < val.length; idx++) {
        rval.TN += val[idx].TN;
        rval.TE+= val[idx].TE;
	if(rval.MT<val[idx].T){
		rval.MT=val[idx].T
	}
    }
    
    return rval
}
db.nodes.mapReduce(mf,rf, "summnodesbyg") 


db.graphIPCRes.aggregate([{$lookup: {from:"summnodesbyg", localField:"gid", foreignField:"_id.gid", as:"snbg" }}])
db.graphIPCRes.aggregate([{$lookup: {from:"summnodesbygc", localField:"gid", foreignField:"gid", as:"snbg" }}])


var mf= function(){
    emit({ gid:this.gid}, { TN:this.TN, T:this.T, TANMN:this.TANMN, TAMN:this.TAMN, TAN:this.TANMN+this.TAMN, TFNMN:this.TFNMN, TFMN:this.TFMN, TFN:this.TFNMN+this.TFMN   });
}
var rf= function(key,val){
    rval = { MT:0, PE:0, PEM:0, PAEM:0, PFEM:0, TANMN:0, TAMN:0, TFNMN:0, TFMN:0, TN:0 };
    pval=0;
    for (var idx = 0; idx < val.length; idx++) {
	if(rval.MT<val[idx].T){
	    rval.MT=val[idx].T;
	}
        rval.PE += val[idx].TN*pval;
	pval=val[idx].TN;
        rval.PAEM += val[idx].TAN - (val[idx].TAN>0?1:0);
        rval.PFEM += val[idx].TFN - (val[idx].TFN>0?1:0);
        rval.PEM += val[idx].TFN + val[idx].TAN - (val[idx].TAN>0?1:0) - (val[idx].TFN>0?1:0);
        rval.TANMN += val[idx].TANMN;
        rval.TAMN += val[idx].TAMN;
        rval.TFNMN += val[idx].TFNMN;
        rval.TFMN += val[idx].TFMN;
        rval.TN += val[idx].TN;
    }
    
    return rval;
}

var mf= function(){
    emit({ gid:this.gid}, { TN:this.TN, T:this.T, TANMN:this.TANMN, TAMN:this.TAMN, TAN:this.TANMN+this.TAMN   });
}
var rf= function(key,val){
    rval = { MT:0, PE:0, TAMN:0, TANMN:0, PAEM:0, TN:0 };//, TFNMN:0, TFMN:0, PFEM:00
    pval=0;
    for (var idx = 0; idx < val.length; idx++) {
	if(rval.MT<val[idx].T){
	    rval.MT=val[idx].T;
	}
        rval.PE += val[idx].TN*pval;
	pval=val[idx].TN;
        rval.TN += val[idx].TN;
	
        rval.PAEM += val[idx].TAN - (val[idx].TAN>0?1:0);
        //rval.PFEM += val[idx].TFN - (val[idx].TFN>0?1:0);
        rval.TANMN += val[idx].TANMN;
        rval.TAMN += val[idx].TAMN;
        //rval.TFNMN += val[idx].TFNMN;
        //rval.TFMN += val[idx].TFMN;
	
    }
    
    return rval;
}
db.summNodesByLevel.mapReduce(mf,rf, "graphDen")


db.graphDen.aggregate({$project:{ _id:"$_id.gid", TN:"$value.TN",PE:"$value.PE",PM:"$value.PM",MT:"$value.MT"}},{$out:"graphDenComp"})

db.summnodesbygc.aggregate([{$lookup: {from:"graphDenComp", localField:"gid", foreignField:"_id", as: "edg"} }, {$project: {_id:1, gid:1, TN:1, MT:1, TE:{$multiply:["$TE",0.5]}, PE:"$edg.PE", PM:"$edg.PM"  } }, {$unwind:"$PE"}, {$unwind:"$PM"}, {$project: {_id:1, gid:1, TN:1, MT:1, TE:1, PE:1, PM:1, D:{$divide:[ "$TE", {$add: ["$PE","$PM"]} ]} } }, {$out: "summgraphComp"} ])



db.graphIPCRes.aggregate([{$lookup: {from:"summgraphComp", localField:"gid", foreignField:"gid", as:"snbg" }}, {$project: {_id:1, Planner:1, Dom:1, Time:1, Steps:1, comp:1, gid:1, gn:1, TN:"$snbg.TN", TE:"$snbg.TE", MT:"$snbg.MT", PE:"$snbg.PE", PM:"$snbg.PM", D:"$snbg.D" } }, {$unwind: "$TN"},{$unwind: "$TE"}, {$unwind: "$MT"}, {$unwind: "$PE"}, {$unwind: "$PM"}, {$unwind: "$D"}, {$out:"graphIPCResComp"}])
