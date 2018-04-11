/*
db.createView("nodescedges","nodes",[{$lookup:{from:"graphs", localField:"gid", foreignField:"_id", as:"gnodes"}}, {$project: { _id:"$_id", gid:"$gid", i:"$i", h:"$h", T:"$T", N:"$N", t:"$t", m:"$m", b:"$b", "n":"$n", y:"$y", oe:"$oe", ie:"$ie", de:"$de", xe:"$xe", x1e:"$x1e",coe:{$size:"$oe"},cie:{$size:"$ie"},cde:{$size:"$de"},cxe:{$size:"$xe"},cx1e:{$size:"$x1e"}, gn:"$gnodes.gn", dom:"$gnodes.dom" }}, { $addFields:{ te:{$add:["$coe","$cie","$cde","$cxe","$cx1e"]}, im:{ $cond: [{$gt:[ "$cxe",0 ] }, 1,0 ] } }  }])

db.createView("summNodesbyLevels", "nodescedges", [     {$group:      {  _id:{ gid:"$gid", dom:"$dom", T:"$T"}  , TN:{$sum:1}  , TANMN:{$sum:{$cond :[{$and:[{$eq: ["$y", "a"]},{$eq:["$im",0]}]},1,0] }}  , TAMN: {$sum:{$cond :[{$and:[{$eq: ["$y", "a"]},{$eq:["$im",1]}]},1,0] }}  , TFNMN:{$sum:{$cond :[{$and:[{$eq: ["$y", "f"]},{$eq:["$im",0]}]},1,0] }}  , TFMN: {$sum:{$cond :[{$and:[{$eq: ["$y", "f"]},{$eq:["$im",1]}]},1,0] }}      }     }, { $project:{ _id:0, gid:"$_id.gid", dom:"$_id.dom", T:"$_id.T", TN:1, TANMN:1,TAMN:1, TFNMN:1, TFMN:1}} ])
*/

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


db.summnodesbyL.aggregate({$project:{ _id:0, gid:"$_id.gid", T:"$_id.T", TN:"$value.TN",TANMN:"$value.TANMN",TAMN:"$value.TAMN",TFNMN:"$value.TFNMN",TFMN:"$value.TFMN"}},{$out:"summNodesByLevel"})

db.summnodesbyg.aggregate({$project:{ _id:0, gid:"$_id.gid", TN:"$value.TN",MT:"$value.MT",TE:"$value.TE"}},{$out:"summnodesbygc"})

/*
db.graphs.aggregate([
    {$addFields: {pkey: {$concat:["$com","-", "$dom","-","$gn" ]}}},
    {$lookup:{from:"IPCRes", localField:"pkey", foreignField:"pkey", as:"snbg" }}
    {$project:{  }}

])
*/


var mf= function(){
    emit({ gid:this.gid}, { TN:this.TN, T:this.T, TANMN:this.TANMN, TAMN:this.TAMN, TFNMN:this.TFNMN, TFMN:this.TFMN  });
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
	a1=val[idx].TANMN+val[idx].TAMN;
	a2=val[idx].TFNMN+val[idx].TFMN;
	b1=a1>0?1:0;
	b2=a2>0?1:0;
	c1=a1 - b1;
	c2=a2 - b2;
	//a1=isNaN(a1)?0:a1;
	//a2=isNaN(a2)?0:a2;
        rval.PAEM += c1;
        rval.PFEM += c2;
        rval.PEM += a1+a2;
        rval.TANMN += val[idx].TANMN;
        rval.TAMN += val[idx].TAMN;
        rval.TFNMN += val[idx].TFNMN;
        rval.TFMN += val[idx].TFMN;
        rval.TN += val[idx].TN;
    }
    
    return rval;
}

db.summNodesByLevel.mapReduce(mf,rf, "graphDen")
db.graphDen.aggregate(
    {$project:{ _id:"$_id.gid",
		TN:"$value.TN",
		PE:"$value.PE",
		PEM:"$value.PEM",
		MT:"$value.MT"
	      }},
    {$out:"graphDenComp"}
)
db.graphDen.aggregate({$project:{ _id:"$_id.gid", TN:"$value.TN",PE:"$value.PE",PM:"$value.PM",MT:"$value.MT"}},{$out:"graphDenComp"})

db.summnodesbygc.aggregate([{$lookup: {from:"graphDenComp", localField:"gid", foreignField:"_id", as: "edg"} }, {$project: {_id:1, gid:1, TN:1, MT:1, TE:{$multiply:["$TE",0.5]}, PE:"$edg.PE", PM:"$edg.PM"  } }, {$unwind:"$PE"}, {$unwind:"$PM"}, {$project: {_id:1, gid:1, TN:1, MT:1, TE:1, PE:1, PM:1, D:{$divide:[ "$TE", {$add: ["$PE","$PM"]} ]} } }, {$out: "summgraphComp"} ])

db.graphIPCRes.aggregate([{$lookup: {from:"summgraphComp", localField:"gid", foreignField:"gid", as:"snbg" }}, {$project: {_id:1, Planner:1, Dom:1, Time:1, Steps:1, comp:1, gid:1, gn:1, TN:"$snbg.TN", TE:"$snbg.TE", MT:"$snbg.MT", PE:"$snbg.PE", PM:"$snbg.PM", D:"$snbg.D" } }, {$unwind: "$TN"},{$unwind: "$TE"}, {$unwind: "$MT"}, {$unwind: "$PE"}, {$unwind: "$PM"}, {$unwind: "$D"}, {$out:"graphIPCResComp"}])

