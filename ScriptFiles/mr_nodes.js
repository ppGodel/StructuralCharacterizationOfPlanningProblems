// map-reduce nodes by level
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
};
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
};
db.nodes.mapReduce(mf,rf, "summnodesbyL");
// format summnodesbyL into summNodesByLevel
db.summnodesbyL.aggregate({$project:{ _id:0, gid:"$_id.gid", T:"$_id.T", TN:"$value.TN",TANMN:"$value.TANMN",TAMN:"$value.TAMN",TFNMN:"$value.TFNMN",TFMN:"$value.TFMN"}},{$out:"summNodesByLevel"})

//Join summNodesByLevel with graphRes to unify level features and graph results
db.summNodesByLevel.aggregate([{$lookup:{from:"graphRes", localField:"gid", foreignField:"_id",as:"graphResByLevel"}},{$match:{"graphResByLevel":{$ne:[]}}},{$project:{_id:1,gid:1,T:1,TN:1,TANMN:1,TAMN:1,TFNMN:1,TFMN:1, gn:"$graphResByLevel.gn",dom:"$graphResByLevel.dom", com:"$graphResByLevel.com", mTime:"$graphResByLevel.mTime", mSteps:"$graphResByLevel.dom"}}, {$unwind: "$gn"},{$unwind: "$dom"},{$unwind: "$com"}, {$unwind: "$mTime"},{$unwind: "$mSteps"}, {$out:grpahResByLevel}])

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
