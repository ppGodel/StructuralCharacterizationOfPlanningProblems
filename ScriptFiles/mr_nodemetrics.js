
db.nodes.aggregate([{$lookup: {from:"summNodesByLevel", localField:"gid", foreignField:"gid", as: "edg"} }, {$project: {_id:1, gid:1, TN:1, MT:1, TE:1, TME:1, PE:"$edg.PE", PME:"$edg.PME"  } }, {$unwind:"$PE"}, {$unwind:"$PME"}, {$project: {_id:1, gid:1, TN:1, MT:1, TE:1, TME:1, PE:1, PME:1, D:{$divide:[ "$TE", "$PE" ]}, DM:{$divide:[ "$TME", "$PME" ]} } }, {$out: "summgraphComp"} ])


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



db.nodes.aggregate([
    { "$lookup": { 
        "from": "summNodesByLevel", 
        "localField": "gid", 
        "foreignField": "gid", 
        "as": "al"
    }}, 
    { "$unwind": "$al" },
    { "$redact": { 
        "$cond": [
            { "$eq": [ "$T", "$al.T" ] }, 
            "$$KEEP", 
            "$$PRUNE"
        ]
    }},
    { "$lookup": { 
        "from": "summNodesByLevel", 
        "localField": "gid", 
        "foreignField": "gid", 
        "as": "nl"
    }}, 
    { "$unwind": "$nl" },
    { "$redact": { 
        "$cond": [
            { "$eq": [ "$T", {$subtract:["$nl.T",1]} ] }, 
            "$$KEEP", 
            "$$PRUNE"
        ]
    }},     
    { "$addFields": { 
	"PM": {$cond: [{$eq:["$y","f"]},{$divide: [{$size: "$xe"},{$cond:[{$ne:["$al.TFMN",0]},"$al.TFMN",1]} ] },{$divide: [{$size: "$xe"},{$cond:[{$ne:["$al.TAMN",0]},"$al.TAMN",1]}] }]},
	"PFA": {
	    $let: {
               vars: {
                  tanl: { $add: [ "$al.TAMN", "$al.TANMN" ] },
               },
		    in: {$cond: [{$eq:["$y","f"]},{$divide: [{$size: "$oe"},{$cond:[{$ne:["$$tanl",0]},"$$tanl",1]} ] },0 ]}
	    }
	},
	"PAF": {
	    $let: {
               vars: {
                  tfnl: { $add: [ "$nl.TFMN", "$al.TFNMN" ] },
               },
		    in: {$cond: [{$eq:["$y","a"]},{$divide: [{$size: "$oe"},{$cond:[{$ne:["$$tfnl",0]},"$$tfnl",1]} ] },0 ]}
	    }
	}
    }},
    {$out:"nodesAndLevels"}
])



var mf= function(){
    var poe=0;
    var pde=0;
    var pme=this.PM*100;
    if(this.y=="f"){
	var tan=this.al.TAMN+this.al.TANMN;
	if(tan==0|isNaN(tan)){
	    tan=1;
	}	
	poe=this.oe.length/tan*100;
	pde=this.de.length/tan*100;	
    }else{
	var tfn=this.nl.TFMN+this.nl.TFNMN;
	if(tfn==0|isNaN(tfn)){
	    tfn=1;
	}
	poe=this.oe.length/tfn*100;
	pde=this.de.length/tfn*100;
	if(isNaN(poe)){
	    poe=0
	}
    }
    emit({gid:this.gid,T:this.T, Y:this.y}, {count:1, sumPME:pme, minPME:pme, maxPME:pme, sqsPME:Math.pow(pme,2),cusPME:Math.pow(pme,3),hcsPME:Math.pow(pme,4), meanPME:0, sdPME:0, kurtPME:0, skewPME:0, sumPOE:poe, minPOE:poe, maxPOE:poe, sqsPOE:Math.pow(poe,2),cusPOE:Math.pow(poe,3),hcsPOE:Math.pow(poe,4), meanPOE:0, sdPOE:0, kurtPOE:0, skewPOE:0, sumPDE:pde, minPDE:pde, maxPDE:pde, sqsPDE:Math.pow(pde,2), cusPDE:Math.pow(pde,3), hcsPDE:Math.pow(pde,4), meanPDE:0, sdPDE:0})
}

var rf = function(key,val){
    rval = { count:0,
	     sumPME:0, minPME:999999, maxPME:0, sqsPME:0,cusPME:0,hcsPME:0, meanPME:0, sdPME:0, kurtPME:0, skewPME:0,
	     sumPOE:0, minPOE:999999, maxPOE:0, sqsPOE:0,cusPOE:0,hcsPOE:0, meanPOE:0, sdPOE:0, kurtPOE:0, skewPOE:0,
	     sumPDE:0, minPDE:999999, maxPDE:0, sqsPDE:0,cusPDE:0,hcsPDE:0, meanPDE:0, sdPDE:0, kurtPDE:0, skewPDE:0};
    for (var idx = 0; idx < val.length; idx++) {
	rval.sumPOE += val[idx].sumPOE;
	rval.sqsPOE += val[idx].sqsPOE;
	rval.cusPOE += val[idx].cusPOE;
	rval.hcsPOE += val[idx].hcsPOE;
        rval.minPOE = Math.min(rval.minPOE,val[idx].minPOE);
        rval.maxPOE = Math.max(rval.maxPOE,val[idx].maxPOE);
	
	rval.sumPDE += val[idx].sumPDE;
	rval.sqsPDE += val[idx].sqsPDE;
	rval.cusPDE += val[idx].cusPDE;
	rval.hcsPDE += val[idx].hcsPDE;
        rval.minPDE = Math.min(rval.minPDE,val[idx].minPDE);
        rval.maxPDE = Math.max(rval.maxPDE,val[idx].maxPDE);
	
	rval.sumPME += val[idx].sumPME;
	rval.sqsPME += val[idx].sqsPME;
	rval.cusPME += val[idx].cusPME;
	rval.hcsPME += val[idx].hcsPME;
        rval.minPME = Math.min(rval.minPME,val[idx].minPME);
        rval.maxPME = Math.max(rval.maxPME,val[idx].maxPME);
        rval.count += val[idx].count;
    }
    return rval;
};

var fz = function (key, value){ 
    value.meanPOE = value.sumPOE / value.count;
    value.sdPOE = Math.sqrt((value.sqsPOE / value.count)- Math.pow(value.meanPOE,2));
    value.skewPOE = Math.pow(value.sdPOE,3)*((value.cusPOE/value.count) - (value.meanPOE*value.sqsPOE/value.count ));
    value.kurtPOE = Math.pow(value.sdPOE,4)*((value.hcsPOE/value.count)-4*value.meanPOE*(value.cusPOE/value.count)+6*Math.pow(value.meanPOE,2)*(value.sqsPOE/value.count )-3*Math.pow(value.meanPOE,4)) - 3;
    
    value.meanPDE = value.sumPDE / value.count;
    value.sdPDE = Math.sqrt((value.sqsPDE / value.count)- Math.pow(value.meanPDE,2));
    value.skewPDE = Math.pow(value.sdPDE,3)*((value.cusPDE/value.count) - (value.meanPDE*value.sqsPDE/value.count ));
    value.kurtPDE = Math.pow(value.sdPDE,4)*((value.hcsPDE/value.count)-4*value.meanPDE*(value.cusPDE/value.count)+6*Math.pow(value.meanPDE,2)*(value.sqsPDE/value.count )-3*Math.pow(value.meanPDE,4)) - 3;
    
    value.meanPME = value.sumPME / value.count;
    value.sdPME = Math.sqrt((value.sqsPME / value.count)- Math.pow(value.meanPME,2));
    value.skewPME = Math.pow(value.sdPME,3)*((value.cusPME/value.count) - (value.meanPME*value.sqsPME/value.count ));
    value.kurtPME = Math.pow(value.sdPME,4)*((value.hcsPME/value.count)-4*value.meanPME*(value.cusPME/value.count)+6*Math.pow(value.meanPME,2)*(value.sqsPME/value.count )-3*Math.pow(value.meanPME,4)) - 3;
    return value;
}

db.nodesAndLevels.mapReduce(mf,rf, {out:"summNLbyL", finalize:fz})

var mf= function(){
    var poe=0;
    var pde=0;
    var pme=this.PM*100;
    if(this.y=="f"){
	var tan=this.al.TAMN+this.al.TANMN;
	if(tan==0|isNaN(tan)){
	    tan=1;
	}	
	poe=this.oe.length/tan*100;
	pde=this.de.length/tan*100;	
    }else{
	var tfn=this.nl.TFMN+this.nl.TFNMN;
	if(tfn==0|isNaN(tfn)){
	    tfn=1;
	}
	poe=this.oe.length/tfn*100;
	pde=this.de.length/tfn*100;
	if(isNaN(poe)){
	    poe=0
	}
    }
    emit({gid:this.gid, Y:this.y}, {count:1, sumPME:pme, minPME:pme, maxPME:pme, sqsPME:Math.pow(pme,2),cusPME:Math.pow(pme,3),hcsPME:Math.pow(pme,4), meanPME:0, sdPME:0, kurtPME:0, skewPME:0, sumPOE:poe, minPOE:poe, maxPOE:poe, sqsPOE:Math.pow(poe,2),cusPOE:Math.pow(poe,3),hcsPOE:Math.pow(poe,4), meanPOE:0, sdPOE:0, kurtPOE:0, skewPOE:0, sumPDE:pde, minPDE:pde, maxPDE:pde, sqsPDE:Math.pow(pde,2), cusPDE:Math.pow(pde,3), hcsPDE:Math.pow(pde,4), meanPDE:0, sdPDE:0})
}


db.nodesAndLevels.mapReduce(mf,rf, {out:"summNLbyG", finalize:fz})

db.summNLbyL.aggregate([{$project:{ _id:0, gid:"$_id.gid", T:"$_id.T", Y:"$_id.Y", count:"$value.count", sumPOE:"$value.sumPOE", minPOE:"$value.minPOE", maxPOE:"$value.maxPOE",  meanPOE:"$value.meanPOE", sdPOE:"$value.sdPOE", kurtPOE:"$value.kurtPOE", skewPOE:"$value.skewPOE", sumPDE:"$value.sumPDE", minPDE:"$value.minPDE", maxPDE:"$value.maxPDE", meanPDE:"$value.meanPDE", sdPDE:"$value.sdPDE", kurtPDE:"$value.kurtPDE", skewPDE:"$value.skewPDE", sumPME:"$value.sumPME", minPME:"$value.minPME", maxPME:"$value.maxPME",  meanPME:"$value.meanPME", sdPME:"$value.sdPME", kurtPME:"$value.kurtPME", skewPME:"$value.skewPME"}} ,{$unwind:"$count"},{$unwind:"$sumPOE"},{$unwind:"$minPOE"},{$unwind:"$maxPOE"},{$unwind:"$meanPOE"},{$unwind:"$sdPOE"},{$unwind:"$kurtPOE"},{$unwind:"$skewPOE"},{$unwind:"$sumPDE"},{$unwind:"$minPDE"},{$unwind:"$maxPDE"},{$unwind:"$meanPDE"},{$unwind:"$sdPDE"},{$unwind:"$kurtPDE"},{$unwind:"$skewPDE"},{$unwind:"$sumPME"},{$unwind:"$minPME"},{$unwind:"$maxPME"},{$unwind:"$meanPME"},{$unwind:"$sdPME"},{$unwind:"$kurtPME"},{$unwind:"$skewPME"}, {$out:"summNLbyLC"} ])

db.summNLbyG.aggregate([{$project:{ _id:0, gid:"$_id.gid", Y:"$_id.Y", count:"$value.count", sumPOE:"$value.sumPOE", minPOE:"$value.minPOE", maxPOE:"$value.maxPOE",  meanPOE:"$value.meanPOE", sdPOE:"$value.sdPOE", kurtPOE:"$value.kurtPOE", skewPOE:"$value.skewPOE", sumPDE:"$value.sumPDE", minPDE:"$value.minPDE", maxPDE:"$value.maxPDE", meanPDE:"$value.meanPDE", sdPDE:"$value.sdPDE", kurtPDE:"$value.kurtPDE", skewPDE:"$value.skewPDE", sumPME:"$value.sumPME", minPME:"$value.minPME", maxPME:"$value.maxPME",  meanPME:"$value.meanPME", sdPME:"$value.sdPME", kurtPME:"$value.kurtPME", skewPME:"$value.skewPME"}} ,{$unwind:"$count"},{$unwind:"$sumPOE"},{$unwind:"$minPOE"},{$unwind:"$maxPOE"},{$unwind:"$meanPOE"},{$unwind:"$sdPOE"},{$unwind:"$kurtPOE"},{$unwind:"$skewPOE"},{$unwind:"$sumPDE"},{$unwind:"$minPDE"},{$unwind:"$maxPDE"},{$unwind:"$meanPDE"},{$unwind:"$sdPDE"},{$unwind:"$kurtPDE"},{$unwind:"$skewPDE"},{$unwind:"$sumPME"},{$unwind:"$minPME"},{$unwind:"$maxPME"},{$unwind:"$meanPME"},{$unwind:"$sdPME"},{$unwind:"$kurtPME"},{$unwind:"$skewPME"}, {$out:"summNLbyGC"} ])



db.nodesAndLevels.aggregate([{$project:{
    _id:1, gid:1,
    T:1,
    "PM": {$cond: [{$eq:["$y","f"]},{$divide: [{$size: "$xe"},{$cond:[{$ne:["$al.TFMN",0]},"$al.TFMN",1]} ] },{$divide: [{$size: "$xe"},{$cond:[{$ne:["$al.TAMN",0]},"$al.TAMN",1]}] }]},    
    "POE": {
	$let: {
            vars: {
                tanl: { $add: [ "$al.TAMN", "$al.TANMN" ] },
                tfnl: { $add: [ "$nl.TFMN", "$al.TFNMN" ] },
            },
		in: {$cond: [{$eq:["$y","f"]},{$divide: [{$size: "$oe"},{$cond:[{$ne:["$$tanl",0]},"$$tanl",1]} ] }, {$divide: [{$size: "$oe"},{$cond:[{$ne:["$$tfnl",0]},"$$tfnl",1]} ] }  ]}
	}
    },
    "PDE": {
	$let: {
            vars: {
                tanl: { $add: [ "$al.TAMN", "$al.TANMN" ] },
                tfnl: { $add: [ "$nl.TFMN", "$al.TFNMN" ] },
            },
		in: {$cond: [{$eq:["$y","f"]},{$divide: [{$size: "$de"},{$cond:[{$ne:["$$tanl",0]},"$$tanl",1]} ] }, {$divide: [{$size: "$de"},{$cond:[{$ne:["$$tfnl",0]},"$$tfnl",1]} ] }  ]}
	}
    },
}}, {$out: "nodesAndLevelsSumm"} ])


var mf= function(){
    var poe=0;
    var pde=0;
    var pme=this.PM*100;
    if(this.y=="f"){
	var tan=this.al.TAMN+this.al.TANMN;
	if(tan==0|isNaN(tan)){
	    tan=1;
	}	
	poe=this.oe.length/tan*100;
	pde=this.de.length/tan*100;	
    }else{
	var tfn=this.nl.TFMN+this.nl.TFNMN;
	if(tfn==0|isNaN(tfn)){
	    tfn=1;
	}
	poe=this.oe.length/tfn*100;
	pde=this.de.length/tfn*100;
	if(isNaN(poe)){
	    poe=0
	}
    }
    emit({_id:this._id, gid:this.gid,T:this.T, Y:this.y}, { POE:poe, PME:pme, PDE:pde })
}
var rf = function(key,val){
    return val;
};

db.nodesAndLevels.mapReduce(mf,rf, {out:"nodesAndLevelsSumm"})
