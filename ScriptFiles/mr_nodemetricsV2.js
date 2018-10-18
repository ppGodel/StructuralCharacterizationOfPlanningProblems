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
    "PMFA":{$cond: [{$eq:["$y","f"]},{$divide: [{$size: "$oe"},{$cond:[{$ne:["$al.TAMN",0]},"$al.TAMN",1]} ] },0 ]},
    "PMAF":{$cond: [{$eq:["$y","a"]},{$divide: [{$size: "$oe"},{$cond:[{$ne:["$al.TFMN",0]},"$al.TFMN",1]} ] },0 ]},
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
    emit({gid:this.gid,T:this.T, Y:this.y, M:"PDE"},{count:1, sum:pde, min:pde, max:pde, sqs:Math.pow(pde,2), cus:Math.pow(pde,3), hcs:Math.pow(pde,4), mean:0, sd:0} );
    emit({gid:this.gid,T:this.T, Y:this.y, M:"PME"},{count:1, sum:pme, min:pme, max:pme, sqs:Math.pow(pme,2), cus:Math.pow(pme,3), hcs:Math.pow(pme,4), mean:0, sd:0} );
    emit({gid:this.gid,T:this.T, Y:this.y, M:"POE"},{count:1, sum:poe, min:poe, max:poe, sqs:Math.pow(poe,2), cus:Math.pow(poe,3), hcs:Math.pow(poe,4), mean:0, sd:0} );
}


var rf = function(key,val){
    rval = { count:0,
	     sum:0, min:999999, max:0, sqs:0,cus:0,hcs:0, mean:0, sd:0, kurt:0, skew:0};
    for (var idx = 0; idx < val.length; idx++) {
	rval.sum += val[idx].sum;
	rval.sqs += val[idx].sqs;
	rval.cus += val[idx].cus;
	rval.hcs += val[idx].hcs;
        rval.min = Math.min(rval.min,val[idx].min);
        rval.max = Math.max(rval.max,val[idx].max);	
        rval.count += val[idx].count;
    }
    return rval;
};


var fz = function (key, value){ 
    value.mean = value.sum / value.count;
    value.sd = Math.sqrt((value.sqs / value.count)- Math.pow(value.mean,2));
    value.skew = Math.pow(value.sd,3)*((value.cus/value.count) - (value.mean*value.sqs/value.count ));
    value.kurt = Math.pow(value.sd,4)*((value.hcs/value.count)-4*value.mean*(value.cus/value.count)+6*Math.pow(value.mean,2)*(value.sqs/value.count )-3*Math.pow(value.mean,4)) - 3;
    return value;
}

db.nodesAndLevels.mapReduce(mf,rf, {out:"summMNLbyL", finalize:fz})

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
    emit({gid:this.gid,Y:this.y, M:"PDE"},{count:1, sum:pde, min:pde, max:pde, sqs:Math.pow(pde,2), cus:Math.pow(pde,3), hcs:Math.pow(pde,4), mean:0, sd:0} );
    emit({gid:this.gid,Y:this.y, M:"PME"},{count:1, sum:pme, min:pme, max:pme, sqs:Math.pow(pme,2), cus:Math.pow(pme,3), hcs:Math.pow(pme,4), mean:0, sd:0} );
    emit({gid:this.gid,Y:this.y, M:"POE"},{count:1, sum:poe, min:poe, max:poe, sqs:Math.pow(poe,2), cus:Math.pow(poe,3), hcs:Math.pow(poe,4), mean:0, sd:0} );
}


db.nodesAndLevels.mapReduce(mf,rf, {out:"summMNLbyG", finalize:fz})

db.summMNLbyL.aggregate([{$project:{ _id:0, gid:"$_id.gid", T:"$_id.T", Y:"$_id.Y", M:"$_id.M", count:"$value.count", sum:"$value.sum", min:"$value.min", max:"$value.max",  mean:"$value.mean", sd:"$value.sd", kurt:"$value.kurt", skew:"$value.skew"}} ,{$unwind:"$count"},{$unwind:"$sum"},{$unwind:"$min"},{$unwind:"$max"},{$unwind:"$mean"},{$unwind:"$sd"},{$unwind:"$kurt"},{$unwind:"$skew"}, {$out:"summMNLbyLC"} ])

db.summMNLbyG.aggregate([{$project:{ _id:0, gid:"$_id.gid", Y:"$_id.Y", M:"$_id.M", count:"$value.count", sum:"$value.sum", min:"$value.min", max:"$value.max",  mean:"$value.mean", sd:"$value.sd", kurt:"$value.kurt", skew:"$value.skew"}} ,{$unwind:"$count"},{$unwind:"$sum"},{$unwind:"$min"},{$unwind:"$max"},{$unwind:"$mean"},{$unwind:"$sd"},{$unwind:"$kurt"},{$unwind:"$skew"},{$out:"summMNLbyGC"} ])



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
	    poe=0;
	    pde=0;
	}else{	
	    poe=this.oe.length/tan*100;
	    pde=this.de.length/tan*100;
	}
    }else{
	var tfn=this.nl.TFMN+this.nl.TFNMN;
	if(tfn==0|isNaN(tfn)){
	    tfn=1;
	    poe=0;
	    pde=0;
	}else{	
	    poe=this.oe.length/tan*100;
	    pde=this.de.length/tan*100;
	}
    }
    if(isNaN(poe)){
	poe=0
    }
    emit({_id:this._id,gid:this.gid,T:this.T, Y:this.y}, { POE:poe, PME:pme, PDE:pde })
}
var rf = function(key,val){
    return val;
};

db.nodesAndLevels.mapReduce(mf,rf, {out:"nodesAndLevelsSumm"})

db.nodesAndLevelsSumm.aggregate([{$project:{_id:"$_id._id", gid:"$_id.gid", T:"$_id.T", Y:"$_id.Y", POE:"$value.POE", PDE:"$value.PDE", PME:"$value.PME"}},{$out:"nodePercentageEdges"}])
