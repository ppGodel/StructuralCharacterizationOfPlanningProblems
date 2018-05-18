
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
    poe=0;
    pde=0;    //des=this.de.length;
    if(this.y=="f"){
	tan=this.al.TAMN+this.al.TANMN
	if(tan==0){
	    tan=1
	}	
	poe=this.oe.length/tan;
	pde=this.de.length/tan;	
    }else{
	tfn=this.nl.TFMN+this.nl.TFNMN
	if(tfn==0){
	    tfn=1
	}
	poe=this.oe.length/tfn;
	pde=this.de.length/tfn;
    }
    emit({ gid:this.gid, T:this.T, Y:this.y}, { TN:1, POE:poe })
}
//, PDE:pde,PM:this.PM
//, sumPDE:0, minPDE:999999, maxPDE:0, meanPDE:0, sdPDE:0, kurPDE:0, skwePDE:0, sumPME:0, minPME:999999, maxPME:0, meanPME:0, sdPME:0, kurPME:0, skwePME:0
var rf= function(key,val){
    rval = { N:0, sumPOE:0, minPOE:999999, maxPOE:0, sqsPOE:0,cusPOE:0,hcsPOE:0, meanPOE:0, sdPOE:0 };
    for (var idx = 0; idx < val.length; idx++) {	
        rval.minPOE = Math.min(rval.minPOE,val[idx].POE);
        rval.maxPOE = Math.max(rval.maxPOE,val[idx].POE);
        rval.N += val[idx].N;
	rval.sumPOE+=val[idx].POE
	rval.sqsPOE+=(val[idx].POE)**2
	rval.cusPOE+=(val[idx].POE)**3
	rval.hcsPOE+=(val[idx].POE)**4
    }
    return rval
}

function fz(key, value){ 
    value.meanPOE = value.sumPOE / value.N;
    value.sdPOE = Math.sqrt((value.sqsPOE / value.N)- value.avg**2);
    value.skewPOE = (value.sdPOE**3)*((value.cusSum/value.N) - (value.avg*value.sqsPOE/value.N ));
    value.kurtPOE = (value.sdPOE**4)*((value.hcsSum/value.N)-4*value.avg*(value.cusSum/value.N)+6*(value.avg**2)*(value.sqsPOE/value.N )-3*value.avg**4);
    return value;
}



db.nodesAndLevels.mapReduce(mf,rf, {out:"summNLbyL", finalize:fz})


,
    {$group:{
	_id:{ gid:"$gid", T:"$T"},
	minPM: {$min: "$PM"},
	meanPM: {$avg: "$PM"},
	maxPM: {$max: "$PM"},
	SDPM: {$stdDevPop : "$PM"},	
	NPM: {$sum: 1},
	PMList: {$push:{ soe:"$PM"}}
    }},




{$cond[{$neq:["",0]},,0]}
{$cond: [{$eq:["$y","f"]},{$divide: [{$size: "$oe"},{$cond:[{$ne:["$c2.TAMN",0]},"$c2.TAMN",1]} ] },0 ]}

,
	kurPM: {
	    $let : {
		vars: {
		    nv: {$sum: 1},
		    sd: {$$stdDevPop : "$PM"},		    
		    mv: {$avg: "$PM"},
		},
		    in: { $divide:  [{ $divide:  [{ $pow: [ {$sum:{ $subtract: ["$PM","$$mv" ] }},3] }, "$$nv"] }, { $pow: [ "$$sd",3] }]}
		
	    }
	}
