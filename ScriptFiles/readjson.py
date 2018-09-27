import pymongo
import json
import graph
import sys
import time
import os.path
import os
import ijson
from bson.objectid import ObjectId
from pprint import pprint

def readPGjson(nf = 'graph.json', wm=True, option=1):
    print("loading file")
    gname=""
    vl=list()
    if option == 1:
        with open(nf) as data_file:    
            data = json.load(data_file)
            gname=data['Name']
            vl=data['Vertexlist']
    else:#elif option==2:
        data_file = open(nf)        
        GNV= ijson.items(data_file, 'Name')
        for gnv in GNV:
            gname=gnv
            break
        data_file = open(nf)
        vl=ijson.items(data_file, 'Vertexlist.item')
    

    uh=set()
    print("File loaded, starting parse")                    
    g= graph.Graph(gname, directed=True)
    for e in vl:
        #read atribute vertex
        nid = e['i']#str(int(e["time"])) + "_" + e['hash']        
        if nid not in uh:
            v = graph.Vertex(e['i'], Name = e['N'], time =e['T'], type=e['y'], hash=e['h'])
        else:
            v = g[nid]
            v.label['Name'] = e['N']
            v.label['hash'] = e['h']
            v.label['is_true'] = e['t']
            v.label['uid_mask'] = e['m']
            v.label['uid_block'] = e['b']
            v.label['cant_do'] = e['n']
            v.label['type'] = e['y']
            
            
        if 'flag' in e.keys():
            v.label['flag'] = e['f']
        #add it to the graph
                       
        g.add_vertex(v)
        if(v.label['time']!=0):
            for inn in e['ie']:
                #if inn != e['h']:
                    #kid = str(int(v.label["time"])-1) + "_" + inn['hash']
                    if(inn not in g.vertices.keys()):
                        w = graph.Vertex(inn, Name = 'NN', time =int(v.label["time"])-(1 if v.label['type']=='f' else 0))
                        g.add_vertex(w)
                    else:
                        w=g[inn]            
                        g.add_edge(w, v, weight=1, name = w.label['Name']+ '___' + v.label['Name'], type='I')
        for outn in e['oe']:
            #if outn != e['h']:
                #kid = str(int(v.label["time"])+1) + "_" + outn['hash']
                if(outn not in g.vertices.keys()):
                    w = graph.Vertex(outn, Name = 'NN', time =int(v.label["time"])+(0 if v.label['type']=='f' else 1) )
                    g.add_vertex(w)
                else:
                    w=g[outn]            
                    g.add_edge(v, w, weight=1, name = v.label['Name'] + '___' + w.label['Name'], type='O')
                    #g.add_edge(v,str(int(v.label["time"])+1) + "_" + outn['hash'], weight=1, name = outn['name'],hash=outn['hash'])
        for deln in e['de']:
            #if deln != e['h']:
                #kid = str(int(v.label["time"])+1) + "_" + deln['hash']
                if(deln not in g.vertices.keys()):
                    w = graph.Vertex(deln, Name = 'NN', time =int(v.label["time"])+(-1 if v.label['type']=='f' else 1))
                    g.add_vertex(w)
                else:
                    w=g[deln]            
                    g.add_edge(v, w, weight=1, name = v.label['Name'] + '___' + w.label['Name'], type='D')
        if wm:
            for excn in e['xe']:
                #if excn != e['h']:
                    #kid = str(int(v.label["time"])+1) + "_" + excn['hash']
                    if(excn not in g.vertices.keys()):
                        w = graph.Vertex(excn, Name = 'NN', time =int(v.label["time"]))
                        g.add_vertex(w)
                    else:
                        w=g[excn]        
                        g.add_edge(v, w, weight=1, name = v.label['Name'] + '___' + w.label['Name'], type='X')    
                        #g.add_edge(v, w, weight=1, name = v.label['Name'] + '___' + excn['name'] ,hash=excn['hash'], type='X')
        for eisn in e['x1e']:
            #if eisn != e['h']:
                #kid = str(int(v.label["time"])+1) + "_" + eisn['hash']
                if(eisn not in g.vertices.keys()):
                    w = graph.Vertex(eisn, Name = 'NN', time =int(v.label["time"]))
                    g.add_vertex(w)
                else:
                    w=g[eisn]            
                    g.add_edge(v, w, weight=1, name = v.label['Name'] + '___' + w.label['Name'], type='S')    
                    #g.add_edge(v, w, weight=1, name = v.label['Name']+ '___' + eisn['name'] ,hash=eisn['hash'], type='S')
    print("Parse finished")
    return g
                    
def do_analysis(g, filename):
    print("starting analysis of " + str(len(g.vertices)) + "nodes")
    filest = open(filename,'w')
    filest.write('id'+','+'gn'+','+ 'name'+','+ 'hash'+ ','+ 'time'+ ','+ 'type'+','
                 + 'ind'+ ',' + 'iid' + ','+ 'iod' + ','+ 'idd' + ','+ 'ixd' + ','+ 'isd' +','
                 + 'otd'+ ',' + 'oid' + ','+ 'ood' + ','+ 'odd' + ','+ 'oxd' + ','+ 'osd' +','
                 + 'ad'+ ',' + 'bc' + '\n')
    for e in g.vertices: #bc:#
        an = len(set(g[e].neighbors.keys()).union(set(g[e].inneighbors.keys())))
        ii = len([x for x in g[e].inneighbors if g[e].inneighbors[x]['type']=='I'])
        oi = len([x for x in g[e].inneighbors if g[e].inneighbors[x]['type']=='O'])
        di = len([x for x in g[e].inneighbors if g[e].inneighbors[x]['type']=='D'])
        xi = len([x for x in g[e].inneighbors if g[e].inneighbors[x]['type']=='X'])
        si = len([x for x in g[e].inneighbors if g[e].inneighbors[x]['type']=='S'])
        io = len([x for x in g[e].neighbors if g[e].neighbors[x]['type']=='I'])
        oo = len([x for x in g[e].neighbors if g[e].neighbors[x]['type']=='O'])
        do = len([x for x in g[e].neighbors if g[e].neighbors[x]['type']=='D'])
        xo = len([x for x in g[e].neighbors if g[e].neighbors[x]['type']=='X'])
        so = len([x for x in g[e].neighbors if g[e].neighbors[x]['type']=='S'])
        filest.write(str(e)+','+ g.id+ ',' + str(g[e].label['Name']) + ',' + str(g[e].label['hash']) + ',' + str(g[e].label['time']) + ','+ g[e].label['type'] + ','
                    + str(len(g[e].inneighbors)) + ','
                    + str(ii) + ',' + str(oi) + ',' + str(di) + ',' + str(xi) + ',' + str(si) + ','
                    + str(len(g[e].neighbors)) + ','
                    + str(io) + ',' + str(oo) + ',' + str(do) + ',' + str(xo) + ',' + str(so) + ','
                    + str(an)+ ','  + str(0) +  '\n')#+ str(bc[e]) +  '\n')#
    filest.close()
    print("Finished analysis")

def do_raw_analysis(jsonfile, csvfile):      
    t0=time.clock()
    print("T0" + str(t0))
    data_file = open(jsonfile)        
    GNV= ijson.items(data_file, 'GN')
    for gnv in GNV:
        gname=gnv
        break
    data_file = open(jsonfile)
    vl=ijson.items(data_file, 'VL.item')
    
            
    t1=time.clock()
    print("T1" + str(t1-t0))
    filest = open(csvfile,'w')
    filest.write('id'+','+'gn'+','+ 'name'+','+ 'hash'+ ','+ 'time'+ ','+ 'type'+','
                 + 'ind'+ ',' + 'iid' + ','+ 'iod' + ','+ 'idd' + ','+ 'ixd' + ','+ 'isd' +','
                 + 'otd'+ ',' + 'oid' + ','+ 'ood' + ','+ 'odd' + ','+ 'oxd' + ','+ 'osd' +','
                 + 'ad'+ ',' + 'bc' + '\n')

    uh=set()    
    for e in vl:
        if e['i'] in uh:
            continue
        ine = len(e['ie'])        
        one = len(e['oe'])
        dne = len(e['de'])
        xne = len(e['xe'])
        x1ne = len(e['x1e'])
        
        filest.write(str(e['i'])+','+ gname + ',' + str(e['N']) + ',' + str(e['h']) + ',' + str(e['T']) + ','+ str(e['y']) + ','
                    + str('0') + ','
                    + str('0') + ',' + str('0') + ',' + str('0') + ',' + str('0') + ',' + str('0') + ','
                    + str('0') + ','
                    + str(ine) + ',' + str(one) + ',' + str(dne) + ',' + str(xne) + ',' + str(x1ne) + ','
                    + str(ine+one+dne+xne+x1ne)+ ','  + str(0) +  '\n')#+ str(bc[e]) +  '\n')#
    filest.close()


def parseToMongo(filename):
    print('exporting to mongo')
    data_file = open(filename)        
    GNV= ijson.items(data_file, 'GN')
    for gnv in GNV:
        gname=gnv
        break
    sgn=gname.split("-")
    client=pymongo.MongoClient('mongodb://ppgodel:123abc@192.168.47.10:27017')
    pgdb=client.planninggraphs
    gm=pgdb.graphs
    nm=pgdb.nodes
    gjson=gm.find_one({"gn":gname,"com":"02"})
    if gjson :
        print('File already parsed: ' + gname)
        return False
    gjson={"_id":ObjectId(),"gn":gname,"dom":sgn[0], "pn":sgn[len(sgn)-1], "com":"00"}
    gm.insert_one(gjson)

    data_file = open(filename)
    vl=ijson.items(data_file, 'VL.item')
    uh=set()
    rep=0
    tim=time.time() 
    print('Start Export of ' + gname)
    try:
        for e in vl:
            if e["i"] not in uh:
                uh.add(e["i"])
                if len(uh)%10000==0:
                    print(str(len(uh)) + '...')
                dicte=nm.find_one({"i":e["i"], "gid":gjson['_id']})
                if not dicte:
                    dicte=e
                    dicte['_id']=ObjectId()
                    dicte['gid']=gjson['_id']
                    nm.insert_one(dicte)
                else:
                    rep+=1
    except Exception as e:
        print(e)
    fin=time.time()-tim
    print('Done Parse of ' + str(len(uh)) +' nodes in ' + str(round(fin,2))+ ' seconds  with a insert rate of ' + str(round(len(uh)/fin,2)) + ' nps and '+str(rep)+' repeated.' )
    return True
    
namefile=sys.argv[1]
filesize = os.path.getsize(namefile)
if filesize > 50:
    pathcsv=''
    withmutex=True
    mongof=False
    delete_Flag=False
    if len(sys.argv)>2:
        withmutex= sys.argv[2] in ('True', 'T','TRUE','true')
    if len(sys.argv)>3:
        pathcsv=sys.argv[3]        
    if len(sys.argv)>4:
        mongof=sys.argv[4] in ('True', 'T','TRUE','true')
    if len(sys.argv)>5:
        delete_Flag=sys.argv[5] in ('True', 'T','TRUE','true')
        
    tim=time.clock() 
    print("reading file " + namefile + " at " +str(tim))
    dfile = open(namefile)        
    GNV= ijson.items(dfile, 'GN')
    for gnv in GNV:
        gname=gnv
        break 
    Nfilecsv=pathcsv + gname + ".csv"
    
    #client=pymongo.MongoClient('mongodb://ppgodel:123abc@192.168.47.10:27017')
    #pgdb=client.planninggraphs
    #gm=pgdb.graphs    
    #gjson=gm.find_one({"gn":gname})
    if mongof:
        parsed=parseToMongo(namefile)
        if delete_Flag and not parsed:
            os.remove(namefile)
    else:        
        if os.path.exists(Nfilecsv):
            print("File already processed")
        else:
            print("file of " + str(round(filesize/1048576,2)) + "MB")
            if filesize < 100000000:
                g=readPGjson(namefile,withmutex,1)   
                print("File readed 1, starting analysis")
                do_analysis(g,Nfilecsv)
                print("Analysis finished time: " + str(time.clock()-tim))
            elif filesize < 400000000:
                g=readPGjson(namefile,withmutex,2)   
                print("File readed 2, starting analysis")
                do_analysis(g,Nfilecsv)
                print("Analysis finished time: " + str(time.clock()-tim))
            else:
                do_raw_analysis(namefile,Nfilecsv)
                print("Analysis finished time: " + str(time.clock()-tim))
else:
    print('File ' + namefile + ' incomplete')
