import json
import graph
import sys
import time
from pprint import pprint
import os.path
import ijson

def readPGjson(nf = 'graph.json', wm=True):
    print("loading file")
    gname=""
    vl=list()
    filesize = os.path.getsize(nf)
    if filesize >  200000000:
        print("file of " + str(filesize/1000000) + " MB")
        data_file = open(nf)
        GNV= ijson.items(data_file, 'GN')
        for gnv in GNV:
            gname=gnv
            break
        data_file = open(nf)
        vl=ijson.items(data_file, 'VL.item')
    else:
        with open(nf) as data_file:    
            data = json.load(data_file)
            gname=data['GN']
            vl=data['VL']
    print("File loaded, starting parse")                    
    g= graph.Graph(gname, directed=True)
    for e in vl:
        #read atribute vertex
        nid = e['i']#str(int(e["time"])) + "_" + e['hash']
        if nid not in g.vertices.keys():
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


if len(sys.argv)>1:
    namefile=sys.argv[1]
    pathcsv=''
    withmutex=True
    if len(sys.argv)>2:
        withmutex= sys.argv[2] in ('True', 'T','TRUE')
    if len(sys.argv)>3:
        pathcsv=sys.argv[3]        
    tim=time.clock()
    print("reading file " + namefile)
    dfile = open(namefile)
    GNV= ijson.items(dfile, 'GN')
    for gnv in GNV:
        gname=gnv
        break 
    Nfilecsv=pathcsv + gname + ".csv"
    if os.path.exists(Nfilecsv):
        print("File already processed")
    else:
        g=readPGjson(namefile,withmutex)   
        print("File readed, starting analysis")
        do_analysis(g,Nfilecsv)
        print("Analysis finished time: " + str(time.clock()-tim))
else:
    print('Using default')
