import json
import graph
import sys
import time
from pprint import pprint

def readPGjson(nf = 'graph.json', wm=True):
    with open(nf) as data_file:    
        data = json.load(data_file)
    
    g= graph.Graph(data['Name'], directed=True)

    for e in data['Vertexlist']:
        #read atribute vertex
        nid = str(int(e["time"])) + "_" + e['hash']
        if nid not in g.vertices.keys():
            v = graph.Vertex(e['id'], Name = e['name'], time =e['time'], type=e['type'], hash=e['hash'])
        else:
            v = g[nid]
            v.label['is_true'] = e['is_true']
            v.label['uid_mask'] = e['uid_mask']
            v.label['uid_block'] = e['uid_block']
            v.label['cant_do'] = e['cant_do']
            v.label['type'] = e['type']
            
            
        if 'flag' in e.keys():
            v.label['flag'] = e['flag']
        #add it to the graph
        g.add_vertex(v)
        if(v.label['time']!=0):
            for inn in e['inedges']:
                if inn['hash'] != e['hash']:
                    kid = str(int(v.label["time"])-1) + "_" + inn['hash']
                    if(kid not in g.vertices.keys()):
                        w = graph.Vertex(kid, Name = inn['name'], time =int(v.label["time"])-1, hash=inn['hash'])
                        g.add_vertex(w)
                    else:
                        w=g[kid]            
                        g.add_edge(w, v, weight=1, name = inn['name']+ '___' + v.label['Name'],hash=inn['hash'], type='I')
        for outn in e['outedges']:
            if outn['hash'] != e['hash']:
                kid = str(int(v.label["time"])+1) + "_" + outn['hash']
                if(kid not in g.vertices.keys()):
                    w = graph.Vertex(kid, Name = outn['name'], time =int(v.label["time"])+1, hash=outn['hash'])
                    g.add_vertex(w)
                else:
                    w=g[kid]            
                    g.add_edge(v, w, weight=1, name = v.label['Name'] + '___' + outn['name'] ,hash=outn['hash'], type='O')
                    #g.add_edge(v,str(int(v.label["time"])+1) + "_" + outn['hash'], weight=1, name = outn['name'],hash=outn['hash'])
        for deln in e['deledges']:
            if deln['hash'] != e['hash']:
                kid = str(int(v.label["time"])+1) + "_" + deln['hash']
                if(kid not in g.vertices.keys()):
                    w = graph.Vertex(kid, Name = deln['name'], time =int(v.label["time"])+1, hash=deln['hash'])
                    g.add_vertex(w)
                else:
                    w=g[kid]            
                    g.add_edge(v, w, weight=1, name = v.label['Name']+ '___' + deln['name'] ,hash=deln['hash'], type='D')
        if wm:
            for excn in e['excedges']:
                if excn['hash'] != e['hash']:
                    kid = str(int(v.label["time"])+1) + "_" + excn['hash']
                    if(kid not in g.vertices.keys()):
                        w = graph.Vertex(kid, Name = excn['name'], time =int(v.label["time"])+1, hash=excn['hash'])
                        g.add_vertex(w)
                    else:
                        w=g[kid]            
                        g.add_edge(v, w, weight=1, name = v.label['Name']+ '___' + excn['name'] ,hash=excn['hash'], type='X')
        for eisn in e['eis_edges']:
            if eisn['hash'] != e['hash']:
                kid = str(int(v.label["time"])+1) + "_" + eisn['hash']
                if(kid not in g.vertices.keys()):
                    w = graph.Vertex(kid, Name = eisn['name'], time =int(v.label["time"])+1, hash=eisn['hash'])
                    g.add_vertex(w)
                else:
                    w=g[kid]            
                    g.add_edge(v, w, weight=1, name = v.label['Name']+ '___' + eisn['name'] ,hash=eisn['hash'], type='S') #bc = g.betweennesscentrality()
    return g
                    
def do_analysis(g, filename):                    
    filest = open(filename,'w')
    filest.write('id'+','+ 'name'+','+ 'hash'+','+ 'time'+','+ 'ind'+','+ 'otd'+','+ 'ad'+ ',' + 'bc' + '\n')
    for e in g.vertices:#bc:#
        an = len(set(g[e].neighbors.keys()).union(set(g[e].inneighbors.keys())))
        filest.write(str(e) + ',' + str(g[e].label['Name']) + ',' + str(g[e].label['hash']) + ',' + str(g[e].label['time']) + ',' + str(len(g[e].inneighbors)) + ',' + str(len(g[e].neighbors)) + ',' + str(an)+ ','  + str(0) +  '\n')#+ str(bc[e]) +  '\n')#
    filest.close()



if len(sys.argv)>1:
    namefile=sys.argv[1]
    withmutex=True
    if len(sys.argv)>2:
        withmutex= sys.argv[2] in ('True', 'T','TRUE')
    tim=time.clock()
    g=readPGjson(namefile,withmutex)    
    do_analysis(g,g.id + "distedges.csv")
    print(time.clock()-tim)    
else:
    print('Using default')
