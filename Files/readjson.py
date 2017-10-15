import json
import graph
from pprint import pprint

with open('graph.json') as data_file:    
    data = json.load(data_file)
    
g= graph.Graph(data['Name'], directed=True)

for e in data['Vertexlist']:
    v = graph.Vertex(e['id'], Name = e['name'], time =e['time'], type=e['type'], hash=e['hash'])
    if 'flag' in e.keys():
        v.label['flag'] = e['flag']
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
                g.add_edge(w, v, weight=1, name = inn['name'],hash=inn['hash'])
    for outn in e['outedges']:
        if outn['hash'] != e['hash']:
            kid = str(int(v.label["time"])+1) + "_" + outn['hash']
            if(kid not in g.vertices.keys()):
                w = graph.Vertex(kid, Name = outn['name'], time =int(v.label["time"])+1, hash=outn['hash'])
                g.add_vertex(w)
            else:
                w=g[kid]            
            g.add_edge(v, w, weight=1, name = outn['name'],hash=outn['hash'])
        #g.add_edge(v,str(int(v.label["time"])+1) + "_" + outn['hash'], weight=1, name = outn['name'],hash=outn['hash'])

bc = g.betweennesscentrality()

filest = open(g.id + "distedges.csv",'w')
filest.write('id'+','+ 'name'+','+ 'hash'+','+ 'time'+','+ 'ind'+','+ 'otd'+','+ 'ad'+ ',' + 'bc' + '\n')
for e in bc:#g.vertices:
    an = len(set(g[e].neighbors.keys()).union(set(g[e].inneighbors.keys())))
    filest.write(str(e) + ',' + str(g[e].label['Name']) + ',' + str(g[e].label['hash']) + ',' + str(g[e].label['time']) + ',' + str(len(g[e].inneighbors)) + ',' + str(len(g[e].neighbors)) + ',' + str(an)+ ',' + str(bc[e]) +  '\n')# + str(0) +  '\n')#
filest.close()


