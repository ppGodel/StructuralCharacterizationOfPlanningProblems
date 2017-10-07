import json
import graph
from pprint import pprint

with open('graph.json') as data_file:    
    data = json.load(data_file)
    
g= graph.Graph(data['Name'], directed=True)

for e in data['Vertexlist']:
    v = graph.Vertex(e['id'], Name = e['name'], time =e['time'], type=e['type'])
    if 'flag' in e.keys():
        v.label['flag'] = e['flag']
    g.add_vertex(v)
    if(v.label['time']!=0):
        for inn in e['inedges']:
            g.add_edge(inn['vid'], v, weight=1, name = inn['name'],hash=inn['hash'])
    for outn in e['outedges']:
        g.add_edge(v,outn['vid'], weight=1, name = outn['name'],hash=outn['hash'])
