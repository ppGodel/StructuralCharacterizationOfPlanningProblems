import multiprocessing
import random
import math
from heapq import heappop, heappush
from collections import defaultdict

def flatten(L):
    while len(L) > 0:
        yield L[0]
        L = L[1]


class Vertex:
    def __init__(self, _id, **args):
        self._id = _id
        self._outneighbors = {}
        self._inneighbors = {}
        self._label = dict()
        self._label['id'] = _id
        for a in args:
            self._label[a] = args[a]

    @property
    def id(self):
        return self._id

    @property
    def label(self):
        return self._label

    @property
    def neighbors(self):
        return self._outneighbors
    
    @property
    def inneighbors(self):
        return self._inneighbors
    
    def __str__(self):
        result = 'Vertex: ' + str(self.id) + '\n'
        for l in self.label:
            result += l +': ' + str(self.label[l]) + '\n'
        if len(self.neighbors)>0:
            result += 'Neighbors: OutDegree('+ str(len(self.neighbors)) + ')\n'
            for n in self.neighbors:
                result += 'Vertex: ' + str(n) + ' '
                if isinstance(self.neighbors[n], dict):
                    for w in self.neighbors[n]:
                        result += w + ': ' + str(self.neighbors[n][w]) + ' '
                else:
                    result += 'value:' + self.neighbors[n]
                result += '\n'
        else:
            result += 'No Neighbors \n'        
        if len(self.inneighbors)>0:
            result += 'In Neighbors: InDegree('+ str(len(self.inneighbors)) + ')\n'
            for n in self.inneighbors:
                result += 'Vertex: ' + str(n) + ' '
                if isinstance(self.inneighbors[n], dict):
                    for w in self.inneighbors[n]:
                        result += w + ': ' + str(self.inneighbors[n][w]) + ' '
                else:
                    result += 'value:' + self.inneighbors[n]
                result += '\n'                
        else:
            result += 'No in Neighbors \n'
        return result

    def add_neighbor(self, _neighbor, **args):
        if _neighbor not in self.neighbors.keys():
            self.neighbors[_neighbor] = dict()
        if len(args) > 0:
            for a in args:
                self.neighbors[_neighbor][a] = args[a]
        else:
            self.neighbors[_neighbor]['weight'] = 1

    def add_inneighbor(self, _inneighbor, **args):
        if _inneighbor not in self.inneighbors.keys():
            self.inneighbors[_inneighbor] = dict()            
        if len(args) > 0:
            for a in args:
                self.inneighbors[_inneighbor][a] = args[a]
        else:
            self.inneighbors[_inneighbor]['weight'] = 1
            
class Graph:
    def __init__(self, name, directed = False):
        self._vertices = dict()
        self._id = name
        self._attr = dict()
        self._directed = directed
        #self._attr['cyclic'] = None
    @property
    def vertices(self):
        return self._vertices

    @property
    def cardinal(self):
        return len(self._vertices)

    @property
    def id(self):
        return self._id

    @property
    def directed(self):
        return self._directed
    
    @property
    def complete(self):
        if 'complete' not in self._attr.keys():
            self._attr['complete'] = self.iscomplete()
        return self._attr['complete']

    @property
    def connected(self):
        if 'connect' not in self._attr.keys():
            self._attr['connect'] = self.isconnected()
        return self._attr['connect']

    @property
    def tree(self):
        if 'tree' not in self._attr.keys():
            self._attr['tree'] = self.istree()
        return self._attr['tree']
    
    @property
    def density(self):
        n = self.cardinal
        if self.directed:
            result = self.getNumberEdges() / (2* n * (n - 1.0))
        else:
            result = self.getNumberEdges() / ( n * (n - 1.0))
        return result
    
    @property
    def degreesum(self):
        return sum([len(self.vertices[v].neighbors) for v in self.vertices]) 

    def __getitem__(self, item):
        res = None
        if (isinstance(item, int) or isinstance(item, str)) and item in self._vertices.keys():
            res = self._vertices[item]
        if isinstance(item, Vertex) and item in self._vertices.keys():
            res = self._vertices[item.id]
        return res

    def __iter__(self):
        res = iter(self._vertices.values())
        return res

    def __str__(self):
        return self.to_string(sv=True, sa=True)
    
    def to_string(self, sv=False, sa=True, key='weight'):
        result = 'Graph: ' + str(self.id) + '\n'
        result += 'Number of vertices: ' + str(self.cardinal) + '\n'
        result += 'Number of edges: ' + str(self.getNumberEdges()) + '\n'
        result += 'Densidad: ' + str(self.density) + '\n'
        if sa:
            self.connected
            self.complete
            self.tree
        for a in self._attr:
            result += a +': ' + str(self._attr[a]) + '\n'
        if sv:
            for v in (self._vertices):
                result += str(self._vertices[v]) + '\n'
                result += 'bc: ' + str(self.betweennesscentrality(v, key)) +'\n'
                result += 'cc: ' + str(self.closenesscentrality(v)) +'\n'
        return result
    def add_vertex(self, _vertex_obj):
        if isinstance(_vertex_obj, Vertex):
            if _vertex_obj.id not in self._vertices.keys():
                self._vertices[_vertex_obj.id] = _vertex_obj
        return _vertex_obj
    def add_edge(self, _begin, _end, **args):
#        beg, end = None, None
        if isinstance(_begin, Vertex):
            if _begin.id in self.vertices.keys():
                beg = self.vertices[_begin.id]
            else:
                beg = _begin
            self.add_vertex(beg)                
        else:
            if _begin in self._vertices.keys():
                beg = self._vertices[_begin]
            else:
                beg = Vertex(_begin)
                self.add_vertex(beg)
        if isinstance(_end, Vertex):
            if _end.id in self.vertices.keys():
                end = self.vertices[_end.id]
            else:
                end = _end
            self.add_vertex(end)
        else:
            if _end in self._vertices.keys():
                end = self._vertices[_end]
            else:
                end = Vertex(_end)
                self.add_vertex(end)

        # print(beg.id, end.id, _weight)
        if not self.directed:
            self.vertices[end.id].add_neighbor(beg.id, **args)
            self.vertices[beg.id].add_inneighbor(end.id, **args)
        self.vertices[beg.id].add_neighbor(end.id, **args)
        self.vertices[end.id].add_inneighbor(beg.id, **args)
#        print(beg.id, beg.inneighbors,end.id, end.inneighbors)

    def remove_vertex(self, v):
        if isinstance(v, Vertex):
            rem = self.vertices.pop(v.id)
        else:
            if v in self._vertices.keys():
                rem = self.vertices.pop(v)
            else:
                return None
        for n in self._vertices:
            if rem.id != self._vertices[n]:
                self._vertices[n].neighbors.pop(v.id)
        return rem

    
    def getedges(self):
        result = dict()
        for v in self.vertices:
            for n in self.vertices[v].neighbors:
                result[(v,n)] = self.vertices[v].neighbors[n]
        return result

    def vertexcomplete(self, v):
        result = False
        if not( len(self._vertices[v].neighbors)  == self.cardinal - 1 and v not in self._vertices[v].neighbors):
            for n in self._vertices:
                if v != n and n not in self._vertices[v].neighbors:
                    break
                else:
                    result = True
        else:
            result=True
        return result

    def getNumberEdges(self):
        edges = 0
        for v in self.vertices:
                edges+= len(self._vertices[v].neighbors)
        return edges
    
    def iscomplete(self):
        result = True
#        with multiprocessing.Pool() as pool:
#            workers = []
#            results = []
#            for v in self._vertices:
#                workers.append(pool.apply_async(func=Graph.vertexcomplete, args=(self, v,)))
#            for w in workers:
#                result = result and w.get()
        
        for v in self.vertices:
            result = result and self.vertexcomplete(v)
        return result

    def istree(self):
        result = False
        if self.connected and (((self.cardinal if self.directed else self.cardinal*2) -1) == self.getNumberEdges()):
            result = True
        return result

    def isconnected(self):
        result = False
        bfs = self.deepfirstsearch(None)
        if len(bfs) == self.cardinal:
            result = True
        return result


    def deepfirstsearch(self, v=None):
        if v is None: 
            lv = [x for x in self.vertices if len(self[x].inneighbors) == 0 and len(self[x].neighbors) > 0]
            if len(lv)> 0:
                v = self[lv[-1]]
            else:
                v = self[random.choice(list(self.vertices))]
        else:
            if v is not Vertex:
                v = self[v]
            else:
                v= self[v.id]                
        g = []
        p = [v]
        while len(p)>0:
            av = p.pop()
            if av.id not in g:
                g.append(av.id)
                # print(av.id)
                # print(av.neighbors.keys())
                if len(av.neighbors) > 0:
                    ne = list(av.neighbors.keys())
                    random.shuffle(ne)
                    for n in ne:
                        #print('v', av.id, 'n', n, 'p', p)
                        ne = self[n]
                        if ne.id not in g:
                            if ne not in p:
                                p.append(ne)
        return g

    def breadthfirstsearch(self, v=None):
        if v is None:
            lv = [x for x in self.vertices if len(self[x].inneighbors) == 0 and len(self[x].neighbors) > 0]
            if len(lv)> 0:
                v = self[lv[-1]]
            else:
                v = self[random.choice(list(self.vertices))]
        else:
            if type(v) is not  Vertex:
                v =self.vertices[v]
            else:
                v= self.vertices[v.id]
        levels = {}
        levels[v.id] = (0,v.id)
        sig = [v]
        #print(v.id, levels[v.id])
        while len(sig) > 0:
            mark = []
            for l in sig:
                for n in l.neighbors:   
                    if n not in levels:
                        levels[n]= (levels[l.id][0]+1, l.id)
                        mark.append(self[n])
            sig = mark            
        return levels

    def closenesscentrality(self, av=None):
        all = False
        m = self.cardinal
        if type(av) is Vertex:
            av = av.id
        elif av is None:
            all = True
            result = defaultdict(int)
        if av in self.vertices:
            lvl = {}            
            bg = defaultdict(int)
            for v in self.vertices:
                bg[v] = self.breadthfirstsearch(v)
            #print(lvl)
            #print(bg)
            s = 0
            m = self.cardinal
            for v in bg:
                if all:
                    for c in bg[v]:
                        if c != n:
                            aux = bg[v][c]
                            if aux < 1:
                                aux = m
                            result[c] += aux
                elif av in bg[v].keys():
                    s += bg[v][av]
                else:
                    s += m
            #print(m,s)
            if all:
                for w in result:
                        result[w] = (m-1)/result[w]
                return result
            elif s <= 0:
                r = math.inf
            else:
                r = (m-1)/s
        else:
            r = None
        return r

               
    def shortest(self, v, w, key='weight'): # Dijkstra's algorithm
        if v is Vertex:
            v = v.id
        if w is Vertex:
            w = w.id
        q = [(0, v, ())]
        visited = set() 
        while len(q) > 0:
            (l, u, p) = heappop(q)
            if u not in visited:
                visited.add(u)
                if u == w:
                    return list(flatten(p))[::-1] + [u]
            p = (u, p)
            for n in self[u].neighbors:
                if n not in visited:
                    el = self.vertices[u].neighbors[n][key] if key in self.vertices[u].neighbors[n].keys() else 1
                    heappush(q, (l + el, n, p))
        return None
    
    def allshortedpaths(self, key= 'weight'):
        p = list()
        visited = []
        for v in self:
            if not self.directed:
                visited.append(v.id)
            for u in self:
                if u.id not in visited:
                    res = self.shortest(v.id, u.id, key)
                    if res is not None:
                        p.append(res)
        return p

    #Esta funcion es basada en la mostrada en la ponencia de chile de la Dra Elisa Schaeffer
    def betweennesscentrality(self, element=None, key='edge'):
        #print(element, key)
        p = self.allshortedpaths()
        if element is None: # all vertex betweennesses
            b = defaultdict(int) # zero if no paths
            for v in self:
                b[v.id] = sum([v.id in s for s in p])
            return b
        elif element in self.vertices: # vertex betweenness
            return sum([element in s for s in p])
        elif len(element) == 2: # edge betweenness
            (v, u) = element
            c = 0
            for s in p:
                if v in s and u in s:
                    if abs(s.index(v) - s.index(u)) == 1:
                        c += 1
            return c

    def kruskal(self, key='weight'):
        e = self.getedges()
        #for v in self.vertices:
        #    for n in self[v].neighbors:
        #        e[(v,n)] = self[v].neighbors[n]
        arbol = Graph(self.id + ' MST from kuskal', directed=True)
        peso = 0
        comp = dict()
        #print(e)
        t = sorted(e.keys(), key = lambda k: e[k][key], reverse=True)        
        #print(t)
        nuevo = set()
        while len(t) > 0 and len(nuevo) < len(self.vertices):
            #print(len(t)) 
            arista = t.pop()
            w = e[arista][key]    
            del e[arista]
            (u,v) = arista
            c = comp.get(v, {v})
            if u not in c:
                #print('u ',u, 'v ',v ,'c ', c)
                arbol.add_edge(u,v,weight=w)
                peso += w
                nuevo = c.union(comp.get(u,{u}))
                for i in nuevo:
                    comp[i]= nuevo
        print('MST con peso', peso, ':', nuevo)
        return arbol

    def findpath(self, v,w, path=[], capacity_key='capacity', flow_key='flow'):
        if v is Vertex:
            v = v.id
        if w is Vertex:
            w = w.id
        if v==w:
            return path
        for n in self.vertices[v].neighbors:
            if capacity_key not in self.vertices[v].neighbors[n].keys():
                self.vertices[v].neighbors[n][capacity_key] = self.vertices[v].neighbors[n]['weight']
            if capacity_key not in self.vertices[n].inneighbors[v].keys():
                self.vertices[n].inneighbors[v][capacity_key] = self.vertices[n].inneighbors[v]['weight']
                
            if flow_key not in self.vertices[v].neighbors[n].keys():
                self.vertices[v].neighbors[n][flow_key] = 0
            if flow_key not in self.vertices[n].inneighbors[v].keys():
                self.vertices[n].inneighbors[v][flow_key] = 0
                
            residual = self.vertices[v].neighbors[n][capacity_key] #- self.vertices[v].neighbors[n][flow_key]
            if residual > 0 and not ((v,n), residual) in path:
                result = self.findpath(n, w, path + [((v,n), residual)])
                if result != None:
                    return result

    def findshortpath(self, v,w, capacity_key='capacity', flow_key='flow'):
        path = []
        if v is Vertex:
            v = v.id
        if w is Vertex:
            w = w.id
        if v==w:
            return path
        #do a bfs from w to v
        levels = {}
        levels[v] = (0,None)
        sig = [v]
        #print(v.id, levels[v.id])
        while len(sig) > 0:
            mark = []
            for l in sig:
                for n in self.vertices[l].neighbors:                    
                    if capacity_key not in self.vertices[l].neighbors[n].keys():
                         self.vertices[l].neighbors[n][capacity_key] = self.vertices[l].neighbors[n]['weight']
                    if capacity_key not in self.vertices[n].inneighbors[l].keys():
                        self.vertices[n].inneighbors[l][capacity_key] = self.vertices[n].inneighbors[l]['weight']                
                    if flow_key not in self.vertices[l].neighbors[n].keys():
                        self.vertices[l].neighbors[n][flow_key] = 0
                    if flow_key not in self.vertices[n].inneighbors[l].keys():
                                self.vertices[n].inneighbors[l][flow_key] = 0

                    residual = self.vertices[l].neighbors[n][capacity_key]

                    if n not in levels and residual > 0:
                        levels[n]= (levels[l][0]+1, l)
                        if n == w:
                            ac = n
                            prv = levels[ac][1]
                            while prv is not None:
                                nr = self.vertices[prv].neighbors[ac][capacity_key]
                                path.append(((prv,ac),nr))
                                ac=prv
                                prv = levels[ac][1]
                            return path    
                        mark.append(n)
            sig = mark            
        #for vl in levels:
        return path
            

    def fordfulkersonmaxflow(self, s, t, capacity_key='capacity', flow_key='flow'):
        result = 0
        path = self.findpath(s,t,[])
        while path is not None and len(path) > 0:
            flow = min(res for edge, res in path)
            for edge, res in path:
                u, v = edge
                self.vertices[u].neighbors[v][flow_key] += flow
                self.vertices[v].inneighbors[u][flow_key] += flow
                
                self.vertices[u].neighbors[v][capacity_key] -= flow
                self.vertices[v].inneighbors[u][capacity_key] -= flow                
            path = self.findpath(s,t,[])
            if type(path) is type(None):
                path =[]
        for n in self.vertices[t].inneighbors:
            if flow_key in self.vertices[t].inneighbors[n]:
                result += self.vertices[t].inneighbors[n][flow_key]
        return result

    def shortaugmentingmaxflow(self, s, t, capacity_key='capacity', flow_key='flow'):
        result = 0
        path = self.findshortpath(s,t)
        while len(path) > 0:
            flow = min(res for edge, res in path)
            for edge, res in path:
                u, v = edge
                self.vertices[u].neighbors[v][flow_key] += flow
                self.vertices[v].inneighbors[u][flow_key] += flow
                
                self.vertices[u].neighbors[v][capacity_key] -= flow
                self.vertices[v].inneighbors[u][capacity_key] -= flow                
            path = self.findshortpath(s,t)
            if type(path) is type(None):
                path =[]
        for n in self.vertices[t].inneighbors:
            if flow_key in self.vertices[t].inneighbors[n]:
                result += self.vertices[t].inneighbors[n][flow_key]
        return result

    def maxflow(self, s, t, capacity_key='capacity', flow_key='flow', algtype=1):
        if algtype ==1:
            return self.shortaugmentingmaxflow(s,t,capacity_key, flow_key)
        else:
            return self.fordfulkersonmaxflow(s,t,capacity_key, flow_key)
                
    def resetflow(self,capacity_key='capacity', flow_key='flow'):
        for v in self.vertices:
            for n in self.vertices[v].neighbors:                    
                self.vertices[v].neighbors[n][capacity_key] = self.vertices[v].neighbors[n]['weight']
                self.vertices[n].inneighbors[v][capacity_key] = self.vertices[n].inneighbors[v]['weight']                
                self.vertices[v].neighbors[n][flow_key] = 0
                self.vertices[n].inneighbors[v][flow_key] = 0
