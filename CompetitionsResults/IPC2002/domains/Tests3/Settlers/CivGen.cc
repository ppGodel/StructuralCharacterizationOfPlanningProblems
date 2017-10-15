#include <iostream>
#include <vector>
#include <set>
#include <map>
#include <queue>
#include <string>
#include <cstdlib>
#include <algorithm>
#include <cstdio>
#include <functional>

enum probTypes {STRIPS,NUMERIC,SIMPLETIME,TIMED,COMPLEX,HARDNUMERIC};
probTypes probType;

enum typeStatus {ON,OFF};
typeStatus typing;

int rnd(int limit) {
	return (int) ((((double)((long int)limit)*random()))/(RAND_MAX+1.0));
};

double rnd() {
	return ((double) random())/(RAND_MAX+1.0);
};

double abs(double x)
{
	return x < 0?-x:x;
};

class ProblemObject {
private:
	enum OutStatus {OBJECT,INIT,GOAL};
	static OutStatus outStatus;
public:
	virtual void object(ostream & o) const = 0;
	virtual void init(ostream & o) const = 0;
	virtual void goal(ostream & o) const = 0;
	void write(ostream & o) const
	{
		switch(outStatus)
		{
			case OBJECT:
				object(o);
				break;
			case INIT:
				init(o);
				break;
			case GOAL:
				goal(o);
				break;
			default:
				break;
		};
	};

	static void Objects() {outStatus = OBJECT;};
	static void Inits() {outStatus = INIT;};
	static void Goals() {outStatus = GOAL;};
};

ProblemObject::OutStatus ProblemObject::outStatus = OBJECT;

ostream & operator<<(ostream & o,const ProblemObject & p)
{
	p.write(o);
	return o;
};

typedef set<int> SeaZone;

vector<SeaZone> seaZones(4);

class Place : public ProblemObject {
private:
	static int id;
	int place;
	bool mountain;
	bool woodland;
	bool metalliferous;
	bool bycoast;
	int seaZone;
public:
	Place() : place(id++), mountain(rnd(10) < 3), woodland(rnd(10) < 6),
		metalliferous(rnd(10) < 2), bycoast(rnd(10) < 5),
		seaZone(rnd(4)) {if(bycoast) seaZones[seaZone].insert(place);};

	void makeMountain() {mountain = true;};
	void makeWoodland() {woodland = true;};
	void makeMetalliferous() {metalliferous = true;};

	void object(ostream & o) const
	{
		o << "\tlocation" << place;
		if(typing==ON)
			o  << " - place";
		o << "\n";
	};
	void init(ostream & o) const
	{
		if(typing==OFF)
			o << "\t(place location" << place << ")\n";
		if(mountain)
			o << "\t(mountain location" << place << ")\n";
		if(woodland)
			o << "\t(woodland location" << place << ")\n";
		if(bycoast)
			o << "\t(by-coast location" << place << ")\n";
		if(metalliferous)
			o << "\t(metalliferous location" << place << ")\n";
		o << "\t(= (housing location" << place << ") 0)\n"
		  << "\t(= (available wood location" << place << ") 0)\n"
		  << "\t(= (available timber location" << place << ") 0)\n"
		  << "\t(= (available ore location" << place << ") 0)\n"
		  << "\t(= (available stone location" << place << ") 0)\n"
		  << "\t(= (available iron location" << place << ") 0)\n"
		  << "\t(= (available coal location" << place << ") 0)\n";
	};
	void goal(ostream & o) const
	{};
};

int Place::id = 0;

typedef map<int,set<int> > graph;

class Goal : public ProblemObject {
public:
	virtual ~Goal() {};
	void object(ostream & o) const {};
	void init(ostream & o) const {};
};

template<class T>
T random_select(set<T> & s)
{
	int i = rnd(s.size());
	set<T>::iterator j = s.begin();
	while(i > 0) --i,++j;
	return *j;
};

class RailLink : public Goal {
private:
	static set<pair<int,int> > links;
	vector<int> locs;
public:
	RailLink(graph & g,int nlocs,int length) 
	{
		int start = rnd(nlocs);
		locs.push_back(start);
		for(int i = 0;i < length;++i)
		{
			if(g[start].empty()) return;
			start = random_select(g[start]);
			if(locs[locs.size()-1] != start)
				locs.push_back(start);
		};
	};
	void goal(ostream & o) const
	{
		for(int i = 0;i < locs.size()-1;++i)
		{
			if(links.find(make_pair(locs[i],locs[i+1]))==links.end())
			{
				o << "\t(connected-by-rail location" 
					<< locs[i] << 
					" location" << locs[i+1] << ")\n";
				links.insert(make_pair(locs[i],locs[i+1]));
			};
		};
	};
};

set<pair<int,int> > RailLink::links = set<pair<int,int> >();

class Building : public Goal {
private:
	static set<Building > allBuildings;
	static const int numBuildings;
	static string buildingName[];
	int building;
	int loc;
public:
	Building(int nlocs) : building(rnd(numBuildings)), loc(rnd(nlocs)) 
	{
		while(loc < nlocs)
		{
			while(allBuildings.find(*this)!=allBuildings.end() && 
				building<numBuildings)
			{
				++building;
			};
			if(building<numBuildings) break;
			++loc;
			building = 0;
		};
		allBuildings.insert(*this);
	};
	bool operator==(const Building & b) const
	{
		return building==b.building && loc==b.loc;
	};
	bool operator<(const Building & b) const
	{
		return loc < b.loc || (loc == b.loc && building < b.building);
	};
	void goal(ostream & o) const
	{
		o << "\t(has-" << buildingName[building] << " location" << loc
			<< ")\n";
	};
};

set<Building> Building::allBuildings = set<Building>();
const int Building::numBuildings = 3;
string Building::buildingName[numBuildings] 
			= {"coal-stack","sawmill","ironworks"};

class Housing : public Goal {
private:
	static set<int> places;
	int place;
	int quantity;
public:
	Housing(int nlocs)
	{
		if(places.empty())
		{
			for(int i = 0;i < nlocs;++i)
				places.insert(i);
		};
		place = random_select(places);
		quantity = rnd(2)+1;
		places.erase(place);
	};
	void goal(ostream & o) const
	{
		o << "\t(>= (housing location" << place << ") " << quantity << ")\n";
	};
};

set<int> Housing::places = set<int>();

class Map : public ProblemObject {
private:
	int places;
	vector<Place> Places;
	graph road;

	vector<Goal *> goals;

	void explore(graph & g,int start,set<int> & reached)
	{
		queue<int> togo;
		
		togo.push(start);
		reached.insert(start);
		while(!togo.empty())
		{
			int loc = togo.front();
			togo.pop();
			
			for(set<int>::const_iterator i = g[loc].begin();i != g[loc].end();++i)
			{
				if(find(reached.begin(),reached.end(),*i)==reached.end())
				{
					togo.push(*i);
					reached.insert(*i);
				};
			};
		};
	};

	void connect(graph & g) 
	{
		set<int> reached;
		int start = rnd(places);
		explore(g,start,reached);
		while(reached.size()!=places)
		{
			int next;
			for(int i = 0;i < places;++i)
			{
				if(find(reached.begin(),reached.end(),i)==reached.end())
				{
					next = i;
					break;
				};
			};
			g[start].insert(next);
			start = next;
			explore(g,start,reached);
		};
		
	};

public:
	Map(int s,int ngs) : places(s)
	{
		for(int i = 0;i < s;++i)
		{
			Places.push_back(Place());
			for(int j = 0;j < 4;++j)
			{
				int f = rnd(s);
				int t = rnd(s);
				if(road[t].find(f)==road[t].end())
					road[f].insert(t);
			};
		};
		connect(road);
		Places[rnd(places)].makeMountain();
		Places[rnd(places)].makeWoodland();
		Places[rnd(places)].makeMetalliferous();

		for(int i = 0; i < ngs;++i)
		{
			if(rnd(10) < 3)
			{
				goals.push_back(new RailLink(road,places,ngs/3));
			}
			else
			{
				if(rnd(10) < 3)
					goals.push_back(new Housing(places));
				else goals.push_back(new Building(places));
			};
		};
	};
	~Map()
	{
		for(vector<Goal *>::iterator i = goals.begin();i != goals.end();++i)
			delete *i;
	};

	void object(ostream & o) const
	{
		copy(Places.begin(),Places.end(),ostream_iterator<Place>(cout,""));
	};
	void init(ostream & o) const
	{
		copy(Places.begin(),Places.end(),ostream_iterator<Place>(cout,""));
		if(typing==OFF)
			o << "\t(resource iron)\n\t(resource wood)\n\t(resource timber)\n\t(resource ore)\n\t(resource stone)\n\t(resource coal)\n";
		
		for(graph::const_iterator i = road.begin();i != road.end();++i)
		{
			for(set<int>::const_iterator j = i->second.begin();
							j != i->second.end();++j)
			{
				if(i->first == *j) continue;
				if(road.find(*j) != road.end() && 
					road.find(*j)->second.find(i->first)
						!= road.find(*j)->second.end())
							continue;

				o << "\t(connected-by-land location" << i->first << " location" << *j << ")\n\t(connected-by-land location" <<
					*j << " location" << i->first 
					<< ")\n";

			};
		};
		for(int i = 0;i < 4;++i)
		{
			for(SeaZone::const_iterator s = seaZones[i].begin();
					s != seaZones[i].end();++s)
			{
				SeaZone::const_iterator t = s;
				for(++t;t != seaZones[i].end();++t)
				{
					o << "\t(connected-by-sea location" << *s << " location" << *t << ")\n\t(connected-by-sea location" << *t << " location" << *s << ")\n";
				};
			};
		};
	};

	void goal(ostream & o) const 
	{
		for(vector<Goal *>::const_iterator i = goals.begin();
			i != goals.end();++i)
		{
			o << **i;
		};
	};

};

class Vehicle : public ProblemObject {
private:
	static int id;
	int vnum;
public:
	Vehicle() : vnum(id++) {};
	void object(ostream & o) const
	{
		o  << "\tvehicle" << vnum;
		if(typing==ON)
			o << " - vehicle";
		o << "\n";
	};
	void init(ostream & o) const
	{
		if(typing==OFF)
			o << "\t(vehicle vehicle" << vnum << ")\n";
		o << "\t(potential vehicle" << vnum << ")\n";
	};
	void goal(ostream & o) const
	{};
};

int Vehicle::id = 0;

int main(int argc,char * argv[])
{
	typing = ON;
	probType = STRIPS;
	if(argc < 4)
	{
		cout << "Usage: settlers [-u] <seed> #<locations> #<goals> #<vehicles>\n\n\tThe number of vehicles is the number of \"potential\" vehicles.\n\n\t-u: Untyped\n\n";
			
		exit(0);
	};
	while(argv[1][0]=='-')
	{
		switch(argv[1][1])
		{
			case 'u':
				typing = OFF;
				break;

			default:
				break;
		};
		++argv;
		--argc;
	};
	srandom(atoi(argv[1]));
	Map p(atoi(argv[2]),atoi(argv[3]));
	vector<Vehicle> carts;
	for(int i = 0;i < atoi(argv[4]);++i)
		carts.push_back(Vehicle());
	cout << "(define (problem settlers)\n(:domain civ)\n(:objects\n";
	cout << p;
	copy(carts.begin(),carts.end(),ostream_iterator<Vehicle>(cout,""));
	cout << ")\n(:init\n\t(= (resource-use) 0)\n\t(= (labour) 0)\n\t(= (pollution) 0)\n";
	ProblemObject::Inits();
	cout << p;
	copy(carts.begin(),carts.end(),ostream_iterator<Vehicle>(cout,""));
	cout << ")\n(:goal (and\n";
	ProblemObject::Goals();
	cout << p;
	cout << "\t)\n)\n\n(:metric minimize (+ (+ (* " << rnd(4) << " (pollution)) (* "
		<< rnd(4) << " (resource-use))) (* " << rnd(4) << " (labour))))\n)\n";
	return 0;
};
