#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

enum probTypes {STRIPS,NUMERIC,SIMPLETIME,TIMED,COMPLEX};
probTypes probType;

enum typeStatus {ON,OFF};
typeStatus typing;

int rnd(int limit) {
	return (int) ((((double)((long int)limit)*random()))/(RAND_MAX+1.0));
};

double rnd() {
	return ((double) random())/(RAND_MAX+1.0);
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

class Card : public ProblemObject {
public:
	static string suitNames[];
private:
	static int numSuits;
	static int numValues;
	int suit;
	int value;
public:
	Card(int s,int v) : suit(s), value(v) {};

	void swap(Card & c)
	{
		int s = suit;
		suit = c.suit;
		c.suit = s;
		s = value;
		value = c.value;
		c.value = s;
	};

	void write(ostream & o) const
	{
		if(suit < 4) 
		{
			o << suitNames[suit];
			
		}
		else
		{
			o << "Suit" << suit+1 << "-";
		};

		switch(value)
		{
			case 0: 
				o << "A";
				break;
			case 10:
				o << "J";
				break;
			case 11:
				o << "Q";
				break;
			case 12:
				o << "K";
				break;
			default:
				o << value+1;
		};
	};

	static void setSuitsValues(int s,int v) {numSuits = s;numValues=v;};

	void object(ostream & o) const
	{
		write(o);
		if(typing==ON) o << " - card";
	};

	void init(ostream & o) const
	{
		o << "\t(value ";
		write(o);
		o << " N" << value+1 << ")\n\t(suit ";
		write(o);
		o << " ";
		if(suit < 4) 
		{
			o << suitNames[suit];
		}
		else
		{
			o << "Suit" << suit+1;
		};
		o << ")\n";
		if(value>=numValues-1 || value < 0) return;
		for(int i = 0;i < numSuits;++i)
		{
			if(suit%2 != i%2)
			{
				o << "\t(canstack ";
				write(o);
				o << " ";
				Card(i,value+1).write(o);
				o << ")\n";
			};
		};
	};

	void goal(ostream & o) const
	{};
};

int Card::numSuits = 0;
int Card::numValues = 0;

string Card::suitNames[4] = {"diamond","club","heart","spade"}; 

class FreeProblem : public ProblemObject {
private:
	int suits;
	int cards;
	int freecells;
	int columns;

	vector<Card> pack;
	vector<vector<Card> > tableau;

public:
	FreeProblem(int s,int c,int f,int cl) : suits(s), cards(c), 
		freecells(f), columns(cl), tableau(cl)
	{
		Card::setSuitsValues(suits,cards);
		for(s = 0;s < suits;++s)
			for(c = 0;c < cards;++c)
				pack.push_back(Card(s,c));
		int ncards = suits*cards;
		for(int i = 0;i < ncards*10;++i)
		{
			int f = rnd(ncards);
			int t = rnd(ncards);
			while(f==t)
			{
				t = rnd(cards);
			};
			pack[f].swap(pack[t]);
		};
		c = 0;
		for(int i = 0;i < ncards;++i)
		{
			tableau[c].push_back(pack[i]);
			++c;
			c = c%columns;
		};
	};

	void object(ostream & o) const
	{
		o << "\t";
		for(int i = 0;i < suits;++i)
		{
			if(i < 4) 
			{
				o << Card::suitNames[i] << " ";
			}
			else
			{
				o << "Suit" << i+1 << " ";
			};
		};
		if(typing==ON) o << "- suitsort";
		o << "\n\t";

		for(int i = 0;i <= max(cards,max(columns,4));++i)
		{
			o << "N" << i << " ";
		};
		if(typing==ON)
		{
			o << "- denomination\n\t";
		};
		copy(pack.begin(),pack.end(),
			ostream_iterator<Card>(o,"\n\t"));
		for(int i = 0;i < suits;++i)
		{
			o << Card(i,-1) << "\n\t";
		};
		o << "\n";
	};

	void init(ostream & o) const
	{
		for(int i = 0;i < max(max(cards,4),columns);++i)
			o << "\t(successor N" << i+1 << " N" << i << ")\n";
		
		o << "\t(cellspace N" << freecells << ")\n";
		for(int i = 0;i < columns;++i)
		{
			if(tableau[i].empty()) continue;
			vector<Card>::const_iterator j = tableau[i].begin();
			o << "\t(clear ";
			j->write(o);
			o << ")\n";
			vector<Card>::const_iterator k = j;
			++k;
			while(k != tableau[i].end())
			{
				o << "\t(on ";
				j->write(o);
				o << " ";
				k->write(o);
				o << ")\n";
				j = k++;
			};
			o << "\t(bottomcol ";
			j->write(o);
			o << ")\n";
		};
		if(pack.size() < columns)
		{
			o << "\t(colspace N" << columns-pack.size() << ")\n";
		}
		else
		{
			o << "\t(colspace N0)\n";
		};
		copy(pack.begin(),pack.end(),ostream_iterator<Card>(o,""));
		for(int i = 0;i < suits;++i)
		{
			o << "\t(home ";
			Card(i,-1).write(o);
			o << ")\n";
			o << Card(i,-1);
		};
	};

	void goal(ostream & o) const
	{
		for(int i = 0;i < suits;++i)
		{
			o << "\t(home ";
			Card(i,cards-1).write(o);
			o << ")\n";
		};
	};


};





void usage() 
{
	cout << "Usage: freegen [-u] <seed> <#s> <#c> <#f> <#cl>\n\n\twhere: #s = number of suits, #c = cards/suit,\n\t\t#f = freecells, #cl = columns\n\n\t-u: Untyped\n\n";
};

int main(int argc,char * argv[])
{
	if(argc < 2)
	{
		usage();
		return 0;
	};

	typing = ON;
	probType = STRIPS;

	while(argv[1][0]=='-')
	{
		switch(argv[1][1])
		{
			case 'u':
				typing=OFF;
				break;
			default:
				break;
		};
		++argv;
		--argc;
	};

	if(argc<5)
	{
		usage();
		return 0;
	};

	srandom(atoi(argv[1]));

	int suits = atoi(argv[2]);
	int cards = atoi(argv[3]);
	int freecells = atoi(argv[4]);
	int columns = atoi(argv[5]);

	FreeProblem fp(suits,cards,freecells,columns);
	cout << "(define (problem FreeCell" << cards << "-" << suits << ")\n(:domain freecell)\n(:objects\n" << fp << ")\n(:init\n";
	ProblemObject::Inits();
	cout << fp << ")\n(:goal (and\n";
	ProblemObject::Goals();
	cout << fp << ")))\n";
	return 0;
};
