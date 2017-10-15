#include <iostream>
#include <cstring>
#include <cstdlib>

bool isDigit(char c)
{
	return c >= '0' && c <= '9';
};

bool mustSkip(char c)
{
	return isDigit(c) || c == '.';
};

int main()
{
	char buff[1000];
	char buff1[10];
	double count = 0.0;

	while(cin.getline(buff,1000))
	{
		int i = 0;
		while(isDigit(buff[i])) 
		{
			++i;
		};
		if(i>0) 
		{
			count += 2.0;
			while(mustSkip(buff[i])) ++i;
			for(int j = 0;j < i;++j)
			{
				buff1[j] = buff[j];
			};
			buff1[i] = '\0';
			double t = strtod(buff1,0);
			t += count/100;
			cout << t;
		};
		cout << &buff[i] << "\n";
	};
};
