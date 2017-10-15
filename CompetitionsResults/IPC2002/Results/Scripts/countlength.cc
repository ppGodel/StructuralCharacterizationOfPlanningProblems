#include <iostream>
#include <cstring>
#include <cstdlib>

bool isDigit(char c)
{
	return c >= '0' && c <= '9';
};

int main()
{
	char buff[1000];
	int planlength = 0;

	while(cin.getline(buff,1000))
	{
		int i = 0;
		while(buff[i]!= '\0')
		{
			while(buff[i] != '\0' && (buff[i]==' ' || buff[i]=='\t')) ++i;
			if(isDigit(buff[i]) || buff[i]=='(') ++planlength;
			while(buff[i] != '\0' && buff[i] != ')') ++i;
			if(buff[i]==')') ++i;
		};

	};

	cout << planlength << "\n";
	return 0;
};