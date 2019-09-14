//Lab_1_find_fact
#include<stdio.h> 
#include<conio.h> 
#include<windows.h> 
#include<math.h> 

char bufRus[256]; 
char* Rus(const char*text) 
{ 
CharToOem(text, bufRus); 
return bufRus; 
} 

long int factorial(long int n);

int main() 
{
	int k = 0;
	long fact;
	while (k < 16){
		printf("%d! = %d\n", k, factorial(k));
		k++;
	}
	return(0);
}

long factorial(long int n)
{
    if (n == 0 || n == 1) return 1;
    return n * factorial(n - 1);
}

