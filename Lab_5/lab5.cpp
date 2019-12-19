#include <iostream>

using namespace std;

//X^32+X^31+X^30+X^29+X^27+X^26+X^25+X^24+X^23+X^21+X^20+X^17+X^16+X^14+X^12+X^9+X^8+X^7+1

double RandomNumber()
{
	static unsigned long long s = 1;
	s = ((((s >> 31) ^ (s >> 30) ^ (s >> 29) ^ (s >> 28) ^ (s >> 26) ^ (s >> 24) ^ (s >> 23) ^
	(s >> 22) ^ (s >> 20) ^ (s >> 19) ^ (s >> 16) ^ (s >> 15) ^ (s >> 13) ^
	(s >> 11) ^ (s >> 8) ^ (s >> 7) ^ (s >> 6) ^ s) & 1) << 31) | (s >> 1);
	
	return static_cast<double>(s) / (4294967295.0);
	// 2 ^ 32 - 1 = 4294967295.0
}

int main()
{
	setlocale(LC_ALL, "rus");
	const int n = 10000;
	double a = 0.5, b = 2, c = 50, d = 125;
	for (int i = 0; i <= 100; i += 5) {
		double percent = i / 100.0;
		double s = 0;
		for (int j = 0; j < n; j++) {
			double r1 = RandomNumber(), r2 = RandomNumber();

			if (r1 < percent) 
				s += b;			
			if (r2 < a) {
				if (r1 < percent) {
					s += c;
				} else {
					s += d;
				}
			}	
		}
		cout << "Процент контроля: " << i << " \tЗатраты на " << n << " резисторов: " << s << endl;
	}
	system("pause");
	return 0;
}
