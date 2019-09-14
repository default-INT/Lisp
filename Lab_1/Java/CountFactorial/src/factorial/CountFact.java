package factorial;

import java.util.Scanner;

public class CountFact {
	public static void main(String[] args) {
		
		int k = 0;
		while (k < 25) {
			System.out.println(k + "! = " + fact(k));
			k++;
		}
	}
	
	public static long fact(long n) {
		if (n == 0 || n == 1) return 1;
		else return n * fact(n - 1);
	}
}
