#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
int main() {
	for (int x = 0; x < 32; x++) {
		int j = x-16;
		double i = (powf(2,((float)j)/12.0));
		i = (1.0/i)*256.0;
		putchar(((int)round(i))&0xff);
	}
	for (int x = 0; x < 32; x++) {
		int j = x-16;
		double i = (powf(2,((float)j)/12.0));
		i = (1.0/i)*256.0;
		putchar(((int)round(i))>>8&0xff);
	}
	return 0;
}
