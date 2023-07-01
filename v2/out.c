#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdbool.h>

int main(void) {
	int a = 5;
	while (a<10) {
		a = a+1;
	}
	printf("%.2f\n", (float)(a));
}
