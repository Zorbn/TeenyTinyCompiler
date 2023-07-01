#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdbool.h>
void printThree(float a, float b, float c);
void printTwo(float a, float b);
void printThree(float a, float b, float c){
printTwo(a, b);
printf("%.2f\n", (float)(c));
}
void printTwo(float a, float b){
printf("%.2f\n", (float)(a));
printf("%.2f\n", (float)(b));
}
int main(void){
printThree(1.5, 2.0+3.0, 4.0/0.33);
}
