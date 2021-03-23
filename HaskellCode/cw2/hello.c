#include <stdio.h>
int main(void) {
  char answer[100];
  printf("What is your name? \n");
  scanf("%s", answer);
  printf("hello, %s! \n", answer);
}