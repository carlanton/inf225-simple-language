#include <stdio.h>

extern int printint(int x) {
  printf("%d\n", x);
  return 0;
}

extern int readint() {
    int x;
    printf("-> ");
    scanf("%d", &x);
    return x;
}
