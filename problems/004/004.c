#include<stdbool.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<time.h>

bool is_palindrome(int n) {
  int reversed = 0, temp = n;

  while (temp != 0) {
    reversed = 10 * reversed + temp % 10;
    temp /= 10;
  }

  return reversed == n;
}

int solution() {
  int max = 0;

  for (int a = 100; a < 1000; a++)
    for (int b = 100; b < a; b++) {
      int p = a * b;

      if (p > max && is_palindrome(p))
        max = p;
    }

  return max;
}

int main(int argc, char *argv[]) {
  if (argc == 2 && strcmp(argv[1], "-a") == 0) {
    printf("%d\n", solution());
    return 0;
  }

  setlinebuf(stdout);

  char line[BUFSIZ];
  while (fgets(line, sizeof line, stdin)) {
    int iters = atoi(line);

    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC_RAW, &start);
    for (int i = 0; i < iters; i++) {
      solution();
    }
    clock_gettime(CLOCK_MONOTONIC_RAW, &end);

    long int secs = end.tv_sec - start.tv_sec;
    long int nsecs = end.tv_nsec - start.tv_nsec;

    printf("%ld\n", secs * 1000000000 + nsecs);
  }
}
