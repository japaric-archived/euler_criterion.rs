#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<time.h>

int solution() {
  int sum = 0, curr = 1, next = 2;

  while (curr < 4000000) {
    if (curr % 2 == 0)
      sum += curr;

    int temp = next;
    next += curr;
    curr = temp;
  }

  return sum;
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
