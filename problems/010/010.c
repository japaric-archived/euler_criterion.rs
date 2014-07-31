#include<stdbool.h>
#include<stdint.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<time.h>

#define LIMIT 2000000
#define SIZE (LIMIT - 1) / 2

uint64_t solution() {
  bool *sieve = calloc(SIZE, sizeof(bool));
  uint64_t sum = 2;

  for (int i = 0; i < SIZE; i++)
    if (!sieve[i]) {
      uint64_t p = 2 * i + 3;

      sum += p;

      for (uint64_t j = p * p; j < LIMIT; j += 2 * p)
        sieve[(j - 3) / 2] = true;
    }

  free(sieve);

  return sum;
}

int main(int argc, char *argv[]) {
  if (argc == 2 && strcmp(argv[1], "-a") == 0) {
    printf("%lu\n", solution());
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
