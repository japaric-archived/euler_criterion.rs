#include<stdint.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<time.h>

#define LIMIT 1000000

int collatz_length(uint64_t n, int *lengths) {
  if (n < LIMIT) {
    int length = lengths[n];

    if (length)
      return length;
  }

  int length = 1 + collatz_length(n % 2 ? 3 * n + 1 : n / 2, lengths);

  if (n < LIMIT)
    lengths[n] = length;

  return length;
}

int solution() {
  int *lengths = calloc(LIMIT, sizeof(int));
  int ans = 0, max_length = 0;
  lengths[1] = 1;

  for (int i = 2; i <= LIMIT; i++) {
    int length = collatz_length(i, lengths);

    if (length > max_length) {
      max_length = length;
      ans = i;
    }
  }

  return ans;
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
