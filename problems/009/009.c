#include<stdint.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<time.h>

#define PERIMETER 1000

uint32_t solution() {
  for (uint32_t c = PERIMETER / 3 + 1; c < PERIMETER / 2; c++)
    for (uint32_t b = (PERIMETER - c) / 2 + 1; b < c; b++) {
      uint32_t a = PERIMETER - b - c;

      if (a * a + b * b == c * c)
        return a * b * c;
    }

  // DON'T OPTIMIZE ME AWAY!
  asm("");

  return 0;
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
