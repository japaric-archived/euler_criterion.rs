#include<stdint.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<time.h>

#define SIZE 20

uint64_t solution() {
  // DON'T OPTIMIZE ME AWAY!
  asm("");

  uint64_t grid[SIZE + 1][SIZE + 1];

  for (int i = 0; i <= SIZE; i++)
    for (int j = 0; j <= SIZE; j++)
      if (i == 0 || j == 0)
        grid[i][j] = 1;
      else
        grid[i][j] = grid[i - 1][j] + grid[i][j - 1];

  return grid[SIZE][SIZE];
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
