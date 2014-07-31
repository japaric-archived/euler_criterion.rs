#include<stdint.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<time.h>

#define WINDOW 13

uint64_t product(char *digits) {
  uint64_t p = 1;

  for (int i = 0; i < WINDOW; i++)
    p *= digits[i];

  return p;
}

uint64_t solution(char *string) {
  char digits[WINDOW] = { 0 };
  int pos = 0;
  uint64_t max = 0;

  int i = 0;
  while (1) {
    char chr = string[i++];

    if (chr == 0)
      break;
    else if (chr != '\n') {
      digits[pos] = chr - 48;

      uint64_t p = product(digits);
      if (p > max)
        max = p;

      pos = (pos + 1) % WINDOW;
    }
  }

  return max;
}

int main(int argc, char *argv[]) {
  FILE *file = fopen("008.txt", "r");
  if (file == NULL) {
    fputs("File error\n", stderr);
    exit(1);
  }

  fseek(file, 0, SEEK_END);
  int size = ftell(file);
  rewind(file);

  char *buffer = malloc(sizeof(char) * size);
  if (buffer == NULL) {
    fputs("Memory error\n", stderr);
    exit(2);
  }

  size_t result = fread(buffer, 1, size, file);
  if (result != size) {
    fputs("Reading error\n", stderr);
    exit(3);
  }
  fclose(file);

  if (argc == 2 && strcmp(argv[1], "-a") == 0) {
    printf("%lu\n", solution(buffer));
    free(buffer);
    return 0;
  }

  setlinebuf(stdout);

  char line[BUFSIZ];
  while (fgets(line, sizeof line, stdin)) {
    int iters = atoi(line);

    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC_RAW, &start);
    for (int i = 0; i < iters; i++) {
      solution(buffer);
    }
    clock_gettime(CLOCK_MONOTONIC_RAW, &end);

    long int secs = end.tv_sec - start.tv_sec;
    long int nsecs = end.tv_nsec - start.tv_nsec;

    printf("%ld\n", secs * 1000000000 + nsecs);
  }

  free(buffer);
}
