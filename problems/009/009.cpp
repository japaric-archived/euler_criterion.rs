#include<cstring>
#include<ctime>
#include<iostream>
#include<string>

using namespace std;

#define PERIMETER 1000

int solution() {
  for (int c = PERIMETER / 3 + 1; c < PERIMETER / 2; c++)
    for (int b = (PERIMETER - c) / 2 + 1; b < c; b++) {
      int a = PERIMETER - b - c;

      if (a * a + b * b == c * c)
        return a * b * c;
    }

  // XXX Don't let the compiler optimize this function away
  asm("");

  return 0;
}

int main(int argc, char* argv[]) {
  if (argc == 2 && strcmp(argv[1], "-a") == 0) {
    cout << solution() << endl;
    return 0;
  }

  for (string line; getline(cin, line);) {
    int iters = atoi(line.c_str());

    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC_RAW, &start);
    for (int i = 0; i < iters; i++) {
      solution();
    }
    clock_gettime(CLOCK_MONOTONIC_RAW, &end);

    long int secs = end.tv_sec - start.tv_sec;
    long int nsecs = end.tv_nsec - start.tv_nsec;
    cout << secs * 1000000000 + nsecs << endl;
  }

  return 0;
}
