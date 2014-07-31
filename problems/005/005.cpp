#include<cstring>
#include<ctime>
#include<iostream>
#include<string>

using namespace std;

uint64_t gcd(uint64_t a, uint64_t b) {
  if (a < b) {
    uint64_t temp = a;
    a = b;
    b = temp;
  }

  uint64_t r;
  do {
    r = a % b;
    a = b;
    b = r;
  } while (r);

  return a;
}

uint64_t lcm(uint64_t a, uint64_t b) {
  return a * b / gcd(a, b);
}

int solution() {
  uint64_t n = 2;

  for (uint64_t i = 3; i < 21; i++) {
    n = lcm(n, i);
  }

  return n;
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
