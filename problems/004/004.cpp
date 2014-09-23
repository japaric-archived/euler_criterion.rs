#include<cstdint>
#include<cstring>
#include<ctime>
#include<iostream>
#include<string>

using namespace std;

bool isPalindrome(uint32_t n) {
  uint32_t reversed = 0, temp = n;

  while (temp != 0) {
    reversed = 10 * reversed + temp % 10;
    temp /= 10;
  }

  return reversed == n;
}

uint32_t solution() {
  uint32_t max = 0;

  for (uint32_t a = 100; a < 1000; a++)
    for (uint32_t b = 100; b < a; b++) {
      uint32_t p = a * b;

      if (p > max && isPalindrome(p))
        max = p;
    }

  return max;
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
