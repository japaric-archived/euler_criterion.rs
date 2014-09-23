#include<cstdint>
#include<cstring>
#include<ctime>
#include<iostream>
#include<string>
#include<unordered_map>

using namespace std;

uint32_t solution() {
  uint32_t i = 0, n = 10000;
  unordered_map<uint32_t, uint32_t> map;

  for (uint32_t q = 2;; q++) {
    uint32_t p = map[q];

    if (p == 0) {
      map[q * q] = q;

      if (i == n) {
        return q;
      } else {
        i += 1;
      }
    } else {
      uint32_t x = p + q;

      while (map[x] != 0) {
        x += p;
      }

      map[x] = p;
    }
  }
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
