import ctypes
import itertools
import os
import sys

def primes():
    map_ = {}

    for q in itertools.count(2):
        p = map_.pop(q, None)

        if p is None:
            yield q

            map_[q + q] = q
        else:
            x = p + q

            while x in map_:
                x += p

            map_[x] = p

def solution():
    return next(itertools.islice(primes(), 10000, 10000 + 1))

args = sys.argv

if len(args) == 2 and args[1] == '-a':
    print(solution())
    sys.exit()

CLOCK_MONOTONIC_RAW = 4

class timespec(ctypes.Structure):
    _fields_ = [
        ('tv_sec', ctypes.c_long),
        ('tv_nsec', ctypes.c_long)
    ]

librt = ctypes.CDLL('librt.so.1', use_errno=True)
clock_gettime = librt.clock_gettime
clock_gettime.argtypes = [ctypes.c_int, ctypes.POINTER(timespec)]

while True:
    iters = int(input())

    start, end = timespec(), timespec()
    clock_gettime(CLOCK_MONOTONIC_RAW, ctypes.byref(start))
    for _ in range(iters):
        solution()
    clock_gettime(CLOCK_MONOTONIC_RAW, ctypes.byref(end))

    secs = end.tv_sec - start.tv_sec
    nsecs = end.tv_nsec - start.tv_nsec

    print(secs * 1000000000 + nsecs)
    sys.stdout.flush()
