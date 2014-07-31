import ctypes
import os
import sys

def solution():
    sum = 0

    for x in range(0, 1000):
        if x % 3 == 0 or x % 5 == 0:
            sum += x

    return sum

# XXX Shorter, 20%+ slower in Pypy, within noise levels (1%) in Python 2/3
#def solution():
    #return sum([x for x in range(0, 1000) if x % 3 == 0 or x % 5 == 0])

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
