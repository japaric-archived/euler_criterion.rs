import ctypes
import os
import sys

def solution():
    sum, curr, next_ = 0, 1, 2

    while curr < 4000000:
        if curr % 2 == 0:
            sum += curr

        temp = next_
        next_ += curr
        curr = temp

    return sum

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
