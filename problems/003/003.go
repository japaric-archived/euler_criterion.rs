package main

import "bufio"
import "fmt"
import "os"
import "strconv"
import "time"

func solution() uint64 {
    n := uint64(600851475143)

    for factor := uint64(3); ; factor += 2 {
        for n % factor == 0 {
            n /= factor
        }

        if factor * factor > n {
            return n;
        } else if n == 1 {
            return factor;
        }
    }
}

func main() {
    args := os.Args

    if len(args) == 2 && args[1] == "-a" {
        fmt.Println(solution())
        return
    }

    scanner := bufio.NewScanner(os.Stdin)
    for scanner.Scan() {
        iters, _ := strconv.Atoi(scanner.Text())

        start := time.Now()
        for i := 0; i < iters; i++ {
            solution()
        }
        end := time.Now()

        fmt.Println(end.Sub(start).Nanoseconds())
    }
}
