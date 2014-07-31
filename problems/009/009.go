package main

import "bufio"
import "fmt"
import "os"
import "strconv"
import "time"

func solution() int {
    perimeter := 1000

    for c := perimeter / 3 + 1; c < perimeter / 2; c++ {
        for b := (perimeter - c) / 2 + 1; b < c; b++ {
            a := perimeter - b - c

            if a * a + b * b == c * c {
                return a * b * c
            }
        }
    }

    return 0
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
