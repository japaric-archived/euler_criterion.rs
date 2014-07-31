package main

import "bufio"
import "fmt"
import "os"
import "strconv"
import "time"

func solution() int {
    sum := 0

    for i := 0; i < 1000; i++ {
        if i % 3 == 0 || i % 5 == 0 {
            sum += i
        }
    }

    return sum
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
