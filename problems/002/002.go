package main

import "bufio"
import "fmt"
import "os"
import "strconv"
import "time"

func solution() int {
    sum, curr, next := 0, 1, 2

    for curr < 4000000 {
        if curr % 2 == 0 {
            sum += curr
        }

        temp := next
        next += curr
        curr = temp
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
