package main

import "bufio"
import "fmt"
import "os"
import "strconv"
import "time"

func gcd(a int, b int) int {
    if a < b {
        temp := a
        a = b
        b = temp
    }

    r := a % b
    for r != 0 {
        a = b
        b = r
        r = a % b
    }

    return b
}

func lcm(a int, b int) int {
    return a * b / gcd(a, b)
}

func solution() int {
    n := 2

    for i := 3; i < 21; i++ {
        n = lcm(n, i)
    }

    return n
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
