package main

import "bufio"
import "fmt"
import "os"
import "strconv"
import "time"

func isPalindrome(n uint32) bool {
    reversed, temp := uint32(0), n

    for temp != 0 {
        reversed = 10 * reversed + temp % 10;
        temp /= 10;
    }

    return reversed == n;
}

func solution() uint32 {
    max := uint32(0)

    for a := uint32(100); a < 1000; a++ {
        for b := uint32(100); b < a; b++ {
            p := a * b

            if p > max && isPalindrome(p) {
                max = p
            }
        }
    }

    return max;
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
