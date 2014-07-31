package main

import "bufio"
import "fmt"
import "io/ioutil"
import "os"
import "strconv"
import "time"

func solution(str []byte) int {
    digits, max, pos := [13]byte {}, 0, 0

    for _, element := range str {
        if element != '\n' {
            digits[pos] = element - '0'

            p := 1
            for _, digit := range digits {
                p *= int(digit)
            }

            if p > max {
                max = p
            }

            pos = (pos + 1) % 13
        }
    }

    return max
}

func main() {
    str, _ := ioutil.ReadFile("008.txt")
    args := os.Args

    if len(args) == 2 && args[1] == "-a" {
        fmt.Println(solution(str))
        return
    }

    scanner := bufio.NewScanner(os.Stdin)
    for scanner.Scan() {
        iters, _ := strconv.Atoi(scanner.Text())

        start := time.Now()
        for i := 0; i < iters; i++ {
            solution(str)
        }
        end := time.Now()

        fmt.Println(end.Sub(start).Nanoseconds())
    }
}
