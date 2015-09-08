package main

import (
  "fmt"
  "time"
)

// shared number variable.
var number int = 0

func main() {
  // ohne go-routines.
  fmt.Println(number)
  number = number + 1

  fmt.Println(number)

  // mit go-routines.
  go increment()
  time.Sleep(1 * time.Second)

  go printNumber()

  // benötigt, damit der go-prozess nicht vor ausführung der routine
  // terminiert.
  time.Sleep(1 * time.Second)
}

func printNumber() {
  fmt.Println(number)
}

func increment() {
  number = number + 1
}
