package main

import (
  "fmt"
  "time"
)

var a int = 1

func main() {
  go checker(1)
  go checker(2)

  time.Sleep(5 * time.Second)
}

// hier sieht man, dass checker nicht atomar ausgeführt wird.
// sub-routinen sind in ihrer ausführung also nicht atomar,
// das ergibt übliche probleme beim verwenden von gemeinsamen speicher.
func checker(index int) {
  fmt.Printf("routine: %v \n", index)

  time.Sleep(1 * time.Second)
  a = a + 1
  fmt.Printf("%v First a=%v\n", index, a)
  time.Sleep(1 * time.Second)
  a = a * 2
  fmt.Printf("%v Second a=%v\n", index, a)
  time.Sleep(1 * time.Second)

  fmt.Printf("%v finished.\n", index)
}
