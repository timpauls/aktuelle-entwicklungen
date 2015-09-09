package main

import (
  "fmt"
)

type MVar struct {
  value chan int
}

func NewMVar () *MVar {
  return &MVar{
    value: make(chan int, 1),
  }
}

func (m * MVar) isEmpty() bool {
  return len(m.value) == 0
}

func (m * MVar) put(v int) {
  m.value <- v
}

func (m * MVar) take() int {
  return <- m.value
}

func main() {
  m := NewMVar()

  // DOUBLE PUT SUSPENDS ON SECOND
  // fmt.Println(m.isEmpty())
  // m.put(1)
  // fmt.Println(m.isEmpty())
  // m.put(1)

  // TAKE ON EMPTY SUSPENDS
  // fmt.Println(m.take())

  // PUT AND TAKE WORKS
  // fmt.Println(m.isEmpty())
  // m.put(1)
  // fmt.Println(m.take())

  // mvar usage with concurrency
  go func() {
    for {
      fmt.Println(m.take())
    }
  }()

  go func() {
    for {
      m.put(0)
    }
  }()

  go func() {
    for {
      m.put(1)
    }
  }()

  <-  make(chan int) // main-process is never terminating.
}
