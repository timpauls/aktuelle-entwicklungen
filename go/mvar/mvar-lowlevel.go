package main

import (
  "fmt"
  "sync"
)

type MVar struct {
  value []int
  reader *sync.Cond
  writer *sync.Cond
}

func NewMVar () *MVar {
  return &MVar{
    value: nil,
    reader: sync.NewCond(&sync.Mutex{}),
    writer: sync.NewCond(&sync.Mutex{}),
  }
}

func (m * MVar) isEmpty() bool {
  return len(m.value) == 0
}

func (m * MVar) put(v int) {
  m.writer.L.Lock() // equivalent to open synchronized

  for !m.isEmpty() {
    m.writer.Wait()
  }

  m.reader.L.Lock()
  m.value = append(m.value, v)
  m.reader.Broadcast() // equivalent to notifyAll
  m.reader.L.Unlock()

  m.writer.L.Unlock() // equivalent to close synchronized
}

func (m * MVar) take() int {
  m.reader.L.Lock()

  for m.isEmpty() {
    m.reader.Wait()
  }

  m.writer.L.Lock()
  result := m.value[0]
  m.value = nil
  m.writer.Broadcast()
  m.writer.L.Unlock()

  m.reader.L.Unlock()
  return result
}

var value []int = make([]int, 0, 1)

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
