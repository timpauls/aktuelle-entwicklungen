package main

import (
  "fmt"
  "time"
)

type Stick struct {
  taken chan bool
}

func MakeStick() *Stick {
  return &Stick{
    taken: make(chan bool, 1),
  }
}

func (s * Stick) take() {
  s.taken <- true
}

func (s * Stick) put() {
  <- s.taken
}

func (s * Stick) isTaken() bool {
  return len(s.taken) == 1
}

func main() {
  s1 := MakeStick()
  s2 := MakeStick()
  s3 := MakeStick()
  s4 := MakeStick()
  s5 := MakeStick()

  // deadlocking dumb philosophers!
  // go phil_deadlock(s1, s2, "1")
  // go phil_deadlock(s2, s3, "2")
  // go phil_deadlock(s3, s4, "3")
  // go phil_deadlock(s4, s5, "4")
  // go phil_deadlock(s5, s1, "5")

  // clever & smart.
  go phil_smart(s1, s2, "1")
  go phil_smart(s2, s3, "2")
  go phil_smart(s3, s4, "3")
  go phil_smart(s4, s5, "4")
  go phil_smart(s5, s1, "5")

  <- make(chan int) // don't terminate dude!
}

func phil_deadlock(s1, s2 *Stick, name string) {
  for {
    fmt.Println(name + " is thinking.")
    time.Sleep(150 * time.Millisecond)
    s1.take()
    time.Sleep(300 * time.Millisecond) // provoke deadlock situation.
    s2.take()
    fmt.Println(name + " is eating!")
    time.Sleep(300 * time.Millisecond)
    s1.put()
    s2.put()
  }
}

func phil_smart(s1, s2 * Stick, name string) {
  for {
    fmt.Println(name + " is thinking.")
    time.Sleep(150 * time.Millisecond)
    s1.take()
    time.Sleep(300 * time.Millisecond)
    if !s2.isTaken() {
      s2.take()
      fmt.Println(name + " is eating!")
      time.Sleep(300 * time.Millisecond)
      s2.put()
    }

    s1.put()
  }
}
