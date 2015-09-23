package main

import (
  "./stm"
  "time"
  "fmt"
)

type Stick struct {
  tvar *stm.TVar
}

func NewStick() *Stick {
  return &Stick{
    tvar: stm.NewTVar(false),
  }
}

func (s *Stick) take() error {
  value, err := s.tvar.Get()
  taken := value.(bool)

  if err != nil {
    return err
  }

  if (taken) {
    return stm.Retry()
  }

  s.tvar.Set(true)

  return nil
}

func (s *Stick) put() error {
  s.tvar.Set(false)
  return nil
}

func phil(s1,s2 *Stick, name string) {
  for {
    stm.Atomically(func() (stm.STMValue, error) {
      err := s1.take()

      if err != nil {
        fmt.Printf("%s in %s.\n", err.Error(), name)
        return nil, err
      }

      err = s2.take()

      if err != nil {
        fmt.Printf("%s in %s.\n", err.Error(), name)
        return nil, err
      }

      return nil, nil
    })

    fmt.Println(name + " is eating...")
    time.Sleep(1 * time.Second)

    stm.Atomically(func() (stm.STMValue, error) {
      s1.put()
      s2.put()
      return nil, nil
    })
    fmt.Println(name + " put back...")
  }
}

func main() {
  s1 := NewStick()
  s2 := NewStick()
  s3 := NewStick()
  s4 := NewStick()
  s5 := NewStick()

  go phil(s1, s2, "1")
  go phil(s2, s3, "2")
  go phil(s3, s4, "3")
  go phil(s4, s5, "4")
  phil (s5, s1, "5")
}
