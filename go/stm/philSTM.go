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

func (s *Stick) take(atom *stm.AtomicallyType) error {
  value, err := atom.ReadTVar(s.tvar)
  taken := value.(bool)

  if err != nil {
    return err
  }

  if (taken) {
    return stm.Retry()
  }

  atom.WriteTVar(s.tvar, true)

  return nil
}

func (s *Stick) put(atom *stm.AtomicallyType) error {
  atom.WriteTVar(s.tvar, false)
  return nil
}

func phil(s1,s2 *Stick, name string) {
  for {
    stm.Atomically(func(atom *stm.AtomicallyType) (stm.STMValue, error) {
      err := s1.take(atom)

      if err != nil {
        // fmt.Printf("%s in %s.\n", err.Error(), name)
        return nil, err
      }

      err = s2.take(atom)

      if err != nil {
        // fmt.Printf("%s in %s.\n", err.Error(), name)
        return nil, err
      }

      return nil, nil
    })

    fmt.Println(name + " is eating...")
    time.Sleep(1 * time.Second)

    stm.Atomically(func(atom *stm.AtomicallyType) (stm.STMValue, error) {
      s1.put(atom)
      s2.put(atom)
      return nil, nil
    })

    time.Sleep(1 * time.Second)
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
