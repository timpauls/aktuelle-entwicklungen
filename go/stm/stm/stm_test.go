package stm

import (
  "testing"
)

func TestStm(foo *testing.T) {
  t := NewTVar(2)
  state.rs[t] = 42

  _, err := t.Get()
  if (err != nil) {
    foo.Log(err)
    return;
  }

  foo.Log(t.Get())
  foo.Log(t.Get())

  t.Set(3)
  t.Set(4)
  t.Set(5)

  foo.Log(t.Get())

  foo.Log(state)
  //
  // <- t.holder // lock.
  // t.holder <- 5 // unlock.
  // fmt.Println(t.get())
}
