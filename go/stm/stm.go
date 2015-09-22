package main

import (
  "fmt"
)

var state *RWSet = NewRWSet()

type STMValue interface {
}

type TVar struct {
  holder chan STMValue
}

func NewTVar(value STMValue) *TVar {
  retval := &TVar{
    holder: make(chan STMValue, 1),
  }

  retval.holder <- value

  return retval
}

func (t *TVar) get() STMValue {
  if state.ws[t] == nil {
    value := <- t.holder
    t.holder <- value

    if state.rs[t] == nil {
      state.rs[t] = value
      return value
    } else {
      if value != state.rs[t] {
        panic("rollback")
      } else {
        return value
      }
    }
  } else {
    return state.ws[t]
  }
}

func (t *TVar) set(v STMValue) {
  <- t.holder
  state.ws[t] = v
  t.holder <- v
}

type RWSet struct {
  rs map[*TVar]STMValue
  ws map[*TVar]STMValue
}

func NewRWSet() *RWSet {
  return &RWSet{
    rs: make(map[*TVar]STMValue),
    ws: make(map[*TVar]STMValue),
  }
}

type Action func() STMValue

func atomically (trans Action) STMValue {
  state = NewRWSet() // clear rw-set.
  return nil
}


func main() {
  t := NewTVar(2)
  state.rs[t] = 42

  fmt.Println(t.get())
  fmt.Println(t.get())

  t.set(3)
  t.set(4)
  t.set(5)

  fmt.Println(t.get())

  fmt.Println(state)
  //
  // <- t.holder // lock.
  // t.holder <- 5 // unlock.
  // fmt.Println(t.get())

}
