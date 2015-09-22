package stm

import (
  "errors"
  "fmt"
)

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

func (t *TVar) Get() (STMValue, error) {
  if state.ws[t] == nil {
    value := <- t.holder
    t.holder <- value

    if state.rs[t] == nil {
      state.rs[t] = value
      return value, nil
    } else {
      if value != state.rs[t] {
        return nil, errors.New("rollback")
      } else {
        return value, nil
      }
    }
  } else {
    return state.ws[t], nil
  }
}

func (t *TVar) Set(v STMValue) {
  state.ws[t] = v
}

var state *RWSet = NewRWSet()

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

type Action func() (STMValue, error)

func lockState(state *RWSet) map[*TVar]STMValue {
  // lock all tvars and remember values in them.
  storage := make(map[*TVar]STMValue)

  for t, _ := range state.ws {
    _, exists := storage[t]
    // do not try to double-lock a tvar :-(
    if !exists {
      storage[t] = <- t.holder
    }
  }

  for t, _ := range state.rs {
    _, exists := storage[t]
    // do not try to double-lock a tvar :-(
    if !exists {
      storage[t] = <- t.holder
    }
  }

  return storage
}

func validate (storage map[*TVar]STMValue, state *RWSet) bool {
  fmt.Println(storage)
  fmt.Println(state)
  // validate readset.
  for t, v := range state.rs {
    if storage[t] != v {
      return false;
    }
  }
  return true;
}

func Atomically (trans Action) STMValue {
  state = NewRWSet() // clear rw-set.

  fmt.Println("Atomically start..")
  fmt.Println(state)

  fmt.Println("Executing Trans..")
  result, err := trans()

  fmt.Println(result)
  fmt.Println(err)

  if err != nil {
    switch err.Error() {
    case "rollback":
      fmt.Println("Executing rollback!")
      return Atomically(trans)
    }
  }

  fmt.Println("Locking State.")
  storage := lockState(state)

  fmt.Println("Validating ...")
  valid := validate(storage, state)
  fmt.Println(valid)

  if !valid {
    // unlock and reset to old values
    for k, v := range storage {
      k.holder <- v
    }
    return Atomically(trans)
  } else {
    // commit
    for key, value := range(state.ws) {
      storage[key] = value
    }

    // unlock
    for k, v := range storage {
      k.holder <- v
    }

    return result
  }
}
