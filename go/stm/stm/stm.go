package stm

import (
  "errors"
  "log"
  "io/ioutil"
  "fmt"
  "sort"
)

type STMValue interface {
}

type TVar struct {
  holder chan STMValue
  notifiers map[chan bool]bool
}

type TVars []*TVar

func (t TVars) Len() int {
  return len(t)
}

func (t TVars) Less(i, j int) bool {
  iPtr :=fmt.Sprint(t[i])
  jPtr :=fmt.Sprint(t[j])

  return iPtr < jPtr
}

func (t TVars) Swap(i, j int) {
  t[i], t[j] = t[j], t[i]
}

func NewTVar(value STMValue) *TVar {
  retval := &TVar{
    holder: make(chan STMValue, 1),
    notifiers: make(map[chan bool]bool),
  }

  retval.holder <- value

  return retval
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

type Action func(atom *AtomicallyType) (STMValue, error)

func Retry() error {
  return errors.New("retry")
}

type AtomicallyType struct {
  trans Action
  state *RWSet
  notifier chan bool
}

func Atomically(trans1 Action) (STMValue, error) {
  atom := &AtomicallyType{
    trans: trans1,
    state: NewRWSet(),
    notifier: make(chan bool, 1),
  }

  return atom.execute()
}

func (a *AtomicallyType) ReadTVar(t *TVar) (STMValue, error) {
  if a.state.ws[t] == nil {
    value := <- t.holder
    t.holder <- value

    if a.state.rs[t] == nil {
      a.state.rs[t] = value
      return value, nil
    } else {
      if value != a.state.rs[t] {
        return nil, errors.New("rollback")
      } else {
        return value, nil
      }
    }
  } else {
    return a.state.ws[t], nil
  }
}

func (a *AtomicallyType) WriteTVar(t *TVar, v STMValue) {
  a.state.ws[t] = v
}

func (a *AtomicallyType) validate (storage map[*TVar]STMValue) bool {
  log.Println(storage)
  log.Println(a.state)
  // validate readset.
  for t, v := range a.state.rs {
    if storage[t] != v {
      return false;
    }
  }
  return true;
}

func (a *AtomicallyType) lockState() map[*TVar]STMValue {
  allTvars := make(map[*TVar]bool)
  for t, _ := range a.state.ws {
    allTvars[t] = true
  }

  for t, _ := range a.state.rs {
    allTvars[t] = true
  }

  // collect all unique tvars in a slice to sort them.
  slice := TVars{}
  for t, _ := range allTvars {
    slice = append(slice, t)
  }

  // sort tvars by pointer
  sort.Stable(slice)

  // lock the shit out of them.
  storage := make(map[*TVar]STMValue)
  for _, t := range(slice) {
    storage[t] = <- t.holder
  }

  return storage
}

func (a *AtomicallyType) execute() (STMValue, error) {
  log.SetOutput(ioutil.Discard)
  log.Println("===================")
  log.Println("Atomically start..")

  a.state = NewRWSet()

  log.Println(a.state)

  log.Println("Executing Trans..")
  result, err := a.trans(a)

  log.Println(result)
  log.Println(err)

  if err != nil {
    switch err.Error() {
    case "rollback":
      log.Println("Executing rollback!")
      log.Println("===================")
      return a.execute()
    case "retry":
      log.Println("Received a Retry!")
      log.Println("===================")
      log.Println(a.state)

      log.Println("Locking TVars in State.")
      storage := a.lockState()

      log.Println("Validating ...")
      valid := a.validate(storage)
      log.Println(valid)

      if valid {
        for k, _ := range(a.state.rs) {
          k.notifiers[a.notifier] = true
        }

        log.Println("Unlocking TVars in State.")
        // unlock and reset to old values
        for k, v := range storage {
          k.holder <- v
        }

        <- a.notifier

        // delete my notifier from the TVar.
        for k, _ := range(a.state.rs) {
          delete(k.notifiers, a.notifier)
        }

        // TODO: neuer channel fuer atomically.
        // atomically notifier channel mehrelementig, damit
        // andere zusätzlich notifzierende tvars nicht suspendieren
      } else {
        log.Println("Unlocking TVars in State.")
        // unlock and reset to old values
        for k, v := range storage {
          k.holder <- v
        }
      }

      log.Println("Apply Retry...")
      log.Println("===================")
      return a.execute()


    }
  }

  log.Println("Locking TVars in State.")
  storage := a.lockState()

  log.Println("Validating ...")
  valid := a.validate(storage)
  log.Println(valid)

  if !valid {
    // unlock and reset to old values
    for k, v := range storage {
      k.holder <- v
    }
    log.Println("Not Valid! Resetting...")
    log.Println("===================")
    return a.execute()
  } else {
    // commit
    for key, value := range(a.state.ws) {
      storage[key] = value
    }
    // unlock
    for k, v := range storage {
      k.holder <- v
    }

    // let all changing tvars notify for retry.
    for k, _ := range a.state.ws {
      for ch, _ := range k.notifiers {
        ch <- true
      }
    }

    log.Println("Successful Transaction!")
    log.Println("===================")
    return result, nil
  }
}
