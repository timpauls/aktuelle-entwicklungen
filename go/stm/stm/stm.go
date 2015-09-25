package stm

import (
  "errors"
  "log"
  "io/ioutil"
)

type STMValue interface {
}

type TVar struct {
  holder chan STMValue
  notifiers map[chan bool]bool
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

type Action func() (STMValue, error)

func Retry() error {
  return errors.New("retry")
}

type AtomicallyType struct {
  trans Action
  state *RWSet
  notifier chan bool
}

func Atomically() *AtomicallyType {
  return &AtomicallyType{
    trans: nil,
    state: NewRWSet(),
    notifier: make(chan bool, 1),
  }
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

func (a *AtomicallyType) SetAction(trans Action) {
  a.trans = trans
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
  // TODO: erst alle TVars sammel, dann sortieren, dann locken

  // lock all tvars and remember values in them.
  storage := make(map[*TVar]STMValue)

  for t, _ := range a.state.ws {
    _, exists := storage[t]
    // do not try to double-lock a tvar :-(
    if !exists {
      storage[t] = <- t.holder
    }
  }

  for t, _ := range a.state.rs {
    _, exists := storage[t]
    // do not try to double-lock a tvar :-(
    if !exists {
      storage[t] = <- t.holder
    }
  }

  return storage
}

func (a *AtomicallyType) Execute() (STMValue, error) {
  log.SetOutput(ioutil.Discard)
  log.Println("===================")
  log.Println("Atomically start..")

  a.state = NewRWSet()

  log.Println(a.state)

  log.Println("Executing Trans..")
  result, err := a.trans()

  log.Println(result)
  log.Println(err)

  if err != nil {
    switch err.Error() {
    case "rollback":
      log.Println("Executing rollback!")
      log.Println("===================")
      return a.Execute()
    case "retry":
      log.Println(a.state)

      //TODO: validate bevor man wartet.

      for k, _ := range(a.state.rs) {
        k.notifiers[a.notifier] = true
      }

      <- a.notifier

      // delete my notifier from the TVar.
      for k, _ := range(a.state.rs) {
        delete(k.notifiers, a.notifier)
      }

      // TODO: neuer channel fuer atomically.
      // atomically notifier channel mehrelementig, damit
      // andere zusätzlich notifzierende tvars nicht suspendieren.

      log.Println("Apply Retry...")
      log.Println("===================")
      return a.Execute()
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
    return a.Execute()
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
