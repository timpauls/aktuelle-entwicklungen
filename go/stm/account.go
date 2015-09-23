package main

import (
  "./stm"
  "time"
  "fmt"
)

type Account struct {
  tvar *stm.TVar
}

func NewAccount(amount int) *Account {
  return &Account{
    tvar: stm.NewTVar(amount),
  }
}

func (a *Account) getBalance(state *stm.RWSet) (int, error) {
  value, err := a.tvar.Get(state)
  return value.(int), err
}

func (a *Account) deposit(amount int, state *stm.RWSet) error {
  current, err := a.getBalance(state)
  a.tvar.Set(current + amount, state)
  return err
}

func (a *Account) transfer(to *Account, amount int, state *stm.RWSet) error {
  err := a.deposit(-amount, state)

  if (err != nil) {
    return err
  }

  err = to.deposit(amount, state)

  return err
}

func main() {
  k1 := NewAccount(100)
  k2 := NewAccount(200)

  atom1 := stm.Atomically()
  atom1.SetAction(func () (stm.STMValue, error) {
    err := k1.transfer(k2, 50, atom1.GetState())
    return nil, err
  })
  atom1.Execute()

  atom2 := stm.Atomically()
  atom2.SetAction(func () (stm.STMValue, error) {
    err := k2.transfer(k1, 10, atom2.GetState())
    return nil, err
  })

  go atom2.Execute()

  time.Sleep(1 * time.Second)

  atom3 := stm.Atomically()
  atom3.SetAction(func () (stm.STMValue, error) {
    fmt.Println(k1.getBalance(atom3.GetState()))
    fmt.Println(k2.getBalance(atom3.GetState()))
    return nil, nil
  })
  atom3.Execute()
}
