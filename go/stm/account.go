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

func (a *Account) getBalance() (int, error) {
  value, err := a.tvar.Get()
  return value.(int), err
}

func (a *Account) deposit(amount int) error {
  current, err := a.getBalance()
  a.tvar.Set(current + amount)
  return err
}

func (a *Account) transfer(to *Account, amount int) error {
  err := a.deposit(-amount)

  if (err != nil) {
    return err
  }

  err = to.deposit(amount)

  return err
}

func main() {
  k1 := NewAccount(100)
  k2 := NewAccount(200)

  stm.Atomically(func () (stm.STMValue, error) {
    err := k1.transfer(k2, 50)
    return nil, err
  })

  go stm.Atomically(func () (stm.STMValue, error) {
    err := k2.transfer(k1, 10)
    return nil, err
  })

  time.Sleep(2 * time.Second)

  fmt.Println(k1.getBalance())
  fmt.Println(k2.getBalance())
}
