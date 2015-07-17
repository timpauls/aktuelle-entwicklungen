package main

import (
	"fmt";
	"time"
)

func main() {
	// sticks erstellen
	s1 := make(chan bool, 1)
	s2 := make(chan bool, 1)
	s3 := make(chan bool, 1)
	s4 := make(chan bool, 1)
	s5 := make(chan bool, 1)

	s1 <- true
	s2 <- true
	s3 <- true
	s4 <- true
	s5 <- true

	// philosophen erstellen und anfangen lassen
	go phil(1, s1, s2)
	go phil(2, s2, s3)
	go phil(3, s3, s4)
	go phil(4, s4, s5)

	time.Sleep(2 * time.Second)
	fmt.Println("Deadlock Phil kicks in.")
	phil(5, s5, s1)
}

func phil(i int, l chan bool, r chan bool) {
	// denken.
	time.Sleep(50 * time.Millisecond)

	// sticks nehmen.
	left := <- l
	right := <- r
	// essen.
	fmt.Printf("%v", i)
	// sticks zurücklegen.
	l <- left
	r <- right
	phil(i, l, r)
}

