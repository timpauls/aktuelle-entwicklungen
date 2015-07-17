package main

import(
	"fmt"
	"time"
)

func main() {
	c := make(chan bool, 1)

	c <- true

	go chantest(c)

	time.Sleep(5 * time.Second)
}

func chantest(ch chan bool) {
	c1 := <-ch
	fmt.Println(c1)

	c2 := <-ch
	fmt.Println(c2)

	c3 := <-ch
	fmt.Println(c3)
}