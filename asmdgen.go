package main

import "fmt"
import "./asmd"
import "io/ioutil"

func main() {
	fmt.Println("Parse")
	machine, err := asmd.Parse("./asmd/example.json")

	fmt.Println("Dump VHDL")
	err = machine.VHDL("./example.vhdl")
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println("End")

	vhdl, err := ioutil.ReadFile("./example.vhdl")
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(string(vhdl))
}
