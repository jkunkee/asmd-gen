package main

import "fmt"
import "./asmd"
import "io/ioutil"

func main() {
	fmt.Println("Begin")
	machine, err := asmd.Parse("./asmd/example.json")
	fmt.Println(machine, err)

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
