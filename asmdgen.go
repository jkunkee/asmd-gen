package main

import "fmt"
import "./asmd"
import "io/ioutil"
import "flag"

func main() {
	flag.Parse()
	var filename string
	if len(flag.Args()) > 0 {
		filename = flag.Arg(0)
	} else {
		filename = "./json/repetitiveadditionmultiplier.json"
	}

	fmt.Println("Parse")
	machine, err := asmd.Parse(filename)
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println("Write VHDL")
	err = machine.VHDL("./example.vhdl")
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println("Dump VHDL")
	vhdl, err := ioutil.ReadFile("./example.vhdl")
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println(string(vhdl))
}
