
package asmd

import (
	"fmt"
)

type StateMachine struct {
	Options Options
	
}

type Options struct {
	ModuleName string
	ClockType string // posedge, negedge, ddr?
	AddAsyncReset bool
}

type Variable struct {
	
}

func Hi() {
	fmt.Println("Hello World!")
}
