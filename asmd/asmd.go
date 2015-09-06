package asmd

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

type StateMachine struct {
	Options    Options
	Inputs     map[string]Variable
	Outputs    map[string]Variable
	Parameters map[string]Variable
	Registers  map[string]Variable
	//States map[string]State
	//Conditions map[string]Condition
}

type Options struct {
	ModuleName    string
	ClockType     string // posedge, negedge, ddr?
	AddAsyncReset bool
	FirstState    string
	Indent        string
	Author        string
}

type Variable struct {
	BitWidth     uint64
	Type         string
	DefaultValue string
	Value        string
}

func Parse(filename string) (*StateMachine, error) {
	fmt.Println("Hello World!")
	mac := new(StateMachine)

	fileBytes, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	err = json.Unmarshal(fileBytes, mac)

	return mac, err
}

// TODO make this durned thing not throw exceptions, or catch them locally
func write(f *os.File, ss ...string) {
	for _, s := range ss {
		n, err := f.WriteString(s)
		if err != nil {
			panic(err)
		}
		if n != len(s) {
			panic("Unable to write full string to file")
		}
	}
}

func (m *StateMachine) indent(n uint) string {
	s := ""
	var i uint
	if m.Options.Indent == "" {
		m.Options.Indent = "    "
	}
	for i = 0; i < n; i++ {
		s += m.Options.Indent
	}
	return s
}

func (m *StateMachine) VHDL(filename string) (err error) {
	file, err := os.Create(filename)
	if err != nil {
		return err
	}

	replacer := strings.NewReplacer(" ", "", "\t", "")
	trimmedModuleName := replacer.Replace(m.Options.ModuleName)

	// Comments
	write(file, "--------------------------------------------------------------------------------\n")
	write(file, "--\n")
	write(file, "-- Module Name: ", m.Options.ModuleName, "\n")
	write(file, "-- Author:      ", m.Options.Author, "\n") // TODO
	write(file, "-- Date:        ", "\n")                   // TODO
	write(file, "--\n")
	write(file, "--------------------------------------------------------------------------------\n")
	write(file, "\n")

	// library and use statements
	// TODO infer the minimal set using given types
	write(file, "library IEEE;\n")
	write(file, "use IEEE.STD_LOGIC_1164.ALL;\n")
	write(file, "use IEEE.NUMERIC_STD.ALL;\n")

	// entity start
	write(file, "entity ", trimmedModuleName, " is\n")

	// Entity - Generics
	if len(m.Parameters) > 0 {
		write(file, m.indent(1), "generic (\n")
		isFirst := true
		for name, properties := range m.Parameters {
			write(file, m.indent(2))
			if isFirst {
				write(file, "  ")
				isFirst = false
			} else {
				write(file, "; ")
			}
			write(file, name, ": ", properties.Type, " := ", properties.DefaultValue)
			write(file, "\n")
		}
		write(file, m.indent(1), ");\n")
	}

	if len(m.Inputs) > 0 || len(m.Outputs) > 0 {
		write(file, m.indent(1), "port (\n")
		var isFirst bool

		// Entity - Inputs
		isFirst = true
		for name, properties := range m.Inputs {
			write(file, m.indent(2))
			if isFirst {
				write(file, "  ")
				isFirst = false
			} else {
				write(file, "; ")
			}
			write(file, name, " : in std_logic")
			if properties.BitWidth > 1 {
				write(file, "_vector (", strconv.FormatUint(properties.BitWidth-1, 10), " downto 0)")
			}
			write(file, "\n")
		}

		// Entity - Outputs
		// We're merely continuing the same list so don't reset isFirst.
		// TODO make this DRY with Inputs section
		for name, properties := range m.Outputs {
			write(file, m.indent(2))
			if isFirst {
				write(file, "  ")
				isFirst = false
			} else {
				write(file, "; ")
			}
			write(file, name, " : out std_logic")
			if properties.BitWidth > 1 {
				write(file, "_vector (", strconv.FormatUint(properties.BitWidth-1, 10), " downto 0)")
			}
			write(file, "\n")
		}

		write(file, m.indent(1), ");\n")
	}

	// Entity end
	write(file, "end ", trimmedModuleName, ";\n")
	write(file, "\n")

	// architecture start
	write(file, "architecture Behavioral of ", trimmedModuleName, " is\n")

	// Constants (?)
	// Internal Signals

	write(file, m.indent(1), "-- FSM declarations\n")
	// State Machine "Next"s
	write(file, m.indent(1), "type state is (")
	//for stateName, _ := range m.States {}
	write(file, ");\n")
	// State machine states
	//if _, ok := m.Options.FirstState in  // verify FirstState is valid
	write(file, m.indent(1), "signal state_reg, state_next : state := ", m.Options.FirstState, ";\n")

	// architecture "begin"
	write(file, "begin\n")

	// Register process
	// Next State process
	// Mealy(?) Output process
	// architecture end
	write(file, "end Behavioral;\n")
	write(file, "\n")

	return nil
}
