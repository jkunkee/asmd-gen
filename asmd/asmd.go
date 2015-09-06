package asmd

import (
	"encoding/json"
	"errors"
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
	ModuleName        string
	trimmedModuleName string // valid C identifier form of ModuleName
	ClockType         string // posedge, negedge
	AddAsyncReset     *bool  // default true
	FirstState        string // must be in States
	Indent            string // default four spaces
	Author            string
}

type Variable struct {
	BitWidth     uint64 // >1 invokes simple HDL array types
	Type         string // natural, std_logic_vector, etc. Default: std_logic
	DefaultValue string // TODO default to zero value or no default?
}

func Parse(filename string) (*StateMachine, error) {
	mac := new(StateMachine)

	fileBytes, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	err = json.Unmarshal(fileBytes, mac)

	if err != nil {
		return nil, err
	}

	// Set defaults on zero-valued inputs, validate input
	if len(mac.Inputs) == 0 {
		return nil, errors.New("No inputs specified.")
	}

	return mac, nil
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
	defer file.Close()

	// fix up module name to be a valid VHDL module name
	replacer := strings.NewReplacer(" ", "", "\t", "")
	trimmedModuleName := replacer.Replace(m.Options.ModuleName)

	// Slip clock and reset definitions into place
	m.Inputs["clk"] = Variable{1, "", "", ""}
	if m.Options.AddAsyncReset {
		m.Inputs["rst"] = Variable{1, "", "", ""}
	}

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
	//for stateName, _ := range m.States {} // TODO
	write(file, ");\n")
	// State machine states
	//if _, ok := m.Options.FirstState in  // verify FirstState is valid
	write(file, m.indent(1), "signal state_reg, state_next : state := ", m.Options.FirstState, ";\n")

	// architecture "begin"
	write(file, "begin\n")

	// Register process
	write(file, m.indent(1), "process(clk, rst)\n")
	write(file, m.indent(1), "begin\n")
	write(file, m.indent(2), "if (rst='1') then\n")
	write(file, m.indent(3), "state_reg <= ", m.Options.FirstState, ";\n")
	if m.Options.ClockType == "posedge" {
		write(file, m.indent(2), "elsif (clk'event and clk='1') then\n")
	} else if m.Options.ClockType == "negedge" {
		write(file, m.indent(2), "elsif (clk'event and clk='0') then\n")
	} else {
		return errors.New("Unrecognized clock type: " + m.Options.ClockType)
	}
	write(file, m.indent(3), "state_reg <= state_next;\n")
	write(file, m.indent(2), "end if;\n")
	write(file, m.indent(1), "end process;\n")
	// Next State process
	// Mealy(?) Output process
	// architecture end
	write(file, "end Behavioral;\n")
	write(file, "\n")

	return nil
}
