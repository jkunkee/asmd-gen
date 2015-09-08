package asmd

import (
	"errors"
	"os"
	"strconv"
	"time"
)

func (v Variable) ToStdLogic(name string) string {
	str := name + " : std_logic"

	if v.BitWidth > 1 {
		str += "_vector (" + strconv.FormatUint(v.BitWidth-1, 10) + " downto 0)"
	}

	return str
}

func (v Variable) ToStdLogicSignal(name string) string {
	str := "signal " + v.ToStdLogic(name)
	if v.DefaultValue != "" {
		str += " := " + v.DefaultValue
	}
	str += ";"
	return str
}

//func (v Variable) ToGeneric(name string) string {}

func (m *StateMachine) VHDL(filename string) (err error) {
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	// Comments
	write(file, "\n")
	write(file, "--------------------------------------------------------------------------------\n")
	write(file, "-- Module Name: ", m.Options.ModuleName, "\n")
	write(file, "-- Author:      ", m.Options.Author, "\n")
	write(file, "-- Date:        ", time.Now().Format("2 Jan 2006"), "\n")
	write(file, "--\n")
	write(file, "--------------------------------------------------------------------------------\n")
	write(file, "\n")
	write(file, "\n")

	// library and use statements
	// TODO infer the minimal set using given types
	write(file, "library IEEE;\n")
	write(file, "use IEEE.STD_LOGIC_1164.ALL;\n")
	write(file, "use IEEE.NUMERIC_STD.ALL;\n")
	write(file, "\n")

	// entity start
	write(file, "entity ", m.Options.trimmedModuleName, " is\n")

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
			write(file, properties.ToStdLogic(name))
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
			write(file, properties.ToStdLogic(name))
			write(file, "\n")
		}

		write(file, m.indent(1), ");\n")
	}

	// Entity end
	write(file, "end ", m.Options.trimmedModuleName, ";\n")
	write(file, "\n")

	// architecture start
	write(file, "architecture Behavioral of ", m.Options.trimmedModuleName, " is\n")

	// Constants (?)
	// Internal Signals
	write(file, m.indent(1), "-- Register signals\n")
	for sigName, signal := range m.Registers {
		write(file, m.indent(1), signal.ToStdLogicSignal(sigName+"_reg, "+sigName+"_next"), "\n")
	}
	write(file, "\n")

	// Internal signals for functional units
	for unitName, unit := range m.FunctionalUnits {
		write(file, m.indent(1), "-- ", unitName, " connections\n")
		for sigName, signal := range unit.Inputs {
			write(file, m.indent(1), signal.ToStdLogicSignal(sigName), "\n")
		}
		for sigName, signal := range unit.Outputs {
			write(file, m.indent(1), signal.ToStdLogicSignal(sigName), "\n")
		}
		for sigName, signal := range unit.Registers {
			write(file, m.indent(1), signal.ToStdLogicSignal(sigName+"_reg"), "\n")
			write(file, m.indent(1), signal.ToStdLogicSignal(sigName+"_reg, "+sigName+"_next"), "\n")
		}
	}
	if len(m.FunctionalUnits) > 0 {
		write(file, "\n")
	}

	write(file, m.indent(1), "-- FSM declarations\n")
	// State machine states
	write(file, m.indent(1), "type state is (")
	isFirst := true
	for stateName, _ := range m.States {
		if isFirst {
			isFirst = false
		} else {
			write(file, ", ")
		}
		write(file, stateName+"_s")
	}
	write(file, "\n")
	// State machine signals
	write(file, m.indent(1), "signal state_reg, state_next : state := ", m.Options.FirstState, ";\n")

	// architecture "begin"
	write(file, "begin\n")

	// Register process
	write(file, m.indent(1), "-- FSM state register\n")
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
