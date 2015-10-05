package asmd

import (
	"encoding/json"
	"errors"
	"io/ioutil"
	"os"
	"strings"
)

type StateMachine struct {
	Options struct {
		ModuleName        string
		Author            string
		trimmedModuleName string // valid C identifier form of ModuleName
		ClockType         string // posedge, negedge
		FirstState        string // must be in States
		AddAsyncReset     *bool  // default true
		Indent            string // default four spaces
	}
	Parameters      map[string]Variable
	Inputs          map[string]Variable
	Outputs         map[string]Variable
	Registers       map[string]Variable
	FunctionalUnits map[string]struct {
		Inputs    map[string]Variable
		Outputs   map[string]Variable
		Registers map[string]Variable
	}
	States map[string]struct {
		Operations map[string]string // Valid HDL expressions to assign to register (map key); note that r <= r+a is rendered as r_next <= r+a
		Next       string            // must match State or Condition
		IsMealy    bool              // embeds logic in previous state instead of creating new state
	}
	Conditions map[string]struct {
		Expression  string
		TrueTarget  string
		FalseTarget string
	}
}

type Variable struct {
	BitWidth     uint64 // >1 invokes simple HDL array types
	Type         string // natural, std_logic_vector, etc. Default: std_logic
	DefaultValue string // TODO default to zero value or no default? Depend on context, perhaps?
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

	err = mac.Validate()
	if err != nil {
		return nil, err
	}

	mac.FixUpWithDefaults()

	return mac, nil
}

func validateInput(name string, input Variable) (err error) {
	if name == "" {
		return errors.New("Input name cannot be the empty string.")
	}
	if input.DefaultValue != "" {
		return errors.New(name + ": Input DefaultValue is nonsensical.")
	}
	return nil
}

func validateOutput(name string, input Variable) (err error) {
	if name == "" {
		return errors.New("Output name cannot be the empty string.")
	}
	if input.DefaultValue == "" {
		return errors.New(name + ": Outputs must have a DefaultValue defined.")
	}
	return nil
}

func validateRegister(name string, register Variable) (err error) {
	if name == "" {
		return errors.New("Register name cannot be the empty string.")
	}
	if register.DefaultValue == "" {
		return errors.New(name + ": Registers must have a DefaultValue defined.")
	}
	return nil
}

func (m *StateMachine) Validate() error {
	// Options
	if m.Options.ModuleName == "" {
		return errors.New("No module name specified (m.Options.Modulename).")
	}

	clockType := strings.ToLower(m.Options.ClockType)
	if !(clockType == "posedge" || clockType == "negedge") {
		return errors.New("m.Options.ClockType must be 'negedge' or 'posedge', not " + m.Options.ClockType)
	}

	if m.Options.FirstState == "" {
		return errors.New("m.Options.FirstState not specified.")
	}
	if _, ok := m.States[m.Options.FirstState]; !ok {
		return errors.New("m.Options.FirstState, " + m.Options.FirstState + ", is not in m.States.")
	}

	// Parameters
	for key, val := range m.Parameters {
		if key == "" {
			return errors.New("Parameter name cannot be the empty string.")
		}
		if val.DefaultValue == "" {
			return errors.New(key + ": Parameters must have a DefaultValue.")
		}
		if val.BitWidth != 0 {
			// TODO reconsider this, perhaps
			return errors.New("Parameter BitWidth is ignored.")
		}
		if strings.ToLower(val.Type) != "natural" {
			// TODO remove this restriction and allow arbitrary types
		}
	}

	// Inputs
	if len(m.Inputs) == 0 {
		//return errors.New("No inputs specified.")
	}
	for key, val := range m.Inputs {
		if err := validateInput(key, val); err != nil {
			return err
		}
	}

	// Outputs
	if len(m.Outputs) == 0 {
		//return errors.New("No Outputs specified.")
	}
	for key, val := range m.Outputs {
		if err := validateOutput(key, val); err != nil {
			return nil
		}
	}

	// Registers
	for name, register := range m.Registers {
		if err := validateRegister(name, register); err != nil {
			return err
		}
	}

	// Functional Units
	for key, val := range m.FunctionalUnits {
		if key == "" {
			return errors.New("FunctionalUnit name cannot be the empty string.")
		}
		if len(val.Inputs) == 0 && len(val.Outputs) == 0 {
			return errors.New("FunctionalUnit " + key + ": Really, a functional unit with no inputs or outputs?")
		}
		// Functional units are inverted from the ASMD's perspective
		for a, b := range val.Inputs {
			if err := validateOutput(a, b); err != nil {
				return err
			}
		}
		for a, b := range val.Outputs {
			if err := validateInput(a, b); err != nil {
				return err
			}
		}
		for a, b := range val.Registers {
			if err := validateRegister(a, b); err != nil {
				return err
			}
		}
	}

	// collect all state and condition names to use when validating graph edges
	stateNames := make(map[string]*string)
	for name, _ := range m.States {
		if name == "" {
			return errors.New("State names must not be the empty string.")
		}
		stateNames[name] = nil
	}
	for name, _ := range m.Conditions {
		if name == "" {
			return errors.New("Condition names must not be the empty string.")
		}
		if _, ok := stateNames[name]; ok {
			return errors.New("Condition " + name + " conflicts with a State name.")
		}
		stateNames[name] = nil
	}

	// collect all register and output names to use when validating RTL operations
	varSources := map[string]map[string]Variable{
		"Output":   m.Outputs,
		"Input":    m.Inputs,
		"Register": m.Registers,
	}
	for fuName, funcUnit := range m.FunctionalUnits {
		varSources["Functional Unit "+fuName+" Input"] = funcUnit.Inputs
		varSources["Functional Unit "+fuName+" Output"] = funcUnit.Outputs
		varSources["Functional Unit "+fuName+" Register"] = funcUnit.Registers
	}
	varNames := make(map[string]string)
	// TODO add inferred extra signals to varNames
	for sourceName, varSource := range varSources {
		for varName, _ := range varSource {
			if other, ok := varNames[varName]; ok {
				return errors.New(varName + " from " + sourceName + " conflicts with '" + other + "'.")
			}
			varNames[varName] = sourceName + " " + varName
		}
	}

	// States
	if len(m.States) == 0 {
		return errors.New("A state machine with no states is not a state machine.")
	}
	for name, state := range m.States {
		// Ensure State.Next is valid
		if _, ok := stateNames[state.Next]; !ok {
			return errors.New("State " + name + " specifies a Next target, " + state.Next + ", which is not a State or Condition.")
		}
		for regName, oper := range state.Operations {
			if regName == "" {
				return errors.New("State " + name + " specifies a register for an operation using an empty string.")
			}
			if oper == "" {
				return errors.New("State " + name + " specifies an empty string operation on register " + regName + ".")
			}
			if _, ok := varNames[regName]; !ok {
				return errors.New("State " + name + " refers to unknown signal " + regName)
			}
			// TODO ensure op targets are not inputs registers
			// TOOD parse Operations to make sure inputs are valid
		}
	}

	// Conditions
	for condName, condition := range m.Conditions {
		if condName == "" {
			return errors.New("Conditions cannot be named the empty string.")
		}
		if condition.Expression == "" {
			// TODO reconsider this in light of skeleton code generation
			return errors.New("Condition " + condName + " cannot have a blank Expression")
		}
		if _, ok := stateNames[condition.TrueTarget]; !ok {
			return errors.New("Condition " + condName + "'s TrueTarget targets unknown state " + condition.TrueTarget)
		}
		if _, ok := stateNames[condition.FalseTarget]; !ok {
			return errors.New("Condition " + condName + "'s FalseTarget targets unknown state " + condition.FalseTarget)
		}
	}

	// State graph is made up of DAGs
	for stateName, state := range m.States {
		errStr := validateStateSubgraph(state.Next, map[string]bool{}, m)
		if errStr != "" {
			return errors.New("While validating DAG after " + stateName + ": " + errStr)
		}
	}

	return nil
}

func validateStateSubgraph(nodeName string, seenNodes map[string]bool, m *StateMachine) string {
	// If we've seen the node name before, we've found a cycle.
	if _, ok := seenNodes[nodeName]; ok {
		return "Graph cycles at " + nodeName
	}

	// If it's a proper state, we're done.
	if state, ok := m.States[nodeName]; ok && !state.IsMealy {
		return ""
	}

	// Otherwise, make a note and recurse.
	seenNodes[nodeName] = true

	// we've landed on a Mealy state
	if state, ok := m.States[nodeName]; ok && state.IsMealy {
		return validateStateSubgraph(state.Next, seenNodes, m)
	}

	// we've landed on a Condition
	if cond, ok := m.Conditions[nodeName]; ok {
		val := validateStateSubgraph(cond.TrueTarget, seenNodes, m)
		if val != "" {
			return val
		}
		val = validateStateSubgraph(cond.FalseTarget, seenNodes, m)
		if val != "" {
			return val
		}
		return ""
	}

	return "Node " + nodeName + " was neither a State nor a Condition."
}

func (m *StateMachine) FixUpWithDefaults() {
	// m.Options

	// fix up module name to be a valid VHDL module name
	replacer := strings.NewReplacer(" ", "", "\t", "", "-", "")
	m.Options.trimmedModuleName = replacer.Replace(m.Options.ModuleName)

	// Set AddAsyncReset default if needed
	if m.Options.AddAsyncReset == nil {
		m.Options.AddAsyncReset = new(bool)
		*m.Options.AddAsyncReset = true
	}

	if m.Options.Indent == "" {
		m.Options.Indent = "    "
	}

	// m.Inputs
}

// TODO make this durned thing not throw exceptions, or catch them locally
func write(f *os.File, ss ...string) {
	for _, s := range ss {
		n, err := f.WriteString(s)
		if err != nil {
			panic(err)
		}
		if n != len(s) {
			panic(errors.New("Unable to write full string to file"))
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
