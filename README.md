# asmd-gen

Takes in an ASMD (hardware RTL state machine design) description and outputs Verilog or VHDL. Like QFSM, only simpler.

To be honest, though, it'll probably be more complex, not simpler. Time will tell.

# ASMD

ASMD == Algorithmic State Machines with Datapath

When I write an algorithm, I usually think in terms of C or higher.

Functional programming is hard to wrap my head around. Designing things for functional programming requires a different approach.

Real hardware is yet another mind-bendingly different way to design an algorithm. One approach to going from a normal algorithm (or, in my case, thoughts like code) to hardware is to implement it using state machines that translate easily to hardware.

One such method is explained in chapter 11 of

`RTL HARDWARE DESIGN USING VHDL Coding for Efficiency, Portability, and Scalability`
`PONG P. CHU`
`Cleveland State University`

Basically, you draw out a finite state machine with special semantics. The special semantics yield a state machine and a datapath that interact with each other. The algorithm is fairly easy to draw up, then the drawing is fairly easy to code up in hardware. (This may not yield the MOST efficient hardware ever, but that's OK.)

# ASMDGen

I'm tired of drawing out diagrams by hand and doing a large amount of boilerplate work to convert it into code. ASMDGen is intended to take a simple (ha!) text description of an ASMD and generate boilerplate VHDL or Verilog.

I'm aware that [QFSM](http://qfsm.sourceforge.net/) has similar capabilities. I haven't given it a go yet, but I doubt I can tweak it to my liking.

[Ragel](https://www.colm.net/open-source/ragel/) is another similar program; one option for this project is to simply fork and modify it to do hardware state diagrams and HDL outputs. Again, I haven't given it a go, but it doesn't look like it knows what an ASMD is.

# Inspiration and Assistance

[GNUU: Writing Your Own Toy Compiler](http://gnuu.org/2009/09/18/writing-your-own-toy-compiler)

`RTL HARDWARE DESIGN USING VHDL Coding for Efficiency, Portability, and Scalability` by `PONG P. CHU` is readily available through the IEEE member library. Most universities, public libraries, and technically minded corporations have subscriptions.
