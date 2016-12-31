Copyright (c) 2015, 2016 Katherine Hudek

NOTE: This software was created during my High School years, beginning in
my sophomore year (10th grade). 
It is released as follows using the MIT license.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
=========================================================================================



README CONTENTS:
  * WHAT IS QUACEE?
  * DEPENDENCIES
  * KEY FILES
  * CONFIGURATION
  * PORTING
  * EXAMPLE USE
  * LIST OF KEY QUACEE FUNCTIONS
  * SIMULATOR RESTRICTIONS



===============
WHAT IS QUACEE?
===============
Quacee (QUAntum Computing Elucidation Extension)

Quacee is a quantum programming language designed to specify the desired
operations of (future) universal quantum computers. 

It is an extension to the Common LISP language and provides
a wide range of functions operating at the gate, qubit, 
and circuit levels. It allows one to specify a quantum
circuit which describes the desired operations of
a universal quantum computer. Since we do not yet have
universal quantum computers, Quacee also provides a 
basic simulation capability.

In a typical case, one would
  -- create a new empty quantum circuit
  -- define the desired quantum computation by
     -- adding quantum gates at various stages
     -- possibly adding more qubits
     -- possibly combining previously saved circuits
  -- when the quantum circuit is complete, one could
     -- print a representation of it
     -- graph a representation of it
     -- hand it off to Quacee's simulator

More information on Quacee can be found in the research paper
associated with this release, and on the KatieScienceAndArts
youtube channel, 
https://www.youtube.com/user/KatieScienceAndArts



============
DEPENDENCIES
============
Quacee depends on a COMMON LISP environment.
One loads in the Quacee code and then can interwork regular LISP with Quacee functions.
Quacee was developed and tested using Clozure CL running on Mac OSX.
Everything except the graph output (see porting note below) should work as is
within a Common Lisp environment.

If you desire the graph output capability, the open source Graphviz
software must be installed on your system and the exact mechanism to
invoke it from within Lisp may need porting (see porting note below).




=========
KEY FILES
=========
The core files of Quacee are as follows:
	quacee.lisp
	quacee_defs.lisp
	gate_defs.lisp
	circ_defs.lisp
	quacee_util.lisp
The basic simulator code is provided in:
	quacee_simbasic.lisp

Put these files in a directory from which they can
be loaded into the LISP code you write.
If you load quacee.lisp it will load the others.	
	


	
=============	
CONFIGURATION
=============
Quacee uses the following flags specified in quacee-defs.lisp.
Feel free to set these to t or nil to modify the behavior as desired.

	*sim_basic_loaded* - if true, qc_exec_circ will attempt to hand off the
	                     specified quantum circuit to the qc_simulate_basic 
	                     function 
	
	*qc_sim_qubitstate_print* - if true, the simulator will print out the state
	                            of the qubits as they evolve through the
	                            simulated execution of the circuit. This can be
	                            an interesting trace.
							
    *qc_sim_qubitjointprob_print* - if true, the simulator will print out the
                                    joint probability distribution of the
                                    qubits as they evolve through the
                                    simulated execution of the circuit. This can be
                                    an interesting trace.
                                     


=======  							    
PORTING
=======
Quacee was developed on an Apple laptop running Clozure Common LISP on OSX;
all functionality should work in any Common LISP environment with the exception 
of the graphing capability, which may require some porting effort.

The qc_output_circ_graph function depends on the open source GraphViz
software and the Quacee code to invoke it may need to be ported to your
particular environment.

After successfully installing GraphViz and making any necessary changes to the
qc_output_circ_graph code to invoke dot on the graph description file, 
set the *graphviz_invocation_is_ported* flag to true.


	*graphviz_invocation_is_ported* - if true, Quacee will attempt to create a graph 
                                      of the circuit in png format using
                                      the GraphViz program. This should only be set to
                                      true if GraphViz has been installed and the 
                                      mechanism for invoking it in qc_output_circ_graph 
                                      has been properly adapted to the user's environment.
                                      The flag is by default set to nil. 





===========
EXAMPLE USE					    
===========
The "Hello World" program included in the Quacee release shows a
very simple example of Quacee's use. 

For this example, we will create and then simulate the execution 
of a simple circuit with two qubits that when measured result 
in either |00> or |11> with equal probability.
If you are not already familiar with this notation,
or with quantum gates and circuits, you may wish to
read the paper on Quacee associated with this release.

The general flow is to create and initialize a new quantum circuit
(with two qubits), add some quantum gates, and then execute it.


In this example, we start with two qubits initialized
to |0>, then put the first qubit into equal superposition
by applying a Hadamard gate, then take a measurement
of it which will cause it to collapse into either
|0> or |1> with equal probability, then use it as the
controller of a CNOT gate with the second qubit as
the target, then take a measurement of the second
qubit. At the end, depending on how the first qubit
collapsed from its superposition, the qubits would
have evolved to either both be |0> or both be |1>
(so state would be |00> or |11>).
In this example, after the circuit is constructed,
we print out an ASCII representation of the circuit,
and then hand it off to the simulator for execution.

The circuit should look something like this...

CIRCUIT HelloWorld:

Qubit_0000  |0> ---| HADAMARD_GATE |------|  MEASURE_GATE |------|1Ctrl-Tl  (1)  |------|   NO_OP_GATE  |---
Qubit_0001  |0> ---|   NO_OP_GATE  |------|   NO_OP_GATE  |------|TARG- CTRL1_NOT|------|  MEASURE_GATE |---

And the simulation results should look something like this...

==================================================
BEGIN SIMULATION
Simulating quantum computer operations
on the given quantum circuit HelloWorld....
--------------------------------------------------
MEASUREMENTS at Stage 1
 Qubit 0 = 0
MEASUREMENTS at Stage 3
 Qubit 1 = 0
END SIMULATION
==================================================

or this (depending on which way the top qubit collapsed out of superposition
when measured)...

==================================================
BEGIN SIMULATION
Simulating quantum computer operations
on the given quantum circuit HelloWorld....
--------------------------------------------------
MEASUREMENTS at Stage 1
 Qubit 0 = 1
MEASUREMENTS at Stage 3
 Qubit 1 = 1
END SIMULATION
==================================================

Here is the basic code, which is also available in the HelloWorld.lisp file
included in this release...

----------------------------------------------------------------------------
(load "quacee")

(defun hello_world_equiv ()
  (let* 
      ((new_circ (qc_create_circ "HelloWorld" :est_qubits 2 :est_stages 4))) 
    (qc_apply_hadamard 0 0 new_circ) 
    (qc_apply_meas 0 1 new_circ) 
    (qc_apply_cnot 0 1 2 new_circ) 
    (qc_apply_meas 1 3 new_circ) 
    (pretty_print_circ new_circ)
    (qc_exec_circ new_circ)))
----------------------------------------------------------------------------


And here it is heavily commented as it appears in the
HelloWorld.lisp file.
----------------------------------------------------------------------------
(load "quacee") ; first load Quacee

(defun hello_world_equiv ()
  (let* 
      ;; initialize a new circuit called "HelloWorld" with 2 qubits and 4 stages.
      ;; the qubits are initialized to |0> by default
      ((new_circ (qc_create_circ "HelloWorld" :est_qubits 2 :est_stages 4))) 
    ;; the default qubit values of |0> are fine, don't need to change them.  
    ;; begin adding quantum gates to the circuit.
    ;; NOTE - qubits and circuit stages start counting at 0, not 1
    ;;
    ;; apply a hadamard gate to qubit0 at stage 0 
    ;; and a measurement gate to qubit0 at stage 1
    (qc_apply_hadamard 0 0 new_circ) 
    (qc_apply_meas 0 1 new_circ) 
    ;; apply a cnot gate with controller qubit0 and target qubit1 at stage 2
    (qc_apply_cnot 0 1 2 new_circ) 
    ;; apply a measurement gate to qubit1 at stage 3
    (qc_apply_meas 1 3 new_circ)
    ;; quantum circuit is now fully specified.
    ;; print out a representation of the circuit 
    (pretty_print_circ new_circ)
    ;; execute the circuit (run the simulator on it)
    (qc_exec_circ new_circ)))
----------------------------------------------------------------------------



============================
LIST OF KEY QUACEE FUNCTIONS    							    
============================
;;
;; Quacee Language
;;
;; The following function is used to create a new circuit
;; ready for the user to populate
;;    - qc_create_circ
;;
;; The following functions are used to specify inputs to
;; the circuit. Several options are available for creating an
;; initial nx1 array of input states 
;;    - qc_init_qubits_by_list
;;    - qc_init_qubits_0first
;;    - qc_init_qubits_1first
;;
;; The following function is used to initialize a circuit
;;    - qc_init_circ
;;
;; The following function is used to populate the circuit with
;; a set of inputs specified by a nx1 array
;;    - qc_add_qubits
;;
;; The following function is used to add another qubit to an
;; existing circuit
;;    - qc_add_qubit
;;
;; The following functions are used to apply gates to specified
;; qubits in a circuit at specified stages.
;; Note that some gates or gate inputs are not valid
;; after a qubit has been measured. The functions check
;; to ensure a gate placement is valid before adding it
;; to the circuit. The naming convention is
;; qc_apply_<gate-type> 
;;    - qc_apply_hadamard
;;    - qc_apply_mult_hadamards
;;    - qc_apply_pauliX
;;    - qc_apply_pauliY
;;    - qc_apply_pauliZ
;;    - qc_apply_phase_shift_pi/2
;;    - qc_apply_meas
;;    - qc_apply_swap
;;    - qc_apply_cnot
;;    - qc_apply_zcnot
;;    - qc_apply_toffoli
;;    - qc_apply_fredkin
;;    - qc_apply_no_op
;;    - qc_apply_phshift_gate
;;    - qc_apply_cphshift_gate
;;
;; The following functions allow one to create a new custom gate type
;; and apply it to specified inputs at a specified stage of the circuit. 
;; The gate's function is described by a matrix
;; which is validated to ensure that it is Unitary. 
;; Both normal and controlled custom gates are supported.
;;    - qc_apply_customgate
;;    - qc_apply_ctrld_customgate
;;
;;
;; The following function allows one to obtain a copy of the given gate.
;;    - qc_copy_gate
;;
;; The following functions allow one to print out the
;; circuit or interesting subsets of it, in a human-friendly manner
;;    - pretty_print_by_qubits
;;    - pretty_print_qubit_path
;;    - pretty_print_stage
;;    - pretty_print_by_stages
;;    - pretty_print_circ
;;
;; The following function allows one to print out the circuit
;;    - print_circ
;;
;; The following function allows one to output a graph of the circuit
;; This function assumes that GraphViz has been installed and is available
;; on your system, and its invocation may require some 
;; porting to your environment
;;    - qc_output_circ_graph
;;
;;
;; The following function allows one to add one circuit to another
;;    - qc_add_circ_to_circ
;;
;;
;; The following function allows one to save their circuit to a file
;;    - qc_save_circ_to_file
;;
;; The following function allows one to read their saved circuit from a file
;;    - qc_read_from_file
;;
;; The following funciton allows one to validate a given circuit
;;    - qc_validate_circ
;;
;; The following function simulates passing the quantum circuit to an
;; attached quantum computer
;;    - qc_exec_circ
;;

    
    

======================================
SIMULATOR RESTRICTIONS AND WORKAROUNDS    							    
======================================
The basic simulator included in this release of Quacee makes some assumptions
that place restrictions on the circuits that can be simulated. It is assumed that:
  - the transition matrix associated with each quantum gate structure can 
  	be used as-is when computing the tensor products for the state evolution 
  	at each stage
     - this implies that all gate inputs are consecutive/adjacent qubits, and
       that control qubits are above the target qubits in the circuit
       (lower circuit row number, but higher order bit)
  - only 1controllers are currently supported (not 0controller gates which
       have their effect if the controller is 0)
       
The user may work around the assumed qubit ordering with the use of
additional swap gates before and after a problematic gate.

For instance, if a CNOT gate is desired to be used to have
a qubit in row N control a qubit in row N-1, the simulator could
not handle it directly. But if the gate were placed into the circuit
such that it satisfies the restrictions (e.g., controller is qubit
on row N-1 and target is on row N), and then swap gates are placed before
the CNOT to exchange the qubit positions and then another swap gate
was placed after the CNOT to restore the original positions, then
the effect would be as originally desired and the simulator could
be used. More complex arrangements with larger gaps between inputs 
would require the use of more swap gates before and after.

 





    							    