;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (c) 2015, 2016 Katherine Hudek
;;
;; NOTE: This software was created during my High School years, beginning in
;; my sophomore year (10th grade). 
;; It is released as follows using the MIT license.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; "Hello World" program
;;
;; This "Hello World" program shows a
;; very simple example of Quacee's use. 
;;
;; For this example, we will create and then simulate a simple
;; circuit with two qubits that when measured result in
;; either |00> or |11> with equal probability.
;; If you are not already familiar with this notation,
;; or with quantum gates and circuits, you may wish to
;; read the paper on Quacee associated with this release.
;;
;; The general flow is to create and initialize a new circuit
;; (with two qubits), add some gates, and then execute it.
;; In this example, we start with two qubits initialized
;; to |0>, then put the first qubit into equal superposition
;; by applying a Hadamard gate, then take a measurement
;; of it which will cause it to collapse into either
;; |0> or |1> with equal probability, then use it as the
;; controller of a CNOT gate with the second qubit as
;; the target, then take a measurement of the second
;; qubit. At the end, depending on how the first qubit
;; collapsed from its superposition, both qubits would
;; have evolved to either both be |0> or both be |1>
;; (so state would be |00> or |11>).
;; In this example, after the circuit is constructed,
;; we print out an ASCII representation of the circuit,
;; and then hand it off to the simulator for execution.
;;
;;
;; returns t if successful or nil if failed
;;

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



