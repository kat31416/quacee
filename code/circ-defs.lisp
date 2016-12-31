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
;; circuit info
;;
;; Top level structure containing information about a given circuit.
;;
;;  num_qubits - number of qubits (rows) in the circ
;;  num_stages - number of stages (columns) in the circ
;;  id - id for this particular circuit
;;  circ - circuit, specifies locations of gates in the quantum network, 
;;         it is represented as an array of gates organized 
;;         with rows associated with qubits and columns associated with stages
;;  qubits_init_state - array of initial state of qubits
;;  qubits_measurement_stage - vector of stages at which qubits were measured
;;     after this point, they are restricted in what they can do
;; 
;;
(defstruct circ_info 
  (num_qubits 0)    ;; number of qubits (rows) in the circ
  (num_stages 0)    ;; number of stages (columns) in the circ
  (id nil)          ;; id for this particular circuit
  (circ nil)        ;; circuit, specifies locations of gates in the quantum network
  (qubits_init_state nil)        ;; array of initial state of qubits 
  (qubits_measurement_stage nil))  ;; vector of stages at which qubits were measured
                                
