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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gate definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quacee supports the following standard quantum gates:
;; Hadamard
;; Pauli-X 
;; Pauli-Y 
;; Pauli-Z
;; Phase-shift-pi
;; Phase-shift-pi/2
;; Phase-shift-pi/4
;; Phase-shift-pi/8
;; Phase-shift-pi/16
;; Phase-shift-pi/32
;; Measurement
;; Swap
;; Cnot
;; ZCnot - CNOT controlled by zero
;; Toffoli
;; Fredkin
;; No-op
;; In addition, Quacee supports the addition of custom-made gates.
 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  structure definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; gate
;; a structure to hold information about a gate
;;
;; type - a symbol to indicate the type of the gate
;; id - used to indicate the location of the gate within a circuit (qubit.stage)
;;      if a gate spans multiple qubits, the qubit porton of the id is the
;;      qubit associated with the lowest numbered row in the circuit array
;; numinputs - number of inputs
;; numoutputs - number of outputs
;; in_regular - list of input ids (qubit number)
;; in_1controllers - list of input ids (qubit number) which control if 1
;; in_0controllers - list of input ids (qubit number) which control if 0
;; trans_matrix - read-only transition matrix mapping inputs to outputs
;; pre_comment - comment placed before the gate on the graph output 
;;               (for annotation use)
;; post_comment - comment placed after the gate on the graph output 
;;                (for annotation use)
;;
(defstruct gate 
  (type nil)  ;symbol indicating type of gate
  (id '(most-positive-fixnum . most-positive-fixnum))  ;qubit . stage
  (numinputs 0)
  (numoutputs 0)
  (in_regular nil)  ; list of input ids (qubit number)
  (in_1controllers nil) ; list of input ids (qubit number) which control if 1
  (in_0controllers nil) ; list of input ids (qubit number) which control if 0
  (trans_matrix nil) ; transition matrix mapping inputs to outputs
  (pre_comment "") 
  (post_comment "")
  )


;;
;; gate_hadamard 
;;
;; a Hadamard gate
;;
(defstruct 
    (gate_hadamard
     (:include gate
               (type 'hadamard_gate)
               (numinputs 1)
               (numoutputs 1)
               (trans_matrix 
                (make-array '(2 2)                   
                            :initial-contents 
                            `((,(complex (/ 1 (sqrt 2)) 0) 
                                   ,(complex (/ 1 (sqrt 2)) 0))
                                  (,(complex (/ 1 (sqrt 2)) 0) 
                                   ,(complex (/ -1 (sqrt 2)) 0))))))))

;;
;; gate_pauliX 
;; a Pauli X gate
;;
(defstruct
    (gate_pauliX
     (:include gate
               (type 'X_gate)
               (numinputs 1)
               (numoutputs 1)
               (trans_matrix 
                (make-array '(2 2)                   
                            :initial-contents 
                            '((0 1)
                              (1 0))))))) 


;;
;; gate_pauliY 
;; a Pauli Y gate
;;
(defstruct
    (gate_pauliY
     (:include gate
               (type 'Y_gate)
               (numinputs 1)
               (numoutputs 1)
               (trans_matrix 
                (make-array '(2 2)                   
                            :initial-contents 
                            `((0 ,(complex 0 -1))
                              (,(complex 0 1) 0)))))))
                               


;;
;; gate_pauliZ 
;; a Pauli Z gate
;;
(defstruct
    (gate_pauliZ
     (:include gate
               (type 'Z_gate)
               (numinputs 1)
               (numoutputs 1)
               (trans_matrix 
                (make-array '(2 2)                   
                            :initial-contents 
                            '((1 0)
                              (0 -1)))))))

;;
;; gate_phase_shift_pi 
;; a pi phase shift
;;
(defstruct
    (gate_phase_shift_pi
     (:include gate
               (type 'ph_pi_gate)
               (numinputs 1)
               (numoutputs 1)
               (trans_matrix
                (make-array '(2 2)
                            :initial-contents
                            '((1 0)
                              (0 -1)))))))

;;
;; gate_cphase_shift_pi 
;; a controlled pi phase shift
;;
(defstruct
    (gate_cphase_shift_pi
     (:include gate
               (type 'cph_pi_gate)
               (numinputs 2)
               (numoutputs 2)
               (trans_matrix
                (make-array '(4 4)
                            :initial-contents
                            '((1 0 0 0)
                              (0 1 0 0)
                              (0 0 1 0)
                              (0 0 0 -1)))))))

                               
;;
;; gate_phase_shift_pi/2 
;; a pi/2 phase shift
;;
(defstruct
    (gate_phase_shift_pi/2
     (:include gate
               (type 'ph_pi/2_gate)
               (numinputs 1)
               (numoutputs 1)
               (trans_matrix 
                (make-array '(2 2)                   
                            :initial-contents 
                            `((1 0)
                              (0 ,(complex 0 1))))))))

;;
;; gate_cphase_shift_pi/2 
;; a controlled pi/2 phase shift
;;
(defstruct
    (gate_cphase_shift_pi/2 
     (:include gate
               (type 'cph_pi/2_gate)
               (numinputs 2)
               (numoutputs 2)
               (trans_matrix
                (make-array '(4 4)
                            :initial-contents
                            `((1 0 0 0)
                              (0 1 0 0)
                              (0 0 1 0)
                              (0 0 0 ,(complex 0 1))))))))

;;
;; gate_phase_shift_pi/4 
;; a pi/4 phase shift
;;
(defstruct
    (gate_phase_shift_pi/4
     (:include gate
               (type 'ph_pi/4_gate)
               (numinputs 1)
               (numoutputs 1)
               (trans_matrix
                (make-array '(2 2)
                            :initial-contents
                            `((1 0)
                              (0 ,(complex (cos (/ pi 4))
                                           (sin (/ pi 4))))))))))

;;
;; gate_cphase_shift_pi/4 
;; a controlled pi/4 phase shift
;;
(defstruct
    (gate_cphase_shift_pi/4
     (:include gate
               (type 'cph_pi/4_gate)
               (numinputs 2)
               (numoutputs 2)
               (trans_matrix
                (make-array '(4 4)
                            :initial-contents
                            `((1 0 0 0)
                              (0 1 0 0)
                              (0 0 1 0)
                              (0 0 0 ,(complex (cos (/ pi 4))
                                               (sin (/ pi 4))))))))))
;;
;; gate_phase_shift_pi/8 
;; a pi/8 phase shift
;;
(defstruct
    (gate_phase_shift_pi/8
     (:include gate
               (type 'ph_pi/8_gate)
               (numinputs 1)
               (numoutputs 1)
               (trans_matrix
                (make-array '(2 2)
                            :initial-contents
                            `((1 0)
                              (0 ,(complex (cos (/ pi 8))
                                           (sin (/ pi 8))))))))))

;;
;; gate_cphase_shift_pi/8 
;; a controlled pi/8 phase shift
;;
(defstruct
    (gate_cphase_shift_pi/8
     (:include gate
               (type 'cph_pi/8_gate)
               (numinputs 2)
               (numinputs 2)
               (trans_matrix
                (make-array '(4 4)
                            :initial-contents
                            `((1 0 0 0)
                              (0 1 0 0)
                              (0 0 1 0)
                              (0 0 0 ,(complex (cos (/ pi 8))
                                               (sin (/ pi 8))))))))))
;;
;; gate_phase_shift_pi/16 
;; a pi/16 phase shift
;;
(defstruct
    (gate_phase_shift_pi/16
     (:include gate
               (type 'ph_pi/16_gate)
               (numinputs 1)
               (numoutputs 1)
               (trans_matrix
                (make-array '(2 2)
                            :initial-contents
                            `((1 0)
                              (0 ,(complex (cos (/ pi 16))
                                           (sin (/ pi 16))))))))))

;;
;; gate_cphase_shift_pi/16 
;; a controlled pi/16 phase shift
;;
(defstruct
    (gate_cphase_shift_pi/16
     (:include gate
               (type 'cph_pi/16_gate)
               (numinputs 2)
               (numoutputs 2)
               (trans_matrix
                (make-array '(4 4)
                            :initial-contents
                            `((1 0 0 0)
                              (0 1 0 0)
                              (0 0 1 0)
                              (0 0 0 ,(complex (cos (/ pi 16))
                                               (sin (/ pi 16))))))))))


;;
;; gate_phase_shift_pi/32 
;; a pi/32 phase shift
;;
(defstruct
    (gate_phase_shift_pi/32
     (:include gate
               (type 'ph_pi/32_gate)
               (numinputs 1)
               (numoutputs 1)
               (trans_matrix
                (make-array '(2 2)
                            :initial-contents
                            `((1 0)
                              (0 ,(complex (cos (/ pi 32))
                                           (sin (/ pi 32))))))))))

;;
;; gate_cphase_shift_pi/32 
;; a controlled pi/32 phase shift
;;
(defstruct
    (gate_cphase_shift_pi/32
     (:include gate
               (type 'cph_pi/32_gate)
               (numinputs 2)
               (numoutputs 2)
               (trans_matrix
                (make-array '(4 4)
                            :initial-contents
                            `((1 0 0 0)
                              (0 1 0 0)
                              (0 0 1 0)
                              (0 0 0 ,(complex (cos (/ pi 32))
                                               (sin (/ pi 32))))))))))
                     
                                                 

;;
;; gate_meas 
;;
;; a gate that measures the qubit; once this happens, the qubit is 
;; now a classical bit, and cannot enter another gate unless it is a controller
;; or the gate is empty
;;
(defstruct
    (gate_meas
     (:include gate
               (type 'measure_gate)
               (numinputs 1)
               (numoutputs 1)
               (trans_matrix
                (make-array '(2 2)
                            :initial-contents
                            '((1 0)
                              (0 1)))))))


;;
;; gate_swap 
;; swaps the probabilities of the qubits
;;
(defstruct 
    (gate_swap
     (:include gate
               (type 'swap_gate)
               (numinputs 2)
               (numoutputs 2)
               (trans_matrix
                (make-array  '(4 4)
                             :initial-contents
                             '((1 0 0 0)
                               (0 0 1 0)
                               (0 1 0 0)
                               (0 0 0 1)))))))


;;
;; gate_cnot 
;; applies NOT only if the first input is 1
;;
(defstruct
    (gate_cnot
     (:include gate
               (type 'ctrl1_not)
               (numinputs 2)
               (numoutputs 2)
               (trans_matrix
                (make-array '(4 4)
                            :initial-contents
                            '((1 0 0 0)
                              (0 1 0 0)
                              (0 0 0 1)
                              (0 0 1 0)))))))

;;
;; gate_zcnot 
;; applies NOT only if the first input is 0
;;
(defstruct
    (gate_zcnot
     (:include gate
               (type 'ctrl0_not)
               (numinputs 2)
               (numoutputs 2)
               (trans_matrix
                (make-array '(4 4)
                            :initial-contents
                            '((0 1 0 0)
                              (1 0 0 0)
                              (0 0 1 0)
                              (0 0 0 1)))))))

;;
;; gate_toffoli 
;; controlled-controlled-not
;; 
(defstruct
    (gate_toffoli
     (:include gate
               (type 'toffoli_gate)
               (numinputs 3)
               (numoutputs 3)
               (trans_matrix
                (make-array '(8 8)
                            :initial-contents
                            '((1 0 0 0 0 0 0 0)
                              (0 1 0 0 0 0 0 0)
                              (0 0 1 0 0 0 0 0)
                              (0 0 0 1 0 0 0 0)
                              (0 0 0 0 1 0 0 0)
                              (0 0 0 0 0 1 0 0)
                              (0 0 0 0 0 0 0 1)
                              (0 0 0 0 0 0 1 0)))))))
                              
;;
;; gate_fredkin 
;; controlled swap
;;
(defstruct
    (gate_fredkin
     (:include gate
               (type 'fredkin_gate)
               (numinputs 3)
               (numoutputs 3)
               (trans_matrix
                (make-array '(8 8)
                            :initial-contents
                            '((1 0 0 0 0 0 0 0)
                              (0 1 0 0 0 0 0 0)
                              (0 0 1 0 0 0 0 0)
                              (0 0 0 1 0 0 0 0)
                              (0 0 0 0 1 0 0 0)
                              (0 0 0 0 0 0 1 0)
                              (0 0 0 0 0 1 0 0)
                              (0 0 0 0 0 0 0 1)))))))


;;
;; gate_no_op 
;; an empty gate, initialized to the identity matrix
;;
(defstruct (gate_no_op
            (:include gate
                      (type 'no_op_gate)
                      (numinputs 1)
                      (numoutputs 1)
                      (trans_matrix (make-array '(2 2) 
                                                :initial-contents '((1 0)
                                                                    (0 1)))))))

