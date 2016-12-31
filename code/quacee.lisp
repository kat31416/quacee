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

(load "gate-defs")
(load "circ-defs")
(load "quacee-defs")
(load "quacee-util")
(load "quacee-simbasic")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Circuit creation 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;;
;; qc_create_circ
;;
;; Create a new circuit
;; Initializes a new circ_info struct and returns it.
;; 
;; id - name or other identifier for this new circuit
;; est_qubits - optional, estimated number of qubits that will be used 
;;              in the circuit. Defaults to 1. Circuit will automatically grow
;;              as needed if more qubits are added later.
;; est_stages - optional, estimated number of stages that will be used
;;              in the circuit. Defaults to 1. Circuit will automatically grow
;;              as needed if more stages are added later.
;;
;; NOTE that the initialized struct will contain a circ_info-circ
;;   matrix initialized with no-op gates. The qubit inputs will be
;;   initialized to |0> (ket zero).
;; 
;; returns new circ_info
;;
(defun qc_create_circ (id &key (est_qubits 1) (est_stages 1))
  (let* ((cinfo (make-circ_info 
                :num_qubits est_qubits
                :num_stages est_stages
                :id id
                :circ (make-array `(,est_qubits ,est_stages) 
                                  :element-type t 
                                  :initial-element nil))))
    (qc_init_circ cinfo)
    cinfo))
    
;;
;; qc_init_circ
;; 
;; Initializes a new circuit with no-op gates and qubit inputs set
;; to |0> (ket zero)
;;
;; cinfo - the circ_info
;;
;; returns the circ_info, which has now been initialized
;;
(defun qc_init_circ (cinfo)
  (let* ((numrow (circ_info-num_qubits cinfo))
         (numcol (circ_info-num_stages cinfo))
         (circ (circ_info-circ cinfo)))
    ;; initialize the circ with no-op gates
    (dotimes (i numrow t)
      (dotimes (j numcol t)
        (setf (aref circ i j) 
              (make-gate_no_op 
               :id (cons i j)
               :in_regular (list i)))))

    ;; initialize the qubits to all have initial
    ;;     value of |0>
    ;; User can override as needed later
    (setf (circ_info-qubits_init_state cinfo)
          (make-array (list numrow numcol) :initial-element *ket_zero*))

    ;; initialize the qubits measured vector to
    ;; indicate no qubits measured yet
    (setf (circ_info-qubits_measurement_stage cinfo) 
          (make-array numrow :initial-element
                      most-positive-fixnum))
    cinfo))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Input specification 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                
;;
;; qc_init_qubits_by_list
;; initializes an Nx1 array of qubits with their
;; initial state.
;;
;; lo0 - list of qubits in intial state 0 
;; lo1 - list of qubits in initial state 1 
;; numqubits - number of qubits total
;; 
;; if a qubits slot is not specified in either list,
;; prints a warning and leaves it initialized to nil 
;;
;; returns the array 
;;
(defun qc_init_qubits_by_list (lo0 lo1 numqubits)
  (let* ((qubit_array (make-array (list numqubits 1) :initial-element nil)))
    (dotimes (i numqubits)  
      (cond 
       ((member i lo0) (setf (aref qubit_array i 0) *ket_zero*))
       ((member i lo1) (setf (aref qubit_array i 0) *ket_one*))
       (t (format t "WARNING: init_qubits_by_list slot ~a not specified~%" i))))
    qubit_array))
          


;;
;; qc_init_qubits_0first
;; initializes an Nx1 array of qubits with their initial state
;; Assumes that the qubits are grouped by state, with
;; num0 qubits together in the 0 state and num1 qubits in the 1 state.
;; Puts the 0 qubits in the first (lower) positions of the array
;; 
;; num0 - number of qubits in state 0
;; num1 - number of qubits in state 1
;;
;; returns the array
;;
(defun qc_init_qubits_0first (num0 num1)
  (let* ((numqubits (+ num0 num1))
         (qubit_array (make-array `(,numqubits 1) :initial-element nil)))
    (dotimes (i num0)
      (setf (aref qubit_array i 0) *ket_zero*))
    (loop for i from num0 below (+ num0 num1) do 
      (setf (aref qubit_array i 0) *ket_one*))
    qubit_array))


;;
;; qc_init_qubits_1first
;; initializes an Nx1 array of qubits with their initial state
;; Assumes that the qubits are grouped by state, with
;; num0 qubits together in the 0 state and num1 qubits in the 1 state.
;; Puts the 1 qubits in the first (lower) positions of the array
;;
;; num1 - number of qubits in state 1
;; num0 - number of qubits in state 0
;;
;; returns the array
;;
(defun qc_init_qubits_1first (num1 num0)
  (let* ((numqubits (+ num0 num1))
         (qubit_array (make-array `(,numqubits 1) :initial-element nil)))
    (dotimes (i num1)
      (setf (aref qubit_array i 0) *ket_one*))
    (loop for i from num1 below (+ num1 num0) do 
      (setf (aref qubit_array i 0) *ket_zero*))
    qubit_array))


;;
;; qc_add_qubits
;;
;; Associate the given input states with the given circuit
;;
;; cinfo - the given circuit
;; qu_init_states - the given input states
;;
;; returns nil if incompatible dimensions and t if compatible
;;
(defun qc_add_qubits (cinfo qu_init_states)
  (let* ((numrow (car (array-dimensions qu_init_states))))
    (cond
     ((not (= numrow (circ_info-num_qubits cinfo))) 
      (progn 
        (format t "WARNING: add_qubits: Incompatible dimensions ~a vs. ~a" 
                numrow (circ_info-num_qubits cinfo))
        nil))
     (t 
      (progn 
        (setf (circ_info-qubits_init_state cinfo) qu_init_states)
        (setf (circ_info-qubits_measurement_stage cinfo) 
              (make-array numrow :initial-element
                          most-positive-fixnum))
        t)))))



;;
;; qc_add_qubit
;;
;; Add a new qubit to the given circuit
;;
;; cinfo - the given circuit
;; qu_init_state - the given input state for the qubit: 
;;                 use 0 to indicate ket zero, 1 for ket one
;;
;; returns cinfo
;;
(defun qc_add_qubit (cinfo qu_init_state)
  (let* ((num_rows (circ_info-num_qubits cinfo))
         (num_cols (circ_info-num_stages cinfo))
         (needed_rows (+ num_rows 1)))
    (progn
      (setf (circ_info-circ cinfo)
            (adjust-array (circ_info-circ cinfo) (list needed_rows num_cols)))
      (setf (circ_info-num_qubits cinfo) needed_rows)
      (qc_add_no_ops (circ_info-circ cinfo) num_rows (1- needed_rows)
               0 (1- num_cols))
      (setf (circ_info-qubits_init_state cinfo)
            (if (= qu_init_state 0)
                (adjust-array (circ_info-qubits_init_state cinfo) 
                              (list needed_rows 1)
                              :initial-element *ket_zero*)
                (adjust-array (circ_info-qubits_init_state cinfo) 
                              (list needed_rows 1)
                              :initial-element *ket_one*)))
      (setf (circ_info-qubits_measurement_stage cinfo)
            (adjust-array (circ_info-qubits_measurement_stage cinfo) 
                          needed_rows
                          :initial-element most-positive-fixnum))
      cinfo)))
  
                   
              
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Add Gates To A Circuit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; qc_apply_hadamard
;; 
;; adds a hadamard gate to a circuit
;;
;; qubit - the qubit input
;; stage - the stage containing the Hadamard
;; cinfo - circuit
;;
;; returns the circ_info if successful, or nil otherwise
;;
(defun qc_apply_hadamard (qubit stage cinfo)
  (if (and
       (qc_adjust_circ_to_fit cinfo qubit stage)
       (qc_valid_gateinputs (list qubit) stage nil 
                            (circ_info-qubits_measurement_stage cinfo)))
      (let* ((new_gate (make-gate_hadamard 
                        :id (cons qubit stage)
                        :in_regular (list qubit))))
        (setf (aref (circ_info-circ cinfo) qubit stage) new_gate)
        cinfo)
      (progn 
        (format t "WARNING: add_hadamard qubit/stage  ~a/~a. Not permitted."
                qubit stage)
        nil)))


;; 
;; qc_apply_mult_hadamards   
;;
;; adds multiple hadamard gates to a circuit
;;
;; loq - list of qubits
;; stage - the stage at which to apply the Hadamards
;; cinfo - circuit
;;
;; returns t if successful, or nil otherwise
;;
(defun qc_apply_mult_hadamards (loq stage cinfo)
  (cond
   ((not loq) t)
   (t 
    (and (qc_apply_hadamard (car loq) stage cinfo)
         (qc_apply_mult_hadamards (cdr loq) stage cinfo)))))


;;
;; qc_apply_pauliX
;;
;; adds a pauli-X gate to a circuit
;;
;; qubit - qubit input
;; stage - stage at which to apply the pauli-X
;; cinfo - circuit
;;
;; returns the circ_info if successful, or nil if otherwise
;;
(defun qc_apply_pauliX (qubit stage cinfo)
  (if (and 
       (qc_adjust_circ_to_fit cinfo qubit stage)
       (qc_valid_gateinputs (list qubit) stage nil 
                            (circ_info-qubits_measurement_stage cinfo)))
      (let* ((new_gate (make-gate_pauliX 
                        :id (cons qubit stage)
                        :in_regular (list qubit))))
        (setf (aref (circ_info-circ cinfo) qubit stage) new_gate)
        cinfo)
      (progn 
        (format t "WARNING: add_pauliX qubit/stage  ~a/~a. Not permitted."
                qubit stage)
        nil)))


;;
;; qc_apply_pauliY
;;
;; adds a pauli-Y gate to a circuit
;;
;; qubit - qubit input
;; stage - stage at which to apply the pauli-Y
;; cinfo - circuit
;;
;; returns the circ_info if successful, or nil if otherwise
;;
(defun qc_apply_pauliY (qubit stage cinfo)
  (if (and
       (qc_adjust_circ_to_fit cinfo qubit stage)
       (qc_valid_gateinputs (list qubit) stage nil 
                            (circ_info-qubits_measurement_stage cinfo)))
      (let* ((new_gate (make-gate_pauliY 
                        :id (cons qubit stage)
                        :in_regular (list qubit))))
        (setf (aref (circ_info-circ cinfo) qubit stage) new_gate)
        cinfo)
      (progn 
        (format t "WARNING: add_pauliY qubit/stage  ~a/~a. Not permitted."
                qubit stage)
        nil)))


;;
;; qc_apply_pauliZ
;;
;; adds a pauli-Z gate to a circuit
;;
;; qubit - qubit input
;; stage - stage at which to apply the pauli-Z
;; cinfo - circuit
;;
;; returns the circ_info if successful, or nil if otherwise
;;
(defun qc_apply_pauliZ (qubit stage cinfo)
  (if (and
       (qc_adjust_circ_to_fit cinfo qubit stage)
       (qc_valid_gateinputs (list qubit) stage nil 
                            (circ_info-qubits_measurement_stage cinfo)))
      (let* ((new_gate (make-gate_pauliZ 
                        :id (cons qubit stage)
                        :in_regular (list qubit))))
        (setf (aref (circ_info-circ cinfo) qubit stage) new_gate)
        cinfo)
      (progn 
        (format t "WARNING: add_pauliZ qubit/stage  ~a/~a. Not permitted."
                qubit stage)
        nil)))


;;
;; qc_apply_phase_shift_pi/2
;;
;; adds a phase-shift pi/2 gate to a circuit
;;
;; qubit - qubit input
;; stage - stage at which to apply the phase-shift
;; cinfo - circuit
;;
;; returns the circ_info if successful, or nil otherwise
;;
(defun qc_apply_phase_shift_pi/2 (qubit stage cinfo)
  (if (and
       (qc_adjust_circ_to_fit cinfo qubit stage)
       (qc_valid_gateinputs (list qubit) stage nil 
                            (circ_info-qubits_measurement_stage cinfo)))
      (let* ((new_gate (make-gate_phase_shift_pi/2 
                        :id (cons qubit stage)
                        :in_regular (list qubit))))
        (setf (aref (circ_info-circ cinfo) qubit stage) new_gate)
        cinfo)
      (progn 
        (format t "WARNING: add_phase_shift_pi/2 qubit/stage  ~a/~a. Not permitted."
                qubit stage)
        nil)))


;;
;; qc_apply_meas
;;
;; creates and adds a measurement gate to the circ.
;; also updates the vector indicating where the given qubit was measured
;;
;; qubit - which qubit the gate affects
;; stage - which stage of the circ to add the gate
;; cinfo - circuit info struct for circuit being modified 
;; 
;; returns the circ_info
;;
(defun qc_apply_meas (qubit stage cinfo)
  (let* ((new_gate (make-gate_meas
                    :id (cons qubit stage)
                    :in_regular (list qubit))))
    (qc_adjust_circ_to_fit cinfo qubit stage)
    (setf (aref (circ_info-circ cinfo) qubit stage) new_gate)
    (setf (svref (circ_info-qubits_measurement_stage cinfo) qubit) stage)
    cinfo))




;;
;; qc_apply_swap 
;;
;; adds a swap gate to a circuit
;;
;; loq - list of qubits
;; stage - stage at which to add the gate
;; cinfo - the circuit
;;
;; returns the circ_info if successful, or nil if otherwise
;;
(defun qc_apply_swap (loq stage cinfo)
  (if (and 
       (qc_adjust_circ_to_fit cinfo (reduce #'max loq) stage)
       (qc_valid_gateinputs loq stage nil 
                            (circ_info-qubits_measurement_stage cinfo)))
      (let* ((new_gate (make-gate_swap 
                        :id (cons (min_of_list loq) stage)
                        :in_regular loq))
             (circ (circ_info-circ cinfo)))
        (setf (aref circ (car loq) stage) new_gate)
        (setf (aref circ (cadr loq) stage) new_gate)
        cinfo)
      (progn 
        (format t "WARNING: add_swap qubits/stage  ~a/~a. Not permitted."
                loq stage)
        nil)))


;;
;; qc_apply_cnot
;; 
;; add a controlled-not gate
;; NOTE-this cnot is controlled by a 1
;;
;; ctrl - controlling qubit
;; target - target qubit
;; stage - stage at which to apply the gate
;; cinfo - the circuit
;;
;; returns the circ_info if successful, or nil if otherwise
;;
(defun qc_apply_cnot (ctrl target stage cinfo)
  (if (and 
       (qc_adjust_circ_to_fit cinfo (reduce #'max (list ctrl target)) stage)
       (qc_valid_gateinputs (list ctrl target) stage 
                            (list ctrl) 
                            (circ_info-qubits_measurement_stage cinfo)))
      (let* ((new_gate (make-gate_cnot
                        :id (cons (min_of_list (list ctrl target)) stage)
                        :in_regular (list target)
                        :in_1controllers (list ctrl)))
             (circ (circ_info-circ cinfo)))
        (setf (aref circ ctrl stage) new_gate)
        (setf (aref circ target stage) new_gate)
        cinfo)
      (progn 
        (format t "WARNING: add_cnotgate ctrl/target/stage ~a/~a/~a. Not permitted."
                ctrl target stage)
        nil)))



;;
;; qc_apply_zcnot
;;
;; ctrl - controlling qubit
;; target - target qubit
;; stage - stage at which to apply the gate
;; cinfo - the circuit
;;
;; returns the circ_info if successful, or nil if otherwise
;;
(defun qc_apply_zcnot (ctrl target stage cinfo)
  (if (and 
       (qc_adjust_circ_to_fit cinfo (reduce #'max (list ctrl target)) stage)
       (qc_valid_gateinputs (list ctrl target) stage 
                            (list ctrl) 
                            (circ_info-qubits_measurement_stage cinfo)))
      (let* ((new_gate (make-gate_cnot
                        :id (cons (min_of_list (list ctrl target)) stage)
                        :in_regular (list target)
                        :in_0controllers (list ctrl)))
             (circ (circ_info-circ cinfo)))
        (setf (aref circ ctrl stage) new_gate)
        (setf (aref circ target stage) new_gate)
        cinfo)
      (progn 
        (format t "WARNING: add_zcnotgate ctrl/target/stage ~a/~a/~a. Not permitted."
                ctrl target stage)
        nil)))


;;
;; qc_apply_toffoli
;;
;; add a toffoli gate
;; NOTE-this controlled-controlled-not is controlled by two 1s
;;
;; loctrls - list of controlling qubits
;; target - target qubit
;; stage - stage at which to apply the gate
;; cinfo - the circuit
;;
;; returns the circ_info if successful, or nil if otherwise
;;
(defun qc_apply_toffoli (loctrls target stage cinfo)
  (if (and 
       (qc_adjust_circ_to_fit cinfo (reduce #'max (cons target loctrls)) stage)
       (qc_valid_gateinputs (cons target loctrls) stage 
                            loctrls (circ_info-qubits_measurement_stage cinfo)))
      (let* ((new_gate (make-gate_toffoli
                        :id (cons (min_of_list (cons target loctrls)) stage)
                        :in_regular (list target)
                        :in_1controllers loctrls))
             (circ (circ_info-circ cinfo)))
        (setf (aref circ (car loctrls) stage) new_gate)
        (setf (aref circ (cadr loctrls) stage) new_gate)
        (setf (aref circ target stage) new_gate)
        cinfo)
      (progn 
        (format t "WARNING: add_toffoli loctrls/target/stage ~a/~a/~a. Not permitted."
                loctrls target stage)
        nil)))



;;
;; qc_apply_fredkin
;;
;; add a fredkin gate
;; NOTE-this controlled-swap is controlled by a 1
;;
;; ctrl - controlling qubit
;; lot - list of target qubits
;; stage - stage at which to apply the gate
;; cinfo - the circuit
;;
;; returns the circ_info if successful, or nil if otherwise
;;
(defun qc_apply_fredkin (ctrl lot stage cinfo)
  (if (and 
       (qc_adjust_circ_to_fit cinfo (reduce #'max (cons ctrl lot)) stage)
       (qc_valid_gateinputs (cons ctrl lot) stage 
                            (list ctrl) 
                            (circ_info-qubits_measurement_stage cinfo)))
      (let* ((new_gate (make-gate_fredkin
                        :id (cons (min_of_list (cons ctrl lot)) stage)
                        :in_regular lot
                        :in_1controllers (list ctrl)))
             (circ (circ_info-circ cinfo)))
        (setf (aref circ ctrl stage) new_gate)
        (setf (aref circ (car lot) stage) new_gate)
        (setf (aref circ (cadr lot) stage) new_gate)
        cinfo)
      
      (progn 
        (format t "WARNING: add_fredkin ctrl/lot/stage ~a/~a/~a. Not permitted."
                ctrl lot stage)
        nil)))



;;
;; qc_apply_no_op
;;
;; add a no-op gate
;; 
;; qubit - qubit input
;; stage - stage at which to apply the gate
;; cinfo - the circuit
;;
;; returns the circ_info if successful, or nil otherwise
;;
(defun qc_apply_no_op (qubit stage cinfo)
  (if (and
       (qc_adjust_circ_to_fit cinfo qubit stage)
       (qc_valid_gateinputs (list qubit) stage nil 
                            (circ_info-qubits_measurement_stage cinfo)))
      (let* ((new_gate (make-gate_no_op
                        :id (cons qubit stage)
                        :in_regular (list qubit))))
        (setf (aref (circ_info-circ cinfo) qubit stage) new_gate)
        cinfo)
      (progn
        (format t "WARNING: add_no_op qubit/stage ~a/~a. Not permitted."
                qubit stage)
        nil)))
    

  
;;
;; qc_apply_phshift_gate
;;
;; adds a phase-shift gate of type specified by the user with pidiv
;;
;; cinfo - the circuit
;; qubit - qubit input
;; stage - stage at which to apply the gate
;; pidiv - the divisor for pi
;;
;; returns the circ_info or nil
;;
(defun qc_apply_phshift_gate (cinfo qubit stage pidiv)
  (if (and
       (qc_adjust_circ_to_fit cinfo qubit stage)
       (qc_valid_gateinputs (list qubit) stage nil 
                            (circ_info-qubits_measurement_stage cinfo)))
      (let* ((new_gate 
              (cond
               ((= pidiv 1) (make-gate_phase_shift_pi 
                             :id (cons qubit stage)
                             :in_regular (list qubit)))
               ((= pidiv 2) (make-gate_phase_shift_pi/2
                             :id (cons qubit stage)
                             :in_regular (list qubit)))
               ((= pidiv 4) (make-gate_phase_shift_pi/4
                             :id (cons qubit stage)
                             :in_regular (list qubit)))
               ((= pidiv 8) (make-gate_phase_shift_pi/8
                             :id (cons qubit stage)
                             :in_regular (list qubit)))
               ((= pidiv 16) (make-gate_phase_shift_pi/16
                              :id (cons qubit stage)
                              :in_regular (list qubit)))
               ((= pidiv 32) (make-gate_phase_shift_pi/32
                              :id (cons qubit stage)
                              :in_regular (list qubit)))
               (t (return-from qc_apply_phshift_gate 
                    (qc_apply_customgate (list qubit) stage cinfo 
                                         (format nil "ph_pi/~a_gate" pidiv)
                                         (make-array '(2 2)
                                                     :initial-contents
                                                     `((1 0)
                                                       (0 ,(complex (cos (/ pi pidiv))
                                                                    (sin (/ pi pidiv))))))))))))
        (setf (aref (circ_info-circ cinfo) qubit stage) new_gate)
        cinfo)
      (progn
        (format t 
                "WARNING: add_phase_shift_pi/~a qubit/stage ~a/~a. Not permitted."
                pidiv qubit stage)
        nil)))
                                                                      
;;
;; qc_apply_cphshift_gate
;;
;; add a controlled phaseshift gate
;;
;; cinfo - the circuit
;; controller - the controlling qubit
;; target - the target qubit
;; stage - the stage at which to apply the gate
;; pidiv - the divisor for pi
;;
;; returns the circ_info if successful, or nil otherwise
;;
(defun qc_apply_cphshift_gate (cinfo controller target stage pidiv)
  (if (and
       (qc_adjust_circ_to_fit cinfo (reduce #'max (list controller target)) stage)
       (qc_valid_gateinputs (list target) stage (list controller) 
                            (circ_info-qubits_measurement_stage cinfo)))
      (let* ((new_gate
              (cond
               ((= pidiv 1) (make-gate_cphase_shift_pi
                             :id (cons (min_of_list (list controller target)) stage)
                             :in_1controllers (list controller)
                             :in_regular (list target)))
               ((= pidiv 2) (make-gate_cphase_shift_pi/2
                             :id (cons (min_of_list (list controller target)) stage)
                             :in_1controllers (list controller)
                             :in_regular (list target)))
               ((= pidiv 4) (make-gate_cphase_shift_pi/4
                             :id (cons (min_of_list (list controller target)) stage)
                             :in_1controllers (list controller)
                             :in_regular (list target)))
               ((= pidiv 8) (make-gate_cphase_shift_pi/8
                             :id (cons (min_of_list (list controller target)) stage)
                             :in_1controllers (list controller)
                             :in_regular (list target)))
               ((= pidiv 16) (make-gate_cphase_shift_pi/16
                             :id (cons (min_of_list (list controller target)) stage)
                             :in_1controllers (list controller)
                             :in_regular (list target)))
               ((= pidiv 32) (make-gate_cphase_shift_pi/32
                             :id (cons (min_of_list (list controller target)) stage)
                             :in_1controllers (list controller)
                             :in_regular (list target)))
               (t (return-from qc_apply_cphshift_gate 
                    (qc_apply_customgate (list controller target) stage 
                                         cinfo (format nil "cph_pi/~a_gate" pidiv)
                                         (make-array '(4 4)
                                                     :initial-contents
                                                     `((1 0 0 0)
                                                       (0 1 0 0)
                                                       (0 0 1 0)
                                                       (0 0 0 ,(complex (cos (/ pi pidiv))
                                                                        (sin (/ pi pidiv))))))))))))
        (setf (aref (circ_info-circ cinfo) controller stage) new_gate)
        (setf (aref (circ_info-circ cinfo) target stage) new_gate)
        cinfo)
      (progn 
        (format t "WARNING: add_cphase_shift_pi/~a controller/target/stage ~a/~a/~a. Not permitted."
                pidiv controller target stage)
        nil)))

        

;;
;; qc_apply_customgate
;; creates and adds a custom gate to the circuit
;;
;; loq - list of qubits 
;; stage - which stage of the circuit to add the gate
;; cinfo - circuit information struct for the circuit to be modified 
;; gate_type - string identifying the type of gate
;; tmatrix - transition matrix describing the effects
;;   of the gate. NOTE that this must be a Unitary matrix.
;;
;; returns the circ_info if successful and nil otherwise 
;;
(defun qc_apply_customgate (loq stage cinfo gate_type tmatrix)
  (if (and (qc_adjust_circ_to_fit cinfo (reduce #'max loq) stage)
           (qc_valid_gateinputs loq stage nil 
                                (circ_info-qubits_measurement_stage cinfo))
           (qc_valid_umatrix tmatrix))
      (let* ((new_gate (make-gate
                        :type gate_type
                        :id (cons (min_of_list loq) stage)
                        :numinputs (length loq)
                        :numoutputs (length loq)
                        :in_regular (copy-list loq)
                        :trans_matrix tmatrix)))
        (dolist (x loq t)
          (setf (aref (circ_info-circ cinfo) x stage) new_gate))
        cinfo)
      (progn 
        (format t 
                "WARNING: add_custom at stage ~a. >=1 qubit measured. Not permitted. Qubits:~%"
                stage)
        (dolist (x loq)
          (format t "qubit ~a" x))
        nil)))



;;
;; qc_apply_ctrld_customgate
;; creates and adds a controlled custom gate to the circuit
;; supports control from zero or more 1s, zero or more 0s, and multiple targets
;;
;; lo1ctrls - list of 1-controller qubits
;; lo0ctrls - list of 0-controller qubits
;; lotargs - list of target qubits 
;; stage - which stage of the circuit to add the gate
;; cinfo - circuit information struct for the circuit to be modified 
;; gate_type - string identifying the type of gate
;; tmatrix - transition matrix describing the effects
;;   of the gate. NOTE that this must be a Unitary matrix.
;;
;; returns circ_info if successful and nil otherwise
;;
(defun qc_apply_ctrld_customgate (lo1ctrls lo0ctrls lotargs 
                                           stage cinfo gate_type tmatrix)
  (let* ((loq (remove nil (append lo1ctrls lo0ctrls lotargs)))
         (loctrls (remove nil (append lo1ctrls lo0ctrls))))
    (if (and (qc_adjust_circ_to_fit cinfo (reduce #'max loq) stage)
             (qc_valid_gateinputs lotargs stage loctrls 
                                  (circ_info-qubits_measurement_stage cinfo))
             (qc_valid_umatrix tmatrix))
        (let* ((new_gate (make-gate
                          :type gate_type
                          :id (cons (min_of_list loq) stage)
                          :numinputs (length loq)
                          :numoutputs (length loq)
                          :in_regular (copy-list lotargs)
                          :in_1controllers (copy-list lo1ctrls)
                          :in_0controllers (copy-list lo0ctrls)
                          :trans_matrix tmatrix)))
          (dolist (x loq t)
            (setf (aref (circ_info-circ cinfo) x stage) new_gate))
          cinfo)
        (progn 
          (format t 
                  "WARNING: add_ctrld_custom at stage ~a. >=1 targ qubit measured. Not permitted. Qubits:~%"
                  stage)
          (dolist (x loq)
            (format t "qubit ~a" x))
          nil))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Output functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; print_circ
;; 
;; prints the circuit in a computer-friendly way
;; that can be easily parsed
;;
;; cinfo - the circuit
;;
;; returns t
;;
(defun print_circ (cinfo)
  (let*
      ((num_qubits (circ_info-num_qubits cinfo))
       (num_stages (circ_info-num_stages cinfo))
       (qstate_array (circ_info-qubits_init_state cinfo))
       (circ (circ_info-circ cinfo)))
    (dotimes (i num_qubits t)
      (format t "[Qubit_~4,'0d] ~3a:" i 
          (if (equalp (aref qstate_array i 0) *ket_zero*) "|0>" "|1>"))
      (dotimes (j num_stages t)
        (format t ",~a" (key_gate_data_as_string (aref circ i j) i)))
      (format t "~%"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Pretty Print/Output functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; pretty_print_by_qubits
;;
;; prints the circuit by qubits in a human-friendly way
;;
;; circ_array - the circuit array
;;
;; returns t
;;
(defun pretty_print_by_qubits (circ_array)
   (let* ((ad (array-dimensions circ_array))
          (numrow (car ad))
          (numcol (cadr ad)))
     (dotimes (i numrow t)
       (format t "~&Qubit ~4a: " i)
       (dotimes (j numcol t) 
         (format t "---|~a|---" (format_self (aref circ_array i j) i ))))))



;;
;; pretty_print_qubit_path
;;
;; prints one qubit's path through the circuit in a human-friendly way
;;
;; qubit - the qubit
;; circ_array - the circuit array
;;
;; returns t
;;
(defun pretty_print_qubit_path (qubit circ_array)
  (let* ((ad (array-dimensions circ_array))
         (numcol (cadr ad)))
    (format t "Qubit ~4a: " qubit)
    (dotimes (j numcol t)
      (format t "---|~a|---" (format_self (aref circ_array qubit j) qubit)))))


;;
;; pretty_print_stage
;; 
;; prints a stage in a human-friendly way
;;
;; stage - the stage
;; circ_array - the circuit array
;;
;; returns t
;;
(defun pretty_print_stage (stage circ_array)
  (let* ((ad (array-dimensions circ_array))
         (numrow (car ad)))
    (format t "Stage ~4a: " stage)
    (dotimes (i numrow t)
      (format t "|~a|" (format_self (aref circ_array i stage) i)))))

         
;;
;; pretty_print_by_stages 
;;
;; prints the circuit by stages in a human-friendly way
;;
;; circ_array - the circuit array
;;
;; returns t
;;
(defun pretty_print_by_stages (circ_array)
  (let* ((ad (array-dimensions circ_array))
         (numrow (car ad))
         (numcol (cadr ad)))
    (dotimes (j numcol t)
      (format t "~&Stage ~4a: " j)
      (dotimes (i numrow t)
        (format t "|~a|" (format_self (aref circ_array i j) i))))))
 
       
;;
;; pretty_print_circ
;; 
;; Prints out the circuit in a human readable fashion
;;
;; cinfo - the circuit
;;
;; returns t
;;
(defun pretty_print_circ (cinfo)
  (let* ((circ (circ_info-circ cinfo))
         (numrow (circ_info-num_qubits cinfo))
         (numcol (circ_info-num_stages cinfo))
         (qu_state (circ_info-qubits_init_state cinfo)))
    (format t "~%CIRCUIT ~a:~%" (circ_info-id cinfo))
    (dotimes (i numrow t)
      (format t "~%Qubit_~4,'0d " i)
      ;; print initial state of this qubit
      (cond
       ((equalp (aref qu_state i 0) *ket_zero*) (format t " |0> "))
       (t (format t " |1> ")))
      ;; print the path for this qubit through the circ
      (dotimes (j numcol t)
        (format t "---|~a|---" (format_self (aref circ i j) i))))))

;;
;; qc_save_circ_to_file
;;
;; saves the circuit to a file
;;
;; dir - the directory path for the file
;;       If dir is nil, the directory defined by
;;       *default_circ_output_dir* will be used
;; fname - the name of the file
;; cinfo - the circuit
;;
;; returns nil
;;
(defun qc_save_circ_to_file (dir fname cinfo)
  (let* ((path (cond
                (dir (format nil "~a~a" dir fname))
                (t (format nil "~a~a" *default_circ_output_dir* fname)))))
    (with-open-file (fhandle
                     path
                     :direction :output
                     :if-exists :supersede)
      (format fhandle "~s" cinfo))))

;;
;; qc_read_from_file
;;
;; reads from the file containing the circuit representation 
;;
;; dir - the directory path of the file
;; fname - the name of the file
;; 
;; returns a circ_info structure or nil if not found
;;
(defun qc_read_from_file (dir fname)
  (let* ((path (cond
                (dir (format nil "~a~a" dir fname))
                (t (format nil "~a~a" *default_circ_output_dir* fname)))))
    (with-open-file (fhandle
                     path
                     :direction :input
                     :if-does-not-exist :error)
      (read fhandle))))


;;
;; qc_copy_gate
;; returns a copy of the given gate, possibly adjusted to a different 
;; row/column
;;
;; gate - source gate to copy
;; roffset - optional. if provided, gives the row offset the copied gate 
;;           should be adjusted for
;; coffset - optional; if provided, gives the column offset the copied gate 
;;           should be adjusted for
;;
;; returns a copy of the given gate 
;;
(defun qc_copy_gate (gate &key (roffset 0) (coffset 0))
  (let* ((newgate (copy-structure gate)))
    ;; adjust any fields of the copy that depend upon the row or column.
    ;; the id, the in_regular, the in_[1 or 0]controllers need adjustments.
    ;; the trans_matrix is read-only so a shared copy is OK
    (setf (gate-id newgate) (cons (+ (car (gate-id gate)) roffset)
                                  (+ (cdr (gate-id gate)) coffset)))
    (setf (gate-in_regular newgate) 
          (mapcar (lambda (x) (+ roffset x)) (gate-in_regular gate)))
    (setf (gate-in_1controllers newgate) 
          (mapcar (lambda (x) (+ roffset x)) (gate-in_1controllers gate)))
    (setf (gate-in_0controllers newgate) 
          (mapcar (lambda (x) (+ roffset x)) (gate-in_0controllers gate)))
    newgate))



;;
;; qc_add_circ_to_circ
;; adds gates from the source circ to the dest circ, overwriting 
;; what is in the dest circ at the (possibly offset) locations
;; for instance, if one wants to place gates in the source circ into the
;; dst circ starting at row 3 column 5, one would call with 
;; orow=2 and ocol=4
;; grows the dst circ if necessary to accommodate the src material
;;
;; 
;; src - source circ to copy from
;; dst - destination circ to add to
;; orow - offset of the src row to the dst row
;; ocol - offset of the src col to the dst row
;;
;; returns the modified destination circuit 
;;
;; NOTE: circuit gates kept in an array, where row corresponds to a qubit and 
;; a column corresponds to a stage
;;
(defun qc_add_circ_to_circ (src dst orow ocol)
  (let* ((src_circ (circ_info-circ src))
         (dst_circ (circ_info-circ dst))
         (ad_dst (array-dimensions dst_circ))
         (num_drows (car ad_dst))
         (num_dcols (cadr ad_dst))
         (ad (array-dimensions src_circ))
         (num_srows (car ad))
         (num_scols (cadr ad))
         (needed_mrow (+ orow num_srows))  ; max needed row to fit all src rows
         (needed_mcol (+ ocol num_scols))) ; max needed col to fit all src cols
    ;; check to see if the src data will fit into the destination
    ;; grow the destination if needed and initialize it with
    ;; no-op gates

    #|
    TEMP PRINT
    (format t "~a/~a - drow/col; ~a/~a neededrow/col~%" 
            num_drows num_dcols needed_mrow needed_mcol)
    |#

    (if (> needed_mrow num_drows)
        ;; Need more rows, which affects the circuit and qubits
        (progn
          ;; adjust the circuit array 
          (setf dst_circ (adjust-array dst_circ (list needed_mrow num_dcols)))
          (setf (circ_info-circ dst) dst_circ)
          (setf (circ_info-num_qubits dst) needed_mrow)
          (qc_add_no_ops dst_circ num_drows (1- needed_mrow) 0 (1- num_dcols))

          ;; adjust the qubit related arrays/vectors
          (setf (circ_info-qubits_init_state dst_circ) 
                (adjust-array (circ_info-qubits_init_state dst) 
                              (list needed_mrow 1)
                              :initial-element *ket_zero*))
          (setf (circ_info-qubits_measurement_stage dst_circ)
                (adjust-array (circ_info-qubits_measurement_stage dst) 
                              needed_mrow
                              :initial-element most-positive-fixnum))

          ;; update the dst_circ measurement array with 
          ;; any non-default measurement info from the src circuit
          (let*
              ((src_measurement_stage (circ_info-qubits_measurement_stage src))
               (srclen (length src_measurement_stage)))
            (dotimes (i srclen)
              (if (< (svref src_measurement_stage i) most-positive-fixnum)
                  (let* ((newval (+ (aref src_measurement_stage i) ocol))
                         (newrow (+ i orow)))
                    (setf (svref 
                           (circ_info-qubits_measurement_stage dst_circ) newrow) 
                          newval)))))
          
          ;; update num_drows to reflect the new size
          (setf num_drows needed_mrow)))

    
    (if (> needed_mcol num_dcols)
        ; Need to add more columns to the destination array
        (progn
          (setf dst_circ (adjust-array dst_circ (list num_drows needed_mcol)))
          (setf (circ_info-circ dst) dst_circ)
          (setf (circ_info-num_stages dst) needed_mcol)          
          (qc_add_no_ops dst_circ 0 (1- num_drows) num_dcols (1- needed_mcol))
          ))

    ;; add gates from the source to the destination
    (dotimes (i num_srows t)
      (dotimes (j num_scols t)
        (setf (aref dst_circ (+ i orow) (+ j ocol)) 
              (qc_copy_gate (aref src_circ i j) 
                            :roffset orow 
                            :coffset ocol))))
    dst)) 



;;
;; qc_output_circ_graph
;;
;; output a file in the dot format, suitable for displaying the circuit, 
;; and then invoke the open-source graphviz dot program to generate a 
;; png image representation of the circuit
;;
;; NOTE: the graphviz dot invocation code may need porting to your environment
;;     
;; cinfo - the circuit
;;
;; returns the result of invoking dot    
;;    
(defun qc_output_circ_graph (cinfo)
  (let*
      ((path *default_graph_output_dir*)
       (fname (format nil "~a~a.dot" path (circ_info-id cinfo)))
       (circ (circ_info-circ cinfo))
       (numrow (circ_info-num_qubits cinfo))
       (numcol (circ_info-num_stages cinfo))
       (clist nil)   ; list of commanded type gates which will get vertical links
       (qu_state (circ_info-qubits_init_state cinfo)))
    
    ;; open a file named by the circuit id with a ".dot" suffix
    (with-open-file (fhandle 
                     fname 
                     :direction :output
                     :if-exists :supersede)
      
      ;; start off with text indicating this is 
      ;; a directed graph going left to right
      (format fhandle "digraph { ~% rankdir=LR; ~% color=white; ~%")

      ;; walk through the circuit setting up the graph nodes and attributes
      ;; each is identified by their row and column within the circuit
      ;; set up each row as a cluster 
      (loop for i downfrom (1- numrow) to 0 do 
        ;; first specify this group as a cluster for graphviz
        (format fhandle "~%~%subgraph cluster_~4,'0d {~%" i)
        ;; specify the input qubits
        (format fhandle "qunode_~4,'0d [shape=\"none\" fontsize=28 label=\"~a\"]" 
                i (if (equalp (aref qu_state i 0) *ket_zero*) "|0>" "|1>"))
        ;; now walk the array and specify each node (quantum gate) 
        ;; in the circuit. some kinds of nodes get different attributes
        (dotimes (j numcol t)
          (let* ((gate (aref circ i j)))
            (cond 
             ;; not a commanded gate
             ((not (or (gate-in_1controllers gate) (gate-in_0controllers gate))) 
              (if (gate_no_op-p gate)
                  ;; this is a no-op gate
                  (format fhandle 
                          "~%node_~4,'0d_~4,'0d [shape=\"none\" label=\"---\"]" 
                          i j )
                  ;; else, a non-empty gate
                  (if (> (gate-numinputs gate) 1)
                      (format fhandle 
                              "~%node_~4,'0d_~4,'0d [shape=\"box\" label=\"~a[ID~a]\"]" 
                              i j (gate-type gate) (gate-id gate))
                      ;; else, only one input, don't need to show ID 
                      ;; to help with multi-input association
                      (format fhandle 
                              "~%node_~4,'0d_~4,'0d [shape=\"box\" label=\"~a\"]" 
                              i j (gate-type gate)))))
             (t  
              ;; this is a commanded gate, push it onto the clist 
              ;; for later processing and also set up 
              ;; node attributes differently if a controller or target
              (progn 
                (if (and (eq i (car (gate-id gate)))
                         (eq j (cdr (gate-id gate))))
                    (push gate clist))
                (cond
                 ((member i (gate-in_1controllers gate))  
                  ;; this is a controller based on 1
                  (progn
                    (format fhandle "~%node_~4,'0d_~4,'0d " i j)
                    (format fhandle 
                          "[shape=\"circle\" style=\"filled\" fillcolor=\"lightgrey\" label=\"C1\"]")))
                 ((member i (gate-in_0controllers gate))  
                  ;; this is a controller based on 0
                  (progn
                    (format fhandle "~%node_~4,'0d_~4,'0d " i j)
                    (format fhandle 
                            "[shape=\"circle\" style=\"filled\" fillcolor=\"yellow\" label=\"C0\"]")))
                 ((member i (gate-in_regular gate))  
                  ;; this is a target
                  (format fhandle 
                          "~%node_~4,'0d_~4,'0d [shape=\"box\" label=\"Tgt-~a\"]"
                          i j (gate-type gate)))
                 (t 
                  ;; this is unknown/error
                  (format t 
                          "~%qc_output_circ_graph - Error unknown type row ~a col ~a~%" i j ))))))))
        (format fhandle "} ~%"))
      
      ;; walk through the circuit setting up the horizontal links 
      ;; between the nodes using the attributes as set up above
      (format fhandle "~%~%")
      (loop for i downfrom (1- numrow) to 0 do 
        (format fhandle "~%qunode_~4,'0d " i)
        (dotimes (j numcol t)
          (format fhandle "-> node_~4,'0d_~4,'0d " i j))
        (format fhandle ";~%"))

      ;; walk through the commanded list and set up the vertical links
      ;; between the nodes from commanding nodes to target nodes
      (dolist (gate clist)
        (dolist (1ctrl (gate-in_1controllers gate))
          (dolist (targ (gate-in_regular gate))
            (let* ((col (cdr (gate-id gate))))
              (format fhandle 
                      "~%node_~4,'0d_~4,'0d -> node_~4,'0d_~4,'0d [constraint=false];~%"
                      1ctrl col targ col ))))

        (dolist (0ctrl (gate-in_0controllers gate))
          (dolist (targ (gate-in_regular gate))
            (let* ((col (cdr (gate-id gate))))
              (format fhandle 
                      "~%node_~4,'0d_~4,'0d -> node_~4,'0d_~4,'0d [constraint=false];~%"
                      0ctrl col targ col )))))

      ;; finally, end the directed graph description 
      (format fhandle "~%}~%"))
    
    ;; now, invoke dot program on file to create a png format of the graph
    ;; -->> THIS MUST BE PORTED TO YOUR ENVIRONMENT <<--
    (if *graphviz_invocation_is_ported*
        (progn 
          (format 
           t "~%After you have installed GraphViz and ported the command to invoke it, ~%")
          (format 
           t "comment these prints out and replace the next line in the source code with your ported command.~%")
          (ccl:run-program "bash" (list "-c" "dot -Tpng") :input fname 
                           :output (concatenate 'string path (circ_info-id cinfo) ".png")
                           :if-output-exists :supersede))
        (format t "~%GraphViz invocation has not been ported yet."))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Circuit Validation functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; qc_validate_circ
;;
;; Validates the given circuit.
;; Checks that no qubit is used in a quantum manner after measurement
;; and also that any custom gates are using a unitary matrix
;;
;; cinfo - the quantum circuit to validate
;;
;; returns t if valid and nil otherwise
;;
(defun qc_validate_circ (cinfo)
  (let*
      ((numrow (circ_info-num_qubits cinfo))
       (numcol (circ_info-num_stages cinfo))
       (circ (circ_info-circ cinfo))
       (meas_vec (circ_info-qubits_measurement_stage cinfo)))
    (dotimes (i numrow t)
      (let* ((mstage (svref meas_vec i)))
        (dotimes (j numcol t)
          (let* ((agate (aref circ i j)))
            (if (not (qc_validate_gate agate))
                (progn
                  (format 
                   t "qc_validate_circ: invalid gate ~a at qubit ~a stage ~a~%"
                   (gate-type agate) i j)
                  (return-from qc_validate_circ nil)))
            (if (> j mstage)
                (cond
                 ((gate_no_op-p agate) t)
                 ((member i (gate-in_1controllers agate)) t)
                 ((member i (gate-in_0controllers agate)) t)
                 (t (progn
                      (format t 
                   "qc_validate_circ: invalid gate ~a at qubit ~a stage ~a after a meas~%"
                   (gate-type agate) i j)
                      (return-from qc_validate_circ nil)))))))))))
              
          
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Exec functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; qc_exec_circ
;;
;; A placeholder function for simulating the handoff of the 
;; quantum circuit to an attached quantum computer for processing.  
;;
;; If the *sim_basic_loaded* flag is true and the circuit
;; is compatible with the basic simulator, it will be invoked.
;; Otherwise, this function prints out the quantum information 
;; that would be passed to the quantum computer and also prints 
;; out what measurement operations would be expected in return. 
;;
;; cinfo - the quantum circuit to be executed 
;;
;; returns nil
;;
(defun qc_exec_circ (cinfo)
  (if (and *sim_basic_loaded* (qc_check_simcompat (circ_info-circ cinfo)))
      (qc_simulate_basic cinfo)
      (progn
        (format 
         t "~%No simulator that can support the quantum circuit is loaded.")
        (format 
         t "~%~%qc_exec_circ: if there was an attached quantum computer,~%")
        (format 
         t "the following quantum circuit would be given to it for evaluation --~%")
        (print_circ cinfo)
        (format 
         t "~%Also, the following measurements would be expected as a result:~%")
        (let* ((counter 0))
          (dotimes (i (circ_info-num_qubits cinfo) t)
            (if (< (svref (circ_info-qubits_measurement_stage cinfo) i) most-positive-fixnum)
                (progn (format t "Qubit_~4,'0d measured in stage ~a~%" i 
                               (svref (circ_info-qubits_measurement_stage cinfo) i))
                  (setf counter (+ counter 1)))))
          (format t "~%~a measurements would be expected as a result.~%"
                  counter)))))
    
  

