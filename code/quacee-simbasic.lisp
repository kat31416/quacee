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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions to provide a basic simulation of a quantum computer
;;
;; The main entry point to the simulator is the qc_simulate_basic function
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;
;; Structure to hold tracing info for later printing
;;
(defstruct qubit_trace_info
  (label nil)
  (matrix nil))

;;
;; qc_simulate_basic
;;
;; Simulate a quantum computer.
;; This function will simulate a quantum computer carrying out 
;; the operations in the circuit and return the measurement results.
;;
;; Restrictions: 
;; Gate inputs are consistent with the gate's transition matrix
;;  - Inputs on consecutive rows
;;  - Controllers before targets
;;  - 1-controllers (not yet 0-controllers)
;; The user can use swap gates to rearrange circuit such that 
;; the gate inputs meet the ordering restrictions
;; 
;; cinfo - the circuit info
;;
;; returns t if successful or nil if failed
;;
(defun qc_simulate_basic (cinfo)
  (let* ((qubit_inputs (circ_info-qubits_init_state cinfo))
         (circ (circ_info-circ cinfo)))
    (if *sim_basic_loaded*    ;; if a simulator is loaded
        ;; check the circuit's compatibility with the simulator
        (if (qc_check_simcompat circ)   
            ;; create a tensor product of the inputs - the input state
            (let* ((tensorprod_inputs (qc_tensor_product_list 
                                       (reverse (qc_convert_to_list qubit_inputs))))) 
              (format t "~%==================================================~%")
              (format t "BEGIN SIMULATION~%Simulating quantum computer operations~%on the given quantum circuit ~a....~%" 
                      (circ_info-id cinfo))
              (format t "--------------------------------------------------~%")
              ;; evolve the state of the system given the input state and the circuit
              (qc_calc_new_states tensorprod_inputs circ) 
              (format t "END SIMULATION~%")
              (format t "==================================================~%"))
            (return-from qc_simulate_basic nil)) ;; otherwise, return nil
        ;; if no simulator, say no simulator has been loaded
        (format t "~%No simulator has been loaded."))
    t))


;;
;; qc_simulate_basic_noprints
;;
;; Simulate a quantum computer.
;; This function will simulate a quantum computer carrying out 
;; the operations in the circuit and return the final evolved qubit state.
;;
;; Restrictions: 
;; Gate inputs are consistent with the gate's transition matrix
;;  - Inputs on consecutive rows
;;  - Controllers before targets
;;  - 1-controllers (not yet 0-controllers)
;; The user can use swap gates to rearrange circuit such that 
;; the gate inputs meet the ordering restrictions
;; 
;; cinfo - the circuit info
;;
;; returns the evolved qubit state if successful or nil if failed
;;
(defun qc_simulate_basic_noprints (cinfo)
  (let* ((qubit_inputs (circ_info-qubits_init_state cinfo))
         (circ (circ_info-circ cinfo))
         (evolved_state nil))
    (if *sim_basic_loaded*    ;; if a simulator is loaded
        ;; check the circuit's compatibility with the simulator
        (if (qc_check_simcompat circ)   
            ;; create a tensor product of the inputs - the input state
            (let* ((tensorprod_inputs 
                    (qc_tensor_product_list 
                     (reverse (qc_convert_to_list qubit_inputs))))) 
              ;; evolve the state of the system given 
              ;; the input state and the circuit
              (setf evolved_state 
                    (qc_calc_new_states tensorprod_inputs circ)))))
    evolved_state))


;;
;; qc_check_simcompat
;;
;; Checks whether the given circuit is compatible with the simulator loaded.
;;
;; circ - the circuit
;;
;; returns t or nil
;;
(defun qc_check_simcompat (circ)
  (let* ((ad (array-dimensions circ))
         (numrow (car ad))
         (numcol (cadr ad))) 
    ;; if the loops complete, return t
    ;; cond will check for problems and break out early with nil 
    ;; if a problem is found
    (dotimes (j numcol t)  ;; loop through columns
      (dotimes (i numrow t) ;; loop through rows
        (let* ((current_gate (aref circ i j))  
               (inputs (gate-in_regular current_gate))
               (ctrl_1inputs (gate-in_1controllers current_gate))
               (ctrl_0inputs (gate-in_0controllers current_gate))
               (inputs_length (length inputs))
               (ctrl_1inputs_length (length ctrl_1inputs))
               (ctrl_0inputs_length (length ctrl_0inputs))
               (id (car (gate-id current_gate))))
          #|
          ;; TEMP PRINT
          (format t "~%id is ~a~%" id) ;;; TEMP PRINT
          (format t "current gate is ~a~%" current_gate)
          (format t "inputs are ~a~%" inputs)
          (format t "control inputs are ~a~%" ctrl_1inputs)
          (format t "inputs length is ~a~%" inputs_length)
          (format t "control inputs length is ~a~%~%" ctrl_1inputs_length) 
          ;; TEMP PRINT
          |#

          (cond
           ;; 0controllers are not currently supported
           ((> ctrl_0inputs_length 0) 
            (return-from qc_check_simcompat nil)) 
           ;; if there are 1controllers in this gate and it's the
           ;; first time we've seen it while looping...
           ((and (> ctrl_1inputs_length 0) (= id i))
            ;; then it is required that the 1st target input is greater 
            ;; than the controllers, and there are more than one input
            (if (and (> (car inputs) (max_of_list ctrl_1inputs)) 
                     (> (+ inputs_length ctrl_1inputs_length) 1)) 
                ;; if the controllers are not in the proper order
                (if (not (qc_check_multinputs_order id ctrl_1inputs_length 
                                                    ctrl_1inputs 
                                                    (car inputs) 
                                                    inputs_length inputs))
                    ;; not supported, return nil
                    (return-from qc_check_simcompat nil))
                (return-from qc_check_simcompat nil)))  
           (t
            ;; if there are no controllers, just multiple regular inputs, 
            ;; then check the regular inputs order, 
            ;; and if they're not in the right order, return nil
            (if (and (> inputs_length 1) (= id i)) 
                (if (not (qc_check_inputs_order id inputs_length inputs))
                    (return-from qc_check_simcompat nil))))))))))


;;
;; qc_check_inputs_order
;;
;; Checks the order of the inputs of a gate
;; Checks that the gate inputs are consecutive
;;
;; 1st_input - starting input, counting from 0
;; numinputs - number of inputs
;; loinputs - the list of inputs
;;
;; returns t or nil
;;
(defun qc_check_inputs_order (1st_input numinputs loinputs)
  (let* ((input_length (length loinputs)))
    (cond
     ;; if the length of the input list is not equal to the stated 
     ;; number of inputs, something is wrong  
     ((not (eq input_length numinputs)) nil) 
     ;; if the lengths match, check order
     (t
      ;; create an ideal list of inputs
      ;; loop through the existing list
      ;; if any of the elements of the existing list 
      ;; do not equal the ideal list, return nil
      (let* ((idealinput_list (consec_list 1st_input numinputs))) 
        (dotimes (i numinputs t)  
          (if (not (eq (nth i idealinput_list) (nth i loinputs)))  
              (return nil))))))))

;;
;; consec_list
;;
;; Makes a list of consecutive numbers
;; Makes a list of numbers starting at start, with 
;;     numelements elements in it
;;
;; start - starting number
;; numelements - number of elements you want in the list
;;
;; returns the list
;;
(defun consec_list (start numelements)
  (let*
      ((end (+ start numelements))) 
    (loop for i  
      from start
      below end
      collect i)))


;;
;; qc_check_multinputs_order
;;
;; Checks the order of both controller and regular inputs
;;
;; Checks that they both are consecutive, and the controllers 
;; are a lower array index than the inputs
;;
;; 1st_ctrl_input - index of 1st controller
;; numctrl_inputs - number of controllers
;; loctrl_inputs - list of controllers
;; 1st_input - index of 1st regular input
;; numinputs - number of regular inputs
;; loinputs - list of inputs
;;
;; returns t if successful and nil if not
;;
(defun qc_check_multinputs_order (1st_ctrl_input 
                                  numctrl_inputs 
                                  loctrl_inputs 1st_input 
                                  numinputs loinputs)
  (let* ((ctrl_input_length (length loctrl_inputs))) 
    (cond
     ;; if the length of the controller list is not equal to the 
     ;; number of controllers, return nil
     ((not (eq ctrl_input_length numctrl_inputs)) nil) 
     ;; make an ideal controller list, compare existing controller 
     ;; input list, if not equal, return nil
     (t (let* ((idealctrl_list (consec_list 1st_ctrl_input numctrl_inputs))) 
          (dotimes (i numctrl_inputs t)
            (if (not (eq (nth i idealctrl_list) (nth i loctrl_inputs))) 
                (return-from qc_check_multinputs_order nil))))))
    ;; if first regular input isn't 1 away from the last 
    ;; controller, return nil
    (if (not (= (- (first loinputs) (car (last loctrl_inputs))) 1)) 
        (return-from qc_check_multinputs_order nil))
    ;; ok so far, check the regular inputs
    (qc_check_inputs_order 1st_input numinputs loinputs))) 


;;
;; qc_tensor_product
;;
;; makes the tensor product of the two matrices
;;
;; matrix1 - 1st matrix
;; matrix2 - 2nd matrix
;;
;; returns the tensor producted matrix
;;
(defun qc_tensor_product (matrix1 matrix2)
  (let* ((ad1 (array-dimensions matrix1))
         (ad2 (array-dimensions matrix2))
         (numrowA (car ad1))
         (numcolA (cadr ad1))
         (numrowB (car ad2))
         (numcolB (cadr ad2))
         (matrixC (make-array (list (* numrowA numrowB) (* numcolA numcolB))))
         (Cstartrow 0)
         (Cstartcol 0))
    (dotimes (ar numrowA t)
      (dotimes (ac numcolA t) ;; loop through rows, and for each row its columns  
        ;; multiply the row by number of rows in 2nd matrix to get starting row 
        ;;    for C
        ;; multiply the col by number of cols in 2nd matrix to get starting col 
        ;;    for C
        (setf Cstartrow (* ar numrowB)) 
        (setf Cstartcol (* ac numcolB)) 
        ;; with the element in A, multiply it by each element in B, 
        ;; and insert the results into C beginning with the startrow 
        ;; and startcol, running through 
        ;; the rows in B and the columns in B
        (dotimes (br numrowB t)
          (dotimes (bc numcolB t)
            (setf (aref matrixC (+ Cstartrow br) (+ Cstartcol bc)) 
                  (* (aref matrix1 ar ac) (aref matrix2 br bc))))))) 
    matrixC))




;;
;; qc_tensor_product_list
;;
;; applies qc_tensor_product to a list of matrices
;;
;; matrix_list - list of matrices
;;
;; returns tensor product of the list
;;
(defun qc_tensor_product_list (matrix_list)
  (reduce #'qc_tensor_product matrix_list)) 



;;
;; qc_convert_to_list
;;
;; converts a one-dimensional matrix of matrices to a list of matrices.
;; list will be in reverse order
;;
;; matrix - the matrix
;;
;; returns the list
;;
(defun qc_convert_to_list (matrix)     
  (let* ((ad (array-dimensions matrix))
         (numrow (car ad))          
         (the_list nil))
    (dotimes (i numrow) 
      ;; add the new matrix element to front of list
      (setf the_list (cons (aref matrix i 0) the_list))) 
    the_list))



;;
;; qc_calc_new_states
;;
;; calculates the new qubit states as they evolve thru the circuit.
;; prints out any measurements taken (e.g., by measurement gate)
;; if *qc_sim_qubitstate_print* is set, traces qubit state evolution
;; if *qc_sim_qubitjointprob_print* is set, traces joint prob evolution
;;
;; input_state - initial tensor product of the input qubits
;; circ - the quantum circuit
;;
;; returns the final qubit state
;;
(defun qc_calc_new_states (input_state circ)  
  (let* (
         (ad (array-dimensions circ))
         (numrow (car ad))
         (numcol (cadr ad))
         (gate_list nil)
         (current_state input_state)
         (meas_list nil)
         (state_list (cons (make-qubit_trace_info 
                            :label (format nil "Initial")
                            :matrix input_state) nil))
         (joint_prob_list (cons (make-qubit_trace_info 
                                 :label (format nil "Initial")
                                 :matrix (create_joint_prob_dist input_state)) 
                                nil)))
    (dotimes (j numcol) ; begin looping through the stages
      (setf gate_list nil) ; set the list of gates to nil
      (setf meas_list nil) ; set the list of measurements to nil
      (dotimes (i numrow) ; now begin looping through the qubits
        (if (eq (car (gate-id (aref circ i j))) i) ; if there is a new gate here
            ;;; makes list in reverse order
            ;; add the transition matrix of the gate to our list
            (setf gate_list (cons (gate-trans_matrix (aref circ i j)) 
                                  gate_list))) 
        ;; if the gate is a measurement gate, add it to the measurement list, 
        ;; continue this process until done with all qubits in the stage
        (if (eq (gate-type (aref circ i j)) 'measure_gate) 
            (setf meas_list (cons i meas_list)))) 

      ;; set the new current state to the tensor product of the gates 
      ;; multiplied by the prior current state
      (setf current_state (qc_inmult_arr (qc_tensor_product_list 
                                          (reverse gate_list)) 
                                         current_state)) 
      ;; add the current state to the list of states the qubits have gone through
      (setf state_list (cons (make-qubit_trace_info 
                              :label (format nil "Stage~a" j) 
                              :matrix current_state) state_list))

      ;; add the joint probability matrix of the current state to 
      ;; the list of joint prob evolutions
      (setf joint_prob_list (cons (make-qubit_trace_info 
                                   :label (format nil "Stage~a" j) 
                                   :matrix (create_joint_prob_dist 
                                            current_state)) 
                                  joint_prob_list))
      #|
      ;; TEMP PRINT
      ;;(print_matrix current_state)
      ;; TEMP PRINT
      |#

      (if meas_list  ; if there have been measurements
          (progn (format t "MEASUREMENTS at Stage ~a~%" j)
            ;; change the current state to account for the measurements
            (setf current_state (qc_adjust_state_probs 
                                 current_state meas_list numrow)) 
            ;; replace the original state with the changed one
            (setf state_list (cons (make-qubit_trace_info 
                                    :label (format nil "Stage~a" j) 
                                    :matrix current_state) (cdr state_list)))))
      ) ; done looping through columns

    (if *qc_sim_qubitstate_print*
        (progn (format t "--------------------------------------------------~%")
          (format t "Trace of Qubit state evolution:~%")
          (print_qubit_state_evo (reverse state_list))
          (format t "--------------------------------------------------~%")))
    (if *qc_sim_qubitjointprob_print*
       (progn (format t "--------------------------------------------------~%")
         (format t "Trace of Qubit Joint Probability evolution:~%")
         (print_qubit_state_evo (reverse joint_prob_list))
         (format t "--------------------------------------------------~%")))
    current_state))



;; 
;; print_matrix
;;
;; prints a matrix in a reader-friendly format
;;
;; matrix - the given matrix
;;
;; returns nil
;;
(defun print_matrix (matrix)          
  (let* ((ad (array-dimensions matrix))
         (numrow (car ad))
         (numcol (cadr ad)))
    ;;(format t "[~%")
    (dotimes (i numrow) ; first begin looping through rows
      (progn (format t "  ~TRow")
        (format t " ~4,'0B, Value:~T" i))
      (dotimes (j numcol) ; then begin looping through columns
          (format t "~6,4F~T" (aref matrix i j)))
      (format t "~%"))
    ;;(format t "]~%")
    ))



;;
;; qc_adjust_state_probs
;;
;; adjusts the probabilities of the qubit states based on measurements.
;;
;; qubits_state_matrix - matrix of square roots of input state probs,
;;                      the tensor product of the qubit states
;; loMeas - list of gates measured
;; numqubits - number of qubits
;;
;; returns the new matrix of states
;;
(defun qc_adjust_state_probs (qubits_state_matrix loMeas numqubits)
  (let* ((ad (array-dimensions qubits_state_matrix))
         (numrow (car ad))
         (prob_of_1_sum 0)
         (meas_row (car loMeas)))
    (cond
     ;; no more measurements, return matrix
     ((eq loMeas nil) qubits_state_matrix)
     ;; simulate taking a measurement on the given qubit(row).
     ;; note that the top qubit in the circuit is the zero row 
     ;; of circ_info-qubits-init-state 
     ;; array of qubit state matrices,
     ;; but is the high order bit position of each row index of 
     ;; the tensor product of the qubit states.
     ;; simulate the measurement by adding up the probability 
     ;; of the to-be-measured qubit being a 1,
     ;; and then use a random number generator (via qc_simulate_meas) 
     ;; to "roll the dice" of measuring either the 1 or 0.
     ;; then have to go through the tensored qubit state matrix 
     ;; and adjust the probabilities to account for the 
     ;; simulated measurement (either 1 or 0) [done in qc_calc_mod_distrib]
     (t (progn
          (let* ((bitpos (- numqubits 1 meas_row))
                 (mask (ash 1 bitpos))
                 (targval mask))
            (dotimes (i numrow)
              ;; find where the qubit at bitpos equals 1
              (if (= (logand i mask) targval)  
                  ;; add the prob to the sum
                  (incf prob_of_1_sum 
                        (qc_state_to_prob (aref qubits_state_matrix i 0)))))  
            ;; prob_of_1_sum holds the probability that the qubit 
            ;; would be a 1 if measured, 
            ;; simulate a measurement using that probability
            (if (qc_simulate_meas prob_of_1_sum)
                (progn 
                  ;; simulated measurement of a 1, the qubit collapsed to a 1
                  ;; modify state matrix to reflect this, and 
                  ;; print measurement results
                  (setf qubits_state_matrix (qc_calc_mod_distrib 
                                             qubits_state_matrix bitpos 1))
                  (format t "~TQubit ~a = 1~%" meas_row))
                (progn 
                  ;; simulated measurement of a 0, the qubit collapsed to a 0
                  ;; modify state matrix to reflect this, and 
                  ;; print measurement results
                  (setf qubits_state_matrix 
                        (qc_calc_mod_distrib qubits_state_matrix bitpos 0))
                  (format t "~TQubit ~a = 0~%" meas_row))))
          (qc_adjust_state_probs qubits_state_matrix (cdr loMeas) 
                                 numqubits))))))



;;
;; qc_prob_to_state 
;;
;; takes the square root of the prob to turn it into a qubit state
;;
;; val - the value
;;
(defun qc_prob_to_state (val)
  (sqrt val))



;;
;; qc_state_to_prob
;;
;; takes the square of the absolute value of the qubit state to turn it into a prob
;;
;; val - the value
;;
(defun qc_state_to_prob (val)
  (* (abs val) (abs val)))



;;
;; qc_calc_mod_distrib
;;
;; changes the prob distribution of the states based on evidence
;;
;; joint_sqrtprob_matrix - square roots of joint prob distr
;;                         (tensor product of the qubit states)
;; bitpos - bit position of interest
;; evidence - evidence value
;;
;; returns the new joint square root prob distribution matrix 
;;
(defun qc_calc_mod_distrib (joint_sqrtprob_matrix bitpos evidence)
  (let* ((mask (ash 1 bitpos))
         (targval 0)
         (ad (array-dimensions joint_sqrtprob_matrix))
         (numrow (car ad))
         (sum_prob_of_evidence 0))
    ;; set up target based on the evidence
    (if (= evidence 1)
        (setf targval mask)
        (setf targval 0))
    ;; sum up the prior probability of the evidence
    ;; convert from state to probability for calculation
    (dotimes (i numrow)
      (if (= (logand i mask) targval)
          (incf sum_prob_of_evidence 
                (qc_state_to_prob (aref joint_sqrtprob_matrix i 0)))))
    ;; adjust square roots of probabilities to account for 
    ;; the evidence (either 1 or 0)
    (dotimes (i numrow)
      ;; run through the matrix and adjust the joint square root 
      ;; probabilities to account for the evidence. Rows which 
      ;; contradict evidence get a value of zero;
      ;; rows which are consistent are scaled by having 
      ;; joint square root prob divided by square root prob of the evidence
      ;;
      ;; this process uses bayes law --> 
      ;; prob(a, b, c,... | b=evidence) = prob(a,b,c....)/prob(b=evidence)
      ;; but using square roots of prob
      (if (= (logand i mask) targval)
          (setf (aref joint_sqrtprob_matrix i 0) 
                (/ (aref joint_sqrtprob_matrix i 0) 
                   (qc_prob_to_state sum_prob_of_evidence)))  
          (setf (aref joint_sqrtprob_matrix i 0) 0)))  
    joint_sqrtprob_matrix))



;;
;; qc_simulate_meas
;; 
;; simulates taking a measurement with a certain prob
;;
;; prob - probability
;;
;; returns t or nil
;;
(defun qc_simulate_meas (prob)
  (if (<= (random 1.0) prob)
      t
      nil))


;;
;; print_qubit_state_evo
;;
;; prints the evolution of the qubits through the circuit
;;
;; loinfos - list of qubit_trace_infos with information to print
;;
;; returns nil
;;
(defun print_qubit_state_evo (loinfos)
  (cond
   ((eq loinfos nil) t)
   (t
    (progn (format t "~a~%" (qubit_trace_info-label (car loinfos)))
      (print_matrix (qubit_trace_info-matrix (car loinfos))) 
      (print_qubit_state_evo (cdr loinfos))))))



;;
;; create_joint_prob_dist
;;
;; creates a joint prob distribution out of matrix of square roots of probs
;;
;; square_root_probs_matrix - matrix of square roots of probs
;;
;; returns joint prob matrix
;;
(defun create_joint_prob_dist (square_root_probs_matrix)
  (let* ((ad (array-dimensions square_root_probs_matrix))
         (numrow (car ad))
         (numcol (cadr ad))
         (jp_matrix (make-array `(,numrow ,numcol) :initial-element nil)))
    (dotimes (j numcol)
      (dotimes (i numrow)
        (setf (aref jp_matrix i j) 
              (expt (abs (aref square_root_probs_matrix i j)) 2))))
    jp_matrix))



