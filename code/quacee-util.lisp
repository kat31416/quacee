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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Useful functions for the Quacee language 
;;  These are helper functions used by the main Quacee functions
;;  and normally would not be invoked directly by the user.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;
;; format_self
;;
;; formats default info about a given gate in a circuit
;;
;; gate - gate to format 
;; row - row of the gate on the circ (may be used to differentiate formatting)
;;       the row is the identifier of the qubit that the gate is associated with
;;
;; returns string describing the gate
;;
(defun format_self (gate row)
  (cond
   ((member row (gate-in_1controllers gate))
    (format nil "1Ctrl-Tl~7:@<~a~>" (gate-in_regular gate)))
   ((member row (gate-in_0controllers gate))
    (format nil "0Ctrl-Tl~7:@<~a~>" (gate-in_regular gate)))
   ((or (gate-in_1controllers gate) (gate-in_0controllers gate))
    ;; 1 or 0 controllers not nil and this gate not in either so a target
    (format nil "TARG-~10:@<~a~>" (gate-type gate))) 
   (t 
    ;; regular non controller gate
    (format nil "~15:@<~a~>" (gate-type gate)))))



;;
;; key_gate_data_as_string
;;
;; forms gate data into a string
;;
;; gate - the gate
;; row - the row the gate is on
;;
;; returns a string
;;
(defun key_gate_data_as_string (gate row)
  (cond
   ((member row (gate-in_1controllers gate))
    (format nil "(C1_~a[##ID-~d.~d##])" 
            (gate-type gate) (car (gate-id gate)) (cdr (gate-id gate))))
   ((member row (gate-in_0controllers gate))
    (format nil "(C0_~a[##ID-~d.~d##])" 
            (gate-type gate) (car (gate-id gate)) (cdr (gate-id gate))))
   ((or (gate-in_1controllers gate) (gate-in_0controllers gate))
    ;; 1 or 0 controllers not nil and this gate not in either so a target       
    (format nil "(TARG_~a[##ID-~d.~d##])" 
            (gate-type gate) (car (gate-id gate)) (cdr (gate-id gate))))
   (t
    ;; not a commanded gate. use regular output string format
    (format nil "(~a[##ID-~d.~d##])" 
            (gate-type gate) (car (gate-id gate)) (cdr (gate-id gate))))))

   
;;
;; qc_approx_eq
;;
;; approximimately equal test, used when doing comparisons involving
;; floating-point numbers 
;;
;; x - the first number to be compared
;; y - the second number to be compared
;;
;; returns a boolean
;;
(defun qc_approx_eq (x y)
  (if (< (abs (- x y)) *qc_epsilon*)
      t
      nil))

;;
;; mnlist
;; make a list of numbers from 0 to n (exclusive)
;;
;; n - the upper limit for the numbers
;;
;; returns a list
;;
(defun mnlist (n)
  (loop for i 
    from 0 
    below n
    collect i))


;;
;; min_of_list
;; returns the smallest value in a list as determined by min
;;
;; x - the list
;; 
;; returns a number    
;;                                    
(defun min_of_list (x)
  (reduce #'min x))

;;
;; max_of_list
;; returns the biggest value in a list as determined by max
;;
;; x - the list
;; 
;; returns a number    
;;                                    
(defun max_of_list (x)
  (reduce #'max x))

;;
;; qc_valid_gateinputs
;; checks to see if it is OK to add a gate with the given qubits
;; at the given stage, based on when a measurement may have been
;; applied to any of the qubits. Once a measurement has been made,
;; the qubit is no longer in a quantum state.
;; However, even after a measurement has been made, it may be acceptable
;; to use a qubit as a controller, for instance.
;;
;;  loq - list of candidate qubits interacting with the gate
;;        (if a controlled gate, this is a list of the targets)
;;  stage - candidate stage considered for the gate
;;  loctrl - list of qubits in acceptable roles for the candidate gate
;;        (i.e, in a controller role)
;;  meas_array - array indicating the stage at which a measurement has
;;        been applied to each qubit
;;
;; returns a boolean
;;
(defun qc_valid_gateinputs (loq stage loctrl meas_array)
  (dolist (x loq t)
    ;; OK if either this is before any measurement has been made 
    ;; or if the qubit is in a controller role for this candidate gate
    (if (not (or (< stage (aref meas_array x)) (member x loctrl)))
        ;; Not OK. Cannot use these inputs for the candidate gate
        (return-from qc_valid_gateinputs nil))))


;;
;; print_circuit
;;
;; prints the circuit that has just been created
;;
;; array_name - name of the circuit
;;
;; returns t
;;
(defun print_circuit (array_name)
  (let* ((ad (array-dimensions array_name))
         (numrow (car ad))
         (numcol (cadr ad)))
    (dotimes (i numrow t)
       (format t "~&Row ~4,'0b: " i)
       (dotimes (j numcol t) 
         (format t " ~10:@<~a~>" (aref array_name i j))))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  circuit/array related functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;     
;; qc_conjtrans
;;
;; creates the conjugate transpose of a given array
;;
;; array - the array
;;
;; returns an array
;;
(defun qc_conjtrans (array)
  (let* ((ad (array-dimensions array))
         (numrow (car ad))
         (numcol (cadr ad))
         (atrans (make-array `(,numcol ,numrow)))) ; creates transpose
    (dotimes (i numrow t) ; begin looping through rows
      (dotimes (j numcol t) ; begin looping through columns
        (setf (aref atrans j i) ; uses conjugate functon on transpose 
                                ; to create conjugate transpose
              (conjugate (aref array i j))))) 
    atrans))


;;
;; qc_inmult_arr
;; 
;; evaluates the inner product of two given arrays
;;
;; a - the first array
;; b - the second array
;;
;; returns an array if dimensions suitable for an inner product, or nil otherwise
;;
(defun qc_inmult_arr (a b) 
  (let* ((ad_a (array-dimensions a))
         (arows (car ad_a))
         (acols (cadr ad_a))
         (ad_b (array-dimensions b))
         (brows (car ad_b))
         (bcols (cadr ad_b))
         (c (make-array `(,arows ,bcols)))
         (x 0))
    (cond 
     ((= acols brows) ; if columns in A = rows in B, then can continue
      (dotimes (i arows t)
        (dotimes (j bcols t)
          (setf x 0)
          (dotimes (k acols t)
            (incf x (* (aref a i k) (aref b k j)))
            (setf (aref c i j) x))))                   
      c)
     (t nil))))
    


;;
;; qc_is_inv
;; 
;; checks to see if second array is inverse of the first 
;; 
;; a - the first array
;; ainv - the second array
;;
;; returns t if ainv is an inverse of a, nil otherwise
;;
(defun qc_is_inv (a ainv)
  (let* ((multarray (qc_inmult_arr a ainv))
         (mad (array-dimensions multarray))
         (mrows (car mad))
         (mcols (cadr mad)))
    (dotimes (i mrows t)
      (dotimes (j mcols t)
        (if (= i j) 
            ;; diagonals must be 1
            (if (not (qc_approx_eq 1 (aref multarray i j)))
                (return-from qc_is_inv nil))
            ;; non-diagonals must be 0
            (if (not (qc_approx_eq 0 (aref multarray i j)))
                (return-from qc_is_inv nil))))
      t)))           
  
      
;;
;; qc_row_norm 
;;
;; calculates the sum of the squares of the absolute values of a row 
;; and returns t if it equals 1
;;
;; array - an array
;; row - a row
;;
;; returns t if row sum of squares equals 1; else, returns nil
;;
(defun qc_row_norm (array row)
  (let*
      ((numcol (cadr (array-dimensions array)))
       (x 0))
    (dotimes (i numcol t)
      ;; take the absolute value before squaring
      ;; as entries may be complex numbers
      (incf x (expt (abs (aref array row i)) 2)))  
    (if (not (qc_approx_eq x 1))
        nil
        t)))


;;
;; qc_column_norm
;; 
;; calculates the sum of the squares of the absolute values of a column 
;; and returns t if it equals 1
;;
;; array - an array
;; column - a column
;;
;; returns t if column sum of squares equals 1; else, returns nil
;;
(defun qc_column_norm (array column)
  (let*
      ((numrow (car (array-dimensions array)))
       (x 0))
    (dotimes (i numrow t)
      ;; take the absolute value before squaring
      ;; as entries may be complex numbers
      (incf x (expt (abs (aref array column i)) 2)))    
    (if (not (qc_approx_eq x 1))
        nil
        t)))
       
 
;;
;; qc_valid_umatrix          
;;
;; checks if the matrix is valid
;;
;; m - the matrix
;;
;; returns t if matrix valid, and nil otherwise
;;
(defun qc_valid_umatrix (m)
  (let* ((ad (array-dimensions m))
         (numrow (car ad))
         (numcol (cadr ad)))
    (if (not (= numrow numcol)) ; if columns do not = rows, matrix is invalid
        (return-from qc_valid_umatrix nil)
        ;; if the inverse is not the conjugate transpose, matrix is invalid
        (if (not (qc_is_inv m (qc_conjtrans m))) 
            (return-from qc_valid_umatrix nil)
            (progn
              (dotimes (i numrow t)
                ;; if the squares of the row probabilities 
                ;; do not add up to 1, matrix is invalid
                (if (not (qc_row_norm m i)) 
                    (return-from qc_valid_umatrix nil))
                (dotimes (j numcol t)
                  ;; if the squares of the row probabilities 
                  ;; do not add up to 1, matrix is invalid
                  (if (not (qc_column_norm m j)) 
                      (return-from qc_valid_umatrix nil)))))))
    t))
                  

;;
;; Adds no-op gates to the circuit at the given range of rows and columns
;;
;; circ - the circuit in which to add no-op gates
;; startrow_index - index of row to start with
;; lastrow_index - index of row to end with
;; startcol_index - index of column to start with
;; lastcol_index - index of column to end with
;;
;; returns a circuit
;;
(defun qc_add_no_ops (circ startrow_index lastrow_index startcol_index lastcol_index)
  (loop for i from startrow_index to lastrow_index
    do
    (loop for j from startcol_index to lastcol_index
      do
      (setf (aref circ i j) (make-gate_no_op :id (cons i j)
                                             :in_regular (list i))))))


;;
;; qc_validate_gate
;;
;; validate a gate
;;
;; thegate - the gate
;;
;; returns t if gate is valid, nil otherwise
;;
(defun qc_validate_gate (thegate)
  ;; checks to see if gate is either one of recognized types, 
  ;; or has a valid matrix
  (cond 
   ((gate_hadamard-p thegate) t) 
   ((gate_pauliX-p thegate) t)
   ((gate_pauliY-p thegate) t)
   ((gate_pauliZ-p thegate) t)
   ((gate_phase_shift_pi/2-p thegate) t)
   ((gate_meas-p thegate) t)
   ((gate_swap-p thegate) t)
   ((gate_cnot-p thegate) t)
   ((gate_zcnot-p thegate) t)
   ((gate_toffoli-p thegate) t)
   ((gate_fredkin-p thegate) t)
   ((gate_no_op-p thegate) t)
   (t (qc_valid_umatrix (gate-trans_matrix thegate)))))


;;
;; qc_adjust_circ_to_fit
;;
;; extends a circ if needed so that it can support the given row and column
;; (row corresponds to qubit and column corresponds to stage)
;;
;; cinfo - the circuit info
;; row_index - the given row index
;; col_index - the given column index
;;
;; returns the circuit that's been adjusted as needed
;;
(defun qc_adjust_circ_to_fit (cinfo row_index col_index)
  (let* ((num_rows (circ_info-num_qubits cinfo))
         (num_cols (circ_info-num_stages cinfo))
         (needed_rows (+ row_index 1))
         (needed_cols (+ col_index 1)))
    (if (> needed_rows num_rows)
        (progn
          ;; adjust array to add more rows
          (setf (circ_info-circ cinfo) 
                (adjust-array (circ_info-circ cinfo) 
                              (list needed_rows num_cols)))
          (setf (circ_info-num_qubits cinfo) needed_rows)
         
          ;; add no-ops to the new array cells
          (qc_add_no_ops (circ_info-circ cinfo) num_rows (1- needed_rows) 
                         0 (1- num_cols))

          ;; add more qubits for the new rows and init to ket_zero
          (setf (circ_info-qubits_init_state cinfo)
                (adjust-array (circ_info-qubits_init_state cinfo) 
                              (list needed_rows 1)
                              :initial-element *ket_zero*))

          ;; add more rows to the measurement stage vector
          (setf (circ_info-qubits_measurement_stage cinfo) 
                (adjust-array (circ_info-qubits_measurement_stage cinfo) 
                              needed_rows
                              :initial-element most-positive-fixnum))

          ;; update the local var to new number of rows
          (setf num_rows needed_rows)))

    (if (> needed_cols num_cols)
        (progn
          ;; adjust array to add more columns
          (setf (circ_info-circ cinfo) 
                (adjust-array (circ_info-circ cinfo) (list num_rows needed_cols)))
          (setf (circ_info-num_stages cinfo) needed_cols)
          
          ;; add no-ops to the new array cells
          (qc_add_no_ops (circ_info-circ cinfo) 0 (1- num_rows) 
                         num_cols (1- needed_cols))))  
    cinfo))
    
   
   
            
  