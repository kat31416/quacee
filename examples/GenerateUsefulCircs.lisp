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


(load "quacee")

;;
;; Generates a number of useful quantum circuits and
;; saves them for future use
;;
;; Creates set of Superposition starter circuits, with
;; ket-zero's followed by Hadamards:
;;    Superpos01
;;    Superpos02
;;    Superpos03
;;    Superpos04
;;    Superpos05
;;    Superpos06
;;    Superpos07
;;    Superpos08
;;    ... to Superpos32
;;
(defun gencircs ()
  (let* ((new_circ nil)) 
    (loop for count from 1 to 32 ; loop through the following process 32 times
      do 
      (setf new_circ (qc_create_circ (format nil "Superpos~2,'0d" count) :est_qubits 1 :est_stages 1)) ; initialize the new circuit each time
      (qc_apply_mult_hadamards (mnlist count) 0 new_circ) ; apply the number of hadamards to the number of qubits in the circuit name
      (qc_save_circ_to_file nil (format nil "Superpos~2,'0d.qcirc" count) new_circ)))) ; save the circuit to a file
      
    
    

    