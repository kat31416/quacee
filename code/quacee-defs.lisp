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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants and parameters used in Quacee
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *qc_epsilon* 0.000001 
  "epsilon used in comparing non-integers for equality")


(defparameter *ket_zero* (make-array '(2 1) ;; an initial state of a qubit
                                :initial-contents
                                '((1)  ; sqrt prob 1 that it is a zero
                                  (0)  ; sqrt prob 0 that it is a one
                                  )))

(defparameter *ket_one* (make-array '(2 1) ;; an intital state of a qubit
                              :initial-contents
                              '((0)  ; sqrt prob 0 that it is a zero
                                (1)  ; sqrt prob 1 that it is a one
                                )))




;;
;; FLAGS 
;; Simulator-related flags to modify the simulator's behavior
;;

;; constant determining whether or not simulator is loaded
(defparameter *sim_basic_loaded* t) 

;; constant determining whether the simulator should print 
;; each state of the qubits as they evolve
(defparameter *qc_sim_qubitstate_print* nil)                                              

;; constant determining whether the simulator should print the
;; joint probability matrices of the combined qubits as they evolve
(defparameter *qc_sim_qubitjointprob_print* nil) 




;;
;; CONFIGURATION
;; Items that need to be configured to your particular environment
;;

;; default directory to put saved circuits
(defparameter *default_circ_output_dir* "/Public/QCircs/") 

;; default directory to put circuit graphs
(defparameter *default_graph_output_dir* "/Public/GraphOutputs/")




;;
;; PORTING RELATED
;;
;; This is a constant determining whether the command 
;; to invoke GraphViz has been ported to your environment
;;

(defparameter *graphviz_invocation_is_ported* nil) 

