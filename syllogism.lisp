;;;; Syllogism solver

;;; Front end

;; The main program
(defun syllogism-repl (&optional (env *toplevel-env*))
  "Run REPL for the syllogism solver."
  (princ "\n\n==> ")
  (let ((input (parse-input (read-line))) proof)
    ;; TODO include an option to quit
    (cond ((null input)
           (princ "\nError: invalid input."))
          ((fact-p input)
           (add-fact (fact input) env)
           (princ "\nok"))
          ((setf proof (prove (query input) env))
           (format t "~%Yes.~{~^~%~S~} q.e.d."
             (mapcar #'output-fact (steps proof))))
          (T (princ "\nNo.")))) ;perhaps describe why the statement is wrong, or where a contradiction occurred.
  (syllogism-repl env))
  
(defun parse-input (str)
  "Parse a statement inputted to the syllogism solver."
  NIL)
  
(defun output-fact (fact)
  "Convert fact into a string to be printed."
  NIL)

(defun fact-p (input)
  "Return T if the input describes a fact."
  NIL)
  
(defun fact (input)
  "Return the fact in an input, if it describes one."
  NIL)

(defun query-p (input)
  "Return T if the input describes a query."
  NIL)
  
(defun query (input)
  "Return the query in an input, if it describes one."
  NIL)
  
;;; Core model

(defun prove (query &optional (env *toplevel-env*))
  "Return a proof of QUERY using facts in ENV, if one exists; returns NIL otherwise."
  NIL)
  
(defun steps (proof)
  "Return each fact and derivation used in proof."
  NIL)
  
;;; Data structures

(defparameter *toplevel-env* NIL)

(defun add-fact (fact &optional (env *toplevel-env*))
  "Add fact to environment."
  NIL)
  
