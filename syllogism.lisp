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
          ((assertion-p input)
           (add-stmt (assertion-stmt input) env)
           (princ "\nok"))
          ((proof-affirmative-p (setf proof (prove (query-stmt input) env)))
           (format t "~%Yes.~{~^~%~S~} q.e.d."
             (mapcar #'output-fact (proof-steps proof))))
          ((proof-steps proof)
           (format t "~%No.~{~^~%~S~} which contradicts the query."
             (mapcar #'output-inference (proof-steps proof))))
          (T (princ "\nToo few information."))))
  (syllogism-repl env))
  
;; returns either an assertion or a query, and NIL for invalid input
(defun parse-input (str)
  "Parse a statement inputted to the syllogism solver."
  NIL)
  
(defun output-inference (inf)
  "Convert inference into a string to be printed."
  NIL)

(defstruct assertion "Wrapper struct for an assertion command." stmt)
(defstruct query "Wrapper struct for a query command." stmt)
  
;;; Core model

(defstruct proof 
  "Stores whether the truth is affirmative or negative, and a list of inferences taken at each step of the proof or refutal."
  affirmative-p steps)

(defun prove (stmt &optional (env *toplevel-env*))
  "Return a proof of the given statement using facts in ENV, if one exists; returns NIL otherwise."
  NIL)
  
;;; Data structures

(defstruct env "Contains an alist mapping each subject to the statements they are a subject of." alist)

(defstruct (stmt (:type list)) 
  "A syllogistic statement that binds a subject to a predicate based on the given type (either A, E, I or O)." 
  sub type pred)

(defconstant inference-rules
  '((A (A A 1))
    (E (E A 1) (A E 2) (E A 2) (A E 4))
    (I (A I 1) (A I 3) (I A 3) (I A 4))
    (O (E A 1) (E I 1) (A E 2) (E A 2) (A O 2) (E I 2)
       (E A 3) (O A 3) (E I 3) (A E 4) (E A 4) (E I 4)))
  "Maps types of conclusions to the different combinations of major and minor premise types and figures that prove it.")

(defparameter *toplevel-env* (make-env) "Default environment for statements.")

(defun add-stmt (stmt &optional (env *toplevel-env*) &aux assoc)
  "Add statement to environment."
  (if (setf assoc (assoc (stmt-sub stmt) (env-alist env)))
      (push (cdr stmt) (cdr assoc))
      (push (list (car stmt) (cdr stmt)) (env-alist env))))
