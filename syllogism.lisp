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
             (mapcar #'output-fact (reverse (proof-steps proof)))))
          ((proof-steps proof)
           (format t "~%No.~{~^~%~S~} which contradicts the query."
             (mapcar #'output-inference (reverse (proof-steps proof)))))
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
  ;; Consider the statements with the same subject as stmt.
  ;; If another statement subsumes stmt, consider it trivially proven with steps containing that statement.
  ;; If another statement contradicts stmt, consider it disproven with steps containing the contradictory statement.
  ;; Otherwise, go through each inference rule that can conclude with (stmt-type stmt).
  ;; Find a major premise (a premise containing the predicate) that can derive the statement with the type given by the rule.
  ;; If no such premise exists, jump to the next inference rule.
  ;; Otherwise, go through each viable premise.
  ;; Construct the required minor premise from the subject and the middle category and try to prove it.
  ;; If it is disproven, go to the next premise.
  ;; If it is proven, return the same proof structure with this inference added to the front of the steps.
  ;; If no premises are left, jump to the next inference rule.
  ;; If no inference rules are left, return negative proof with no steps (lack of information, as opposed to contradiction).
  ;; If all searched premises are disproven, signify contradiction, else lack of knowledge.
  )

(defun contradicts-p (stmt1 stmt2)
  "Return T if STMT1 refutes STMT2."
  (and (eq (stmt-sub stmt1) (stmt-sub stmt2))
       (eq (stmt-pred stmt1) (stmt-pred stmt2))
       (member (stmt-type stmt2) (assoc (stmt-type stmt1) contradictions))))

(defun subsumes-p (stmt1 stmt2)
  "Return T if STMT1 subsumes STMT2."
  (and (eq (stmt-sub stmt1) (stmt-sub stmt2))
       (eq (stmt-pred stmt1) (stmt-pred stmt2))
       (member (stmt-type stmt2) (assoc (stmt-type stmt1) subsumptions))))
  
;;; Data structures

(defstruct env "Contains alists mapping each subject/predicate to the statements they are a subject/predicate of." 
  subs preds)

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

(defconstant contradictions
  '((A E O) (E A I) (I E) (O A))
  "Maps each type to the types they contradict.")

(defconstant subsumptions
  '((A A I) (E E O) (I I) (O O))
  "Maps each type to the types they subsume.")

(defparameter *toplevel-env* (make-env) "Default environment for statements.")

(defmacro alist-push (key val alist &rest options)
  "Add key-value pair to alist."
  (let ((key-var (gensym "KEY"))
        (val-var (gensym "VAL"))
        (assoc-var (gensym "ASSOC")))
    `(let* ((,key-var ,key)
            (,val-var ,val)
            (,assoc-var (assoc ,key-var ,alist .,options)))
       (if ,assoc-var
           (push ,val-var (cdr ,assoc-var))
           (push (list ,key-var ,val-var) ,alist))
       ,alist)))

(defun add-stmt (stmt &optional (env *toplevel-env*))
  "Add statement to environment."
  (alist-push (stmt-sub stmt) stmt (env-subs env))
  (alist-push (stmt-pred stmt) stmt (env-preds env)))

(defun known-p (stmt &optional (env *toplevel-env*))
  "Return T if statement is already known in environment."
  (member (cdr stmt) (assoc (car stmt) (env-subs env)) :test #'equal))

(defun sub-minor-premise (figure sub type mid)
  "Construct new minor premise from the given subject, type and middle category based on the given figure."
  (if (< figure 3)
      (list sub type mid)
      (list mid type sub)))

(defun pred-premise-alist (figure &optional (env *toplevel-env*))
  "Return the alist in which to search for a premise containing a given predicate based on the given figure."
  (if (evenp figure)
      (env-subs env)
      (env-preds env)))
