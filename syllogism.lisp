;;;; Syllogism solver

;;; Structs

(defstruct assertion "Wrapper struct for an assertion command." stmt)
(defstruct query "Wrapper struct for a query command." stmt)

(defstruct proof 
  "Stores whether the truth is affirmative or negative, and a list of steps describing a polysyllogism."
  affirmative-p steps)

(defstruct env "Contains alists mapping each subject/predicate to the statements they are a subject/predicate of." 
  subs preds)

(defstruct (stmt (:type list)) 
  "A syllogistic statement that binds a subject to a predicate based on the given type (either A, E, I or O)." 
  sub type pred)

;;; Globals

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

;;; Main program

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
  
;; TODO
  
;; returns either an assertion or a query, and NIL for invalid input
(defun parse-input (str)
  "Parse a statement inputted to the syllogism solver."
  NIL)
  
(defun output-inference (inf)
  "Convert inference into a string to be printed."
  NIL)
  
;;; Core proof algorithm

(defun prove (stmt &optional (env *toplevel-env*))
  "Return a proof of the given statement using facts in ENV, if one exists; returns NIL otherwise."
  (let ((pred-stmts (cdr (assoc (stmt-pred stmt) (env-preds env) :test #'equal)))
        (rules      (cdr (assoc (stmt-type stmt) inference-rules))) 
        other)
    (cond ((or (null pred-stmts)
               (null (cdr (assoc (stmt-sub stmt) (env-subs env) :test #'equal))))
           ;; No information about the subject or the predicate, return disproof
           (make-proof))
          ((setf other 
             (find stmt pred-stmts :test #'subsumes-p))
           ;; Another statement subsumes stmt, return proof
           (add-stmt stmt env)
           (make-proof :affirmative-p T :steps (list other)))
          ((setf other 
             (find stmt pred-stmts :test #'contradicts-p))
           ;; Another statement contradicts stmt, return disproof
           (make-proof :steps (list other)))
          (T (search-inferences stmt rules env)))))

(defun search-inferences (stmt rules &optional (env *toplevel-env*))
  "Look for an inference that can derive the given statement from the given inference rules."
  (do* ((rules (cons NIL rules))
        rule middle stmts proof) ;defined and redefined after check
       ((and (null (cdr rules)) (null stmts))
        (make-proof)) ;no rules can deduce stmt, return disproof
       (cond ((null stmts) 
              ;; No premises left, try next inference rule
              (setf rules  (cdr rules)
                    rule   (car rules)
                    middle (middle-category (third rule))
                    stmts  (remove (first rule)
                             (major-premises (third rule) (stmt-pred stmt) env)
                             :key #'stmt-type :test-not #'equal)))
             (T
              ;; Try to prove the corresponding minor premise to the first major premise in stmts
              (setf proof 
                (prove (minor-premise 
                         (third rule) (stmt-sub stmt) (second rule) (funcall middle (car stmts)))
                       env))
              (when (proof-affirmative-p proof)
                ;; Add the major premise and stmt to the proof and return it
                (add-stmt stmt env) ;consider stmt proven
                (push (car stmts) (proof-steps proof))
                (rplacd (last (proof-steps proof)) stmt)
                (return proof))
              ;; Go to next major premise
              (pop stmts))))
              
;;; Auxiliary functions

(defun contradicts-p (stmt1 stmt2)
  "Return T if STMT2 refutes STMT1."
  (and (equal (stmt-sub stmt1) (stmt-sub stmt2))
       (equal (stmt-pred stmt1) (stmt-pred stmt2))
       (member (stmt-type stmt1) (assoc (stmt-type stmt2) contradictions))))

(defun subsumes-p (stmt1 stmt2)
  "Return T if STMT2 subsumes STMT1."
  (and (equal (stmt-sub stmt1) (stmt-sub stmt2))
       (equal (stmt-pred stmt1) (stmt-pred stmt2))
       (member (stmt-type stmt1) (assoc (stmt-type stmt2) subsumptions))))

(defun minor-premise (figure sub type mid)
  "Construct new minor premise from the given subject, type and middle category based on the given figure."
  (if (< figure 3)
      (list sub type mid)
      (list mid type sub)))

(defun major-premises (figure pred &optional (env *toplevel-env*))
  "Return a list of major premises containing PRED based on FIGURE."
  (if (evenp figure)
      (cdr (assoc pred (env-subs env) :test #'equal))
      (cdr (assoc pred (env-preds env) :test #'equal))))

(defun middle-category (figure)
  "Return accessor function to the middle category of a major premise based on the given figure."
  (if (evenp figure)
      #'stmt-pred
      #'stmt-sub))

(defun add-stmt (stmt &optional (env *toplevel-env*))
  "Add statement to environment."
  (alist-push (stmt-sub stmt) stmt (env-subs env) :test #'equal)
  (alist-push (stmt-pred stmt) stmt (env-preds env) :test #'equal)
  stmt)

(defun known-p (stmt &optional (env *toplevel-env*))
  "Return T if statement is already known in environment."
  (member (cdr stmt) (assoc (car stmt) (env-subs env) :test #'equal) :test #'equal))

(defmacro alist-push (key val alist &rest options)
  "Add key-value pair to alist."
  (let ((key-var (gensym "KEY"))
        (val-var (gensym "VAL"))
        (assoc-var (gensym "ASSOC")))
    `(let* ((,key-var ,key)
            (,val-var ,val)
            (,assoc-var (assoc ,key-var ,alist .,options)))
       (if ,assoc-var
           (pushnew ,val-var (cdr ,assoc-var))
           (push (list ,key-var ,val-var) ,alist))
       ,alist)))
