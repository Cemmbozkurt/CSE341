(setq newList '())
(setq typeList '())
(setq resultList '())
(defun readFile (filename)
    (let ((in (open filename :if-does-not-exist nil)))
        (when in
            (loop for line = (read-line in nil)
                while line do (parse_string line))
            (close in)
        )
    )
)

(defun parse_string(input &optional (start 0) (end 0))

    (cond
        (
            (>= end (length input))
            t
        )

        (
            (and (not (alpha-char-p(char input start))) (not(digit-char-p(char input start))) (string/= "_" (char input start)) )   ; if(input[i] != alphabetic  && input[i] != digit)
                (cond
                    (
                        (and (string= ";" (char input start)) (string= ";" (char input (+ 1 start))))
                            (setf newList (append newList(list ";;")))
                            t
                    )

                    (
                        (string/= " "(char input start))
                            (setf newList (append newList(list(char input start))))
                            (parse_string input (+ end 1) (+ end 1))
                    )

                    (
                        (parse_string input (+ end 1) (+ end 1))
                    )
                        
                )
        )

        (
            (or (alpha-char-p(char input end)) (digit-char-p(char input end)) (string= "_" (char input end)))
                (parse_string input start (+ 1 end))
        )

        (
            (not (eq start end))
                (setf newList (append newList(list(subseq input start end))))
                (parse_string input end end)
        )

    )

)


(defun detect (str)
    (cond
        ((string= str "+")  "OP_PLUS")
        ((string= str "-") "OP_MINUS")
        ((string= str "/") "OP_DIV")
        ((string= str "*") "OP_MULT")
        ((string= str "(") "OP_OP")
        ((string= str ")") "OP_CP")
        ((string= str #\" ) "OP_OC")
        ((string= str "**") "OP_DBLMULT")
        ((string= str ",") "OP_COMMA")
        ((string= str ";;") "COMMENT")

        ((string= str "and")  "KW_AND")
        ((string= str "or") "KW_OR")
        ((string= str "not") "KW_NOT")
        ((string= str "equal") "KW_EQUAL")
        ((string= str "less") "KW_LESS")
        ((string= str "nil") "KW_NIL")
        ((string= str "list") "KW_LIST")
        ((string= str "append") "KW_APPEND")
        ((string= str "concat") "KW_CONCAT")
        ((string= str "set") "KW_SET")
        ((string= str "deffun") "KW_DEFFUN")
        ((string= str "for") "KW_FOR")
        ((string= str "if") "KW_IF")
        ((string= str "exit") "KW_EXIT")
        ((string= str "load") "KW_LOAD")
        ((string= str "disp") "KW_DISP")
        ((string= str "true") "KW_TRUE")
        ((string= str "false") "KW_FALSE")

        (t (controlIdenty str))
    )
)

(defun controlIdenty (str)
    (cond
        ((digit-char-p (char str 0)) (controlValue str))
        (  (and (string/= "_" (char str 0)) (not (alpha-char-p (char str 0))))    "ERROR")
        (t  "IDENTIFIER")
    )
)

(defun controlValue (str)
    (setq counter 0)
    (if (and (< 1 (length str)) (char= #\0 (char str 0)))  (return-from controlValue "ERROR"))
    (setq i 0)

    (loop while (< i (length str))
        do(cond
            (
                (and(string= "f" (char str i)))
                (setf counter (+ 1 counter))

            )
        )
         (if (and (> counter 1) (not(digit-char-p (char str i)))) (return-from controlValue "ERROR"))
        (setf i (+ 1 i))
    )
    
    (cond
        (
            (= 1 counter) (return-from controlValue "VALUEF")
        )

        (
            (return-from controlValue "VALUEI")    
        )
    )
    
)

(defun classify (input)
    (setq temp nil)
    (dolist (i newList)
        (setf typeList (detect (string i)))
        (setf temp (append temp (list (string i) (detect(string i)) )))
        (setf resultList (append resultList (list temp)))
        (setf temp nil)
    )
)


(defun findString(input)
    (setq i 0)
    (setq compare (list "\"" "OP_OC"))
    (loop while (< i (length input))
    
        do(cond
            (
                (and (equal compare (nth i input)) (equal compare (nth (+ i 2) input)))
                (setf i (+ 1 i))
                (setf (nth 1 (nth i input)) "VALUESTR")
                (setf i (+ 1 i))
                (setf(nth 1 (nth i input)) "OP_CC")
            )
        )
    
        (setf i (+ 1 i))
    )

)




(defun replace-f-with-slash (input)
  (let ((result ""))
    (dotimes (i (length input))
      (let ((ch (aref input i)))
        (if (and (char= ch #\f) (not(= i 0)) (digit-char-p (aref input (- i 1))) (digit-char-p (aref input (+ i 1))))
            (setf result (concatenate 'string result "/"))
          (setf result (concatenate 'string result (string ch))))))
    result))


(defun eval-expr (expr)
    (cond

    ((numberp expr)
        expr)
    (
        (and (listp expr) (eql (car expr) '+))
        (+ (eval-expr (findValue(second expr) vars)) (eval-expr (findValue (third expr) vars)))
    )
    (
        (and (listp expr) (eql (car expr) '-)) 
        (- (eval-expr (findValue(second expr) vars)) (eval-expr (findValue (third expr) vars)))
    )
    (
        (and (listp expr) (eql (car expr) '*))
        (* (eval-expr (findValue(second expr) vars)) (eval-expr (findValue (third expr) vars)))
    )
    (
        (and (listp expr) (eql (car expr) '/))
        (/ (eval-expr (findValue(second expr) vars)) (eval-expr (findValue (third expr) vars)))
    )

    (
        (and (listp expr) (eql (car expr) 'gt))
        (> (eval-expr (findValue(second expr) vars)) (eval-expr (findValue (third expr) vars)))
    )

    (
        (and (listp expr) (eql (car expr) 'eq))
        (equal (eval-expr (findValue(second expr) vars)) (eval-expr (findValue (third expr) vars)))
    )

    (t
        nil)
    )
)



(defun eval-logic (expr)
  (cond
    ((symbolp expr)
     expr)

    (
        (and (listp expr) (eql (car expr) 'and))
        (and (replace-T-NIL(eval-logic (second expr))) (replace-T-NIL(eval-logic (third expr))))
     
    )
   
    (
        (and (listp expr) (eql (car expr) 'not))
        (not (replace-T-NIL(eval-logic (second expr))))
    )
    (
        (and (listp expr) (eql (car expr) 'or))
        (or (replace-T-NIL(eval-logic (second expr))) (replace-T-NIL(eval-logic (third expr))))
    )

    (t
     nil))
)


(defun decideOperation (expr)

    (cond

        (
            (or (and (listp expr) (eql (car expr) 'and)) (and (listp expr) (eql (car expr) 'not)) (and (listp expr) (eql (car expr) 'or)))
            (print(eval-logic expr))
        )

        (
            (or (and (listp expr) (eql (car expr) '+)) (and (listp expr) (eql (car expr) '-)) (and (listp expr) (eql (car expr) '*)) (and (listp expr) (eql (car expr) '/)) (and (listp expr) (eql (car expr) 'gt)) (and (listp expr) (eql (car expr) 'eq)))
            (print(eval-expr expr))
        )
        (
            (or (eq (nth 0 input) 'defv) (eq (nth 0 input) 'set))
            (defv-set expr)
        )
        (
            (eql (car expr) 'if)
            (for-if expr)
        )
        (
            (eql (car expr) 'while)
            (for-while expr)
        )
    )
)


(defun replace-true-false (input)
  (cond
    ((eq input 'true) 1)
    ((eq input 'false) 0)
    (t input)))

(defun transform-input (input)
  (cond
    ((atom input) (replace-true-false input))
    (t (mapcar #'transform-input input))))



(defun replace-T-NIL (input)
  (cond
    ((equal input 'true) 't)
    ((equal input 'false) 'nil)
    (t input)))

(defun transform-TNil (input)
  (cond
    ((atom input) (replace-T-NIL input))
    (t (mapcar #'transform-TNil input))))


(defstruct key-value
    key
    value
)


(defun findValue (key key-value-list)
    (if (numberp key) key
    (loop for kv in key-value-list
        when (equal (key-value-key kv) key)
        return (key-value-value kv)))
)



(defun changeValue (key new-value key-value-list)
  (loop for kv in key-value-list
        when (equal (key-value-key kv) key)
        do (setf (key-value-value kv) new-value))
)



(setq vars '())
(defun defv-set (input)
    (if (eq (nth 0 input) 'defv)
        (if (numberp (nth 2 input))
            (setf vars (append vars (list (make-key-value :key (nth 1 input) :value (nth 2 input)))))
            (setf vars (append vars (list (make-key-value :key (nth 1 input) :value (eval-expr(nth 2 input))))))
        )
    )
    (if (eq (nth 0 input) 'set)
        (if (numberp (nth 2 input)) 
            (changeValue (nth 1 input) (nth 2 input) vars)
            (changeValue (nth 1 input) (eval-expr(nth 2 input)) vars)
        )
    )
    (print vars)
)

(defun for-if (input)
    (if (not (eq (nth 0 input) 'if))
        "ERROR"
    )
    (if (eval-logic (nth 1 input))
        (explist (nth 2 input))
        (explist (nth 3 input))
    )
)

(defun for-while (input)
    (if (not (eq (nth 0 input) 'while))
        "ERROR")
    
    (print (nth 1 input))
    (print (nth 2 input))
    (loop while (decideoperation (nth 1 input))
       do 
        (explist (nth 2 input))
    )
    
) 

(defun explist (input)
    (setq len (length input))
    (setq i 0)
    (loop while (< i len)
    do  (decideoperation (nth i input))
    (setf i (+ i 1))
    (when (= i len) (return)))
)

(defun gppinterpreter()

          
    (setq input (read-line)) ;; Taking input from            
    (loop while (string/= input "")
        do 
            (setf input (replace-f-with-slash input))
            (setf input (read-from-string input))
            (decideoperation input)
            (terpri)

            (setf input (read-line))
    )
                        
    ;(print input)
    ;(parse_string readFile)
    ;(print newList)
    ;newList will be used;
    ;(setf input (transform-input (read-from-string input)))
    ;(print input)
    
   
    ;(classify newList)
    ;(print resultList)
    ;(elifmethod newList)
    ;(print newList_2)
    ;(findString resultList)
    ;(print resultList)
    ;(setf llist resultList)
)
                


(defun main()


    (gppinterpreter)
     
)
(main)