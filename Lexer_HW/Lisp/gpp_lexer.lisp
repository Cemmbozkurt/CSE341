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
            (>= end (length input))  ;; if last >= input return
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
                ;(print (subseq input first end))
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



(defun gppinterpreter()

            (cond
                (   (not(null *args*))
                    (setq readFile (readFile (elt *args* 0)))
               
                   
                )

                (t
                    
                    (setq input (read-line)) ;; Taking input from
                    (parse_string input)
                )
            ) 
                        
        
        

    ;(print input)
    ;(parse_string readFile)
    ;(print newList)
    (classify newList)
    ;(print resultList)
    (findString resultList)
    (print resultList)
)
                


(defun main()

	(gppinterpreter)
)
(main)