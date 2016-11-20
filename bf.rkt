(define-struct bf [ptr tape])

(define get-new-ptr (lambda (sym lobfc ptr)
                      (local [(define ptr-new
                                (cond [(equal? sym 'jf)
                                       (indexof-first (list-tail lobfc ptr)
                                                      'jb
                                                      ptr)]
                                      [(equal? sym 'jb)
                                       (indexof-last (list-head lobfc ptr)
                                                     'jf)]))]
                        (begin (display sym) (display " caused moving from ") (display ptr) (display " to ") (display ptr-new) (display "\n") ptr-new))))

(define apply-op-to-byte-at-ptr (lambda (bfstate func)
                                  (local [(define ptr (bf-ptr bfstate))
                                          (define tape (bf-tape bfstate))]
                                    (make-bf ptr
                                             (map (lambda (index) (if (equal? index ptr)
                                                                      (modulo (func (list-ref tape index)) 256)
                                                                      (list-ref tape index)))
                                                  (build-list (length tape) (lambda (x) x)))))))

(define run-command (lambda (command bfstate)
                      (cond [(equal? 'ip command)
                             (list (make-bf (add1 (bf-ptr bfstate))
                                            (bf-tape bfstate))
                                   "")]
                            [(equal? 'dp command)
                             (list (make-bf (sub1 (bf-ptr bfstate))
                                            (bf-tape bfstate))
                                   "")]
                            [(equal? 'ib command)
                             (list (apply-op-to-byte-at-ptr bfstate add1) "")]
                            [(equal? 'db command)
                             (list (apply-op-to-byte-at-ptr bfstate sub1) "")]
                            [(equal? 'out command)
                             (list (begin (display (integer->char (list-ref (bf-tape bfstate) (bf-ptr bfstate))))
                                          bfstate)
                                   (integer->char (list-ref (bf-tape bfstate) (bf-ptr bfstate))))]
                            [(equal? 'in command)
                             (list (apply-op-to-byte-at-ptr bfstate (lambda (x) (read)))
                                   "")]
                            [(equal? 'jf command)
                             (if (equal? (list-ref (bf-tape bfstate) (bf-ptr bfstate))
                                         0)
                                 ;; If it is 0, then jump forward past the next 'jb
                                 (list 'jf "")
                                 ;; Else, do nothing:
                                 (list bfstate ""))]
                            [(equal? 'jb command)
                             (if (equal? (list-ref (bf-tape bfstate) (bf-ptr bfstate))
                                         0)
                                 ;; If it is 0, do nothing
                                 (list bfstate "")
                                 ;; Else, jump backward to the previous 'jf
                                 (list 'jb ""))])))

(define bf-eval (lambda (lobfc bfstate exec-ptr bf-out)
                  (cond [(equal? exec-ptr (length lobfc)) bf-out]
                        [else (local [(define currentCommand (list-ref lobfc exec-ptr))
                                      (define output (run-command currentCommand bfstate))
                                      (define next-ptr
                                        (if (not (bf? (first output)))
                                            (get-new-ptr (first output) lobfc (add1 exec-ptr))
                                            (add1 exec-ptr)))
                                      (define next-bfstate
                                        (if (bf? (first output))
                                            (first output)
                                            bfstate))]
                                (bf-eval lobfc next-bfstate next-ptr (cons (second output)
                                                                        bf-out)))])))

(define convert-char (lambda (char)
                       (cond [(equal? char ">") 'ip]
                             [(equal? char "<") 'dp]
                             [(equal? char "+") 'ib]
                             [(equal? char "-") 'db]
                             [(equal? char ".") 'out]
                             [(equal? char ",") 'in]
                             [(equal? char "[") 'jf]
                             [(equal? char "]") 'jb]
                             [else (error 'convert "Failed to convert because " char " is not a valid BF command.")])))

(define convert (lambda (bfstr)
                  (map convert-char (explode bfstr))))

(define list-tail (lambda (lox num)
                    (cond [(equal? num 0) lox]
                          [else (list-tail (rest lox) (sub1 num))])))

(define list-head (lambda (lox num)
                    (cond [(equal? num 0) empty]
                          [else (cons (first lox) (list-head (rest lox)
                                                             (sub1 num)))])))

(define indexof-last (lambda (lox x)
                       (sub1 (- (length lox) (indexof-first (reverse lox) x 0)))))

(define indexof-first (lambda (lox x acc)
                        (cond [(empty? lox) (error 'indexof "Given empty list")]
                              [(empty? (first lox)) (error 'indexof "Couldn't find element " x " in the list.")]
                              [(equal? (first lox) x) acc]
                              [else (indexof-first (rest lox) x (add1 acc))])))

(define run (lambda (str)
              (list->string (reverse (filter (lambda (x) (not (equal? x "")))
                                             (bf-eval (convert str)
                                                      (make-bf 500 (build-list 1000
                                                                               (lambda (x) 0)))
                                                      0
                                                      empty))))))

(run "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")