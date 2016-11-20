;;; A BFTape is a [ListOf Numbers]
;;; A BFState is a (make-bf ptr BFTape)
(define-struct bf [ptr tape])
;;; A ptr is a Number
;;; A BFCommand is one of:
;;;   - 'ip
;;;   - 'dp
;;;   - 'ib
;;;   - 'db
;;;   - 'out
;;;   - 'in
;;;   - 'jf
;;;   - 'jb

;;; A [Or X Y Z] is one of:
;;;   - X
;;;   - Y
;;;   - Z

;;; Symbol [ListOf BFCommand] Number -> Number
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

(check-expect (get-new-ptr 'jf (list 'ib 'ib 'jf 'ib 'jb) 2)
              4)
(check-expect (get-new-ptr 'jf (list 'ib 'ib 'jf 'ib 'jb) 0)
              4)
(check-expect (get-new-ptr 'jb (list 'ib 'ib 'jf 'ib 'jb) 4)
              2)
(check-expect (get-new-ptr 'jb (list 'ib 'ib 'jf 'jf 'ib 'jb) 4)
              3)

;;; BFState [Number -> Number]
(define apply-op-to-byte-at-ptr (lambda (bfstate func)
                                  (local [(define ptr (bf-ptr bfstate))
                                          (define tape (bf-tape bfstate))]
                                    (make-bf ptr
                                             (map (lambda (index) (if (equal? index ptr)
                                                                      (modulo (func (list-ref tape index)) 256)
                                                                      (list-ref tape index)))
                                                  (build-list (length tape) (lambda (x) x)))))))

(check-expect (apply-op-to-byte-at-ptr bfstate add1)
              (make-bf 1 (list 0 2 0 0)))
(check-expect (apply-op-to-byte-at-ptr bfstate sub1)
              (make-bf 1 (list 0 0 0 0)))
(check-expect (apply-op-to-byte-at-ptr bfstate (lambda (x) x))
              bfstate)
(check-expect (apply-op-to-byte-at-ptr (make-bf 0 empty) add1)
              (make-bf 0 empty))

;;; BFCommand BFState -> (list [Or BFState 'jf 'jb] String)
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

(define bfstate (make-bf 1 (list 0 1 0 0)))
(check-expect (run-command 'ip bfstate)
              (list (make-bf 2 (list 0 1 0 0)) ""))
(check-expect (run-command 'dp bfstate)
              (list (make-bf 0 (list 0 1 0 0)) ""))
(check-expect (run-command 'ib bfstate)
              (list (make-bf 1 (list 0 2 0 0)) ""))
(check-expect (run-command 'db bfstate)
              (list (make-bf 1 (list 0 0 0 0)) ""))
;(check-expect (run-command 'out bfstate) (list bfstate ""))
;(check-expect (run-command 'in bfstate) (list bfstate ""))
(check-expect (run-command 'jf (make-bf 0 (list 0 1)))
              (list 'jf ""))
(check-expect (run-command 'jf (make-bf 1 (list 0 1)))
              (list (make-bf 1 (list 0 1)) ""))
(check-expect (run-command 'jb (make-bf 0 (list 1 0)))
              (list 'jb ""))
(check-expect (run-command 'jb (make-bf 1 (list 1 0)))
              (list (make-bf 1 (list 1 0)) ""))

;;; [ListOf BFCommand] BFState Number [ListOf String]-> BFState
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

(check-expect (bf-eval (list 'ip 'ib 'ib)
                       (make-bf 0 (list 0 0 0))
                       0
                       empty)
              (list "" "" ""))
(check-expect (bf-eval (list 'dp 'db 'db)
                       (make-bf 2 (list 0 10 0))
                       0
                       empty)
              (list "" "" ""))

;;; A string 1 is a string of length 1
;;; String1 -> BFCommand
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

;;; String -> [ListOf BFCommand]
(define convert (lambda (bfstr)
                  (map convert-char (explode bfstr))))


;;; [ListOf X] Number -> [ListOf X]
(define list-tail (lambda (lox num)
                    (cond [(equal? num 0) lox]
                          [else (list-tail (rest lox) (sub1 num))])))

(check-expect (list-tail (list 1 2 3) 2) (list 3))

;;; [ListOf X] Number -> [ListOf X]
(define list-head (lambda (lox num)
                    (cond [(equal? num 0) empty]
                          [else (cons (first lox) (list-head (rest lox)
                                                             (sub1 num)))])))

(check-expect (list-head (list 1 2 3) 2) (list 1 2))

;;; [ListOF X] X -> Number
(define indexof-last (lambda (lox x)
                       (sub1 (- (length lox) (indexof-first (reverse lox) x 0)))))

(check-expect (indexof-last (list "a" "b" "c" "a") "a") 3)
(check-expect (indexof-last (list "a" "b" "c") "a") 0)

;;; [ListOF X] X -> Number
(define indexof-first (lambda (lox x acc)
                        (cond [(empty? lox) (error 'indexof "Given empty list")]
                              [(empty? (first lox)) (error 'indexof "Couldn't find element " x " in the list.")]
                              [(equal? (first lox) x) acc]
                              [else (indexof-first (rest lox) x (add1 acc))])))

(check-expect (indexof-first (list 'a 'b 'c 'd 'a) 'a 0) 0)
(check-expect (indexof-first (list 'a 'b 'c 'd) 'd 0) 3)

;;; String -> BFState
(define run (lambda (str)
              (list->string (reverse (filter (lambda (x) (not (equal? x "")))
                                             (bf-eval (convert str)
                                                      (make-bf 500 (build-list 1000
                                                                               (lambda (x) 0)))
                                                      0
                                                      empty))))))

;;; Hello World: 
(run "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")
