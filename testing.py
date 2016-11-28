import unittest

from racython import runRexp, runFile, stripSemiColonComments, stripComments, stripMLComments, eval, topLevelEnv, closure, apply
import racython

class TestInterpreterInternals(unittest.TestCase):

    def test_parseRExpr(self):
        self.assertEqual(racython.parseRExpr("(\"Hello World!\")"), ["\"Hello World!\""])
        self.assertEqual(racython.parseRExpr("(5)"), [5])
        self.assertEqual(racython.parseRExpr("(5.0)"), [5.0])
        self.assertEqual(racython.parseRExpr("(\"5\" \"Hello World!\")"), [5, "\"Hello World!\""])
        self.assertEqual(racython.parseRExpr("((lambda (x) (func (+ 1 1) 5)) 2)"),
                         [['lambda', ['x'], ['func', ['+', 1, 1], 5]], 2])

    def test_parseAtom(self):
        self.assertEqual(racython.parseAtom("5"), 5)
        self.assertEqual(racython.parseAtom("5.0"), 5.0)
        self.assertEqual(racython.parseAtom("Hello World!"), "\"Hello World!\"")

    def test_stripComments(self):
        self.assertEqual(stripSemiColonComments("before;comment", False), ("before", False))
        self.assertEqual(stripSemiColonComments("\"inString;stillInString\";comment", False), ("\"inString;stillInString\"", False))
        self.assertEqual(stripSemiColonComments("test;;test", False), ("test", False))
        self.assertEqual(stripComments(["test;test", "\"test;test\";comment", "\"testMultiLine;part", "part2;part3\";comment"]),
                         ["test", "\"test;test\"", "\"testMultiLine;part", "part2;part3\""])
        self.assertEqual(stripMLComments("(define #|test|# a 5)", False), ("(define  a 5)",False))
        self.assertEqual(stripMLComments("(define test|# a 5)", True), (" a 5)", False))
        self.assertEqual(stripComments(["(define a 5) #| firstComment", "restComment |# (define b 4)"]),
                                       (["(define a 5) ", " (define b 4)"]))

    def test_eval(self):
        self.assertEqual(eval(5, topLevelEnv)[0], 5)  # integers
        self.assertEqual(eval(5.0, topLevelEnv)[0], 5.0)  # floats
        self.assertEqual(eval("\"Hello World\"", topLevelEnv)[0], "Hello World")  # strings
        self.assertEqual(eval("#true", topLevelEnv)[0], True)  # Booleans work via looking them up in the env
        self.assertEqual(eval(["quote", 1, 2, 3], topLevelEnv)[0], [1, 2, 3])  # quote works with numbers
        self.assertEqual(eval(["quote", "a", "b", "c"], topLevelEnv)[0], ["a", "b", "c"])  # and with strings/symbols
        self.assertEqual(eval(["if", ["equal?", 1, 1],
                                     ["+", 1, 1],
                                     ["-", 1, 1]],
                              topLevelEnv)[0],
                         2)
        self.assertEqual(eval(["if", ["equal?", 1, 2],
                               ["+", 1, 1],
                               ["-", 1, 1]],
                              topLevelEnv)[0],
                         0)
        self.assertEqual(eval(["cond", [["equal?", 1, 1], ["+", 1, 1]],
                                       [["else", 0]]],
                              topLevelEnv)[0],
                         2)
        self.assertEqual(eval(["cond", [["equal?", 1, 0], ["+", 1, 1]],
                                       ["else", 0]],
                              topLevelEnv)[0],
                         0)
        self.assertEqual(isinstance(eval(["lambda", ["x"], ["+", 1, "x"]], topLevelEnv)[0], closure), True)
        self.assertEqual(eval(["local", [["define", "a", 5]], ["+", "a", 5]], topLevelEnv)[0], 10)

    def test_apply(self):
        def add(x,y):
            return x+y
        self.assertEqual(apply(add, [1, 2])[0], 3)
        addClosure = eval(["lambda", ["x", "y"], ["+", "x", "y"]], topLevelEnv)[0]
        self.assertEqual(apply(addClosure, [1, 2])[0], 3)

class LargeRacketInterpreterTests(unittest.TestCase):
    def test_myLength(self):
        self.assertEqual(runFile(""";;; [ListOf X] -> Number
                                    (define my-length (lambda (lox)
                                        (cond [(empty? lox) 0]
                                              [else (+ 1 (my-length (rest lox)))])))

                                    (check-expect (my-length empty) 0)
                                    (check-expect (my-length (list 1)) 1)
                                    (check-expect (my-length (list 1 2)) 2)
                                    (check-expect (my-length (list 1 2 3)) 3)
                                    (check-expect (my-length (list 1 2 3 4)) 4)"""), [None] * 6)

    def test_fib(self):
        self.assertEqual(runFile(""";;; Number -> Number
                                    (define fib (lambda (x)
                                      (cond [(equal? x 0) 0]
                                            [(equal? x 1) 1]
                                            [else (+ (fib (- x 2))
                                                     (fib (- x 1)))])))

                                    (check-expect (fib 0) 0)
                                    (check-expect (fib 1) 1)
                                    (check-expect (fib 2) 1)
                                    (check-expect (fib 3) 2)
                                    (check-expect (fib 4) 3)
                                    (check-expect (fib 5) 5)
                                    (check-expect (fib 12) 144)"""), [None] * 8)

    def test_treeSearch_v1(self):
        self.assertEqual(runFile(""";;; A [TreeOf X] is one of:
                                    ;;;   - (make-leaf X)
                                    ;;;   - (make-node [TreeOf X] X [TreeOf X])

                                    (define-struct leaf [val])
                                    (define-struct node [left val right])

                                    ;;; [TreeOf X] X -> Boolean
                                    (define tree-contains? (lambda (tree x)
                                      (cond [(leaf? tree) (equal? x (leaf-val tree))]
                                            [else (or (equal? x (node-val tree))
                                                      (tree-contains? (node-left tree) x)
                                                      (tree-contains? (node-right tree) x))])))

                                    (define t2.L (make-node (make-leaf 2) 3 (make-leaf 4)))
                                    (define t2.R (make-node (make-leaf 6) 7 (make-leaf 8)))
                                    (define t3 (make-node t2.L 5 t2.R))

                                    (check-expect (tree-contains? (make-leaf 5) 5) #true)
                                    (check-expect (tree-contains? (make-leaf 5) 6) #false)
                                    (check-expect (tree-contains? t3 5) #true)
                                    (check-expect (tree-contains? t3 2) #true)
                                    (check-expect (tree-contains? t3 3) #true)
                                    (check-expect (tree-contains? t3 4) #true)
                                    (check-expect (tree-contains? t3 6) #true)
                                    (check-expect (tree-contains? t3 7) #true)
                                    (check-expect (tree-contains? t3 8) #true)
                                    (check-expect (tree-contains? t3 9) #false)"""), [None] * 16)

    def test_treeSearch_v2(self):
        self.assertEqual(runFile(""";;; A [WeightedTreeOf X] is one of:
                                    ;;;   - (make-leaf X)
                                    ;;;   - (make-node [WeightedTreeOf X] X [WeightedTreeOf X])
                                    ;;; Interpretation: Everything to the left the center val is less
                                    ;;;                 than it and everything to the right is greater.

                                    (define-struct leaf [val])
                                    (define-struct node [left val right])

                                    ;;; [WeightedTreeOf X] X -> Boolean
                                    (define wtree-contains? (lambda (wtree x)
                                      (cond [(leaf? wtree) (equal? x (leaf-val wtree))]
                                            [(equal? x (node-val wtree)) #true]
                                            [(< x (node-val wtree))
                                             (wtree-contains? (node-left wtree) x)]
                                            [else (wtree-contains? (node-right wtree) x)])))

                                    (define t2.L (make-node (make-leaf 2) 3 (make-leaf 4)))
                                    (define t2.R (make-node (make-leaf 6) 7 (make-leaf 8)))
                                    (define t3 (make-node t2.L 5 t2.R))

                                    (check-expect (wtree-contains? (make-leaf 5) 5) #true)
                                    (check-expect (wtree-contains? (make-leaf 5) 6) #false)
                                    (check-expect (wtree-contains? t3 5) #true)
                                    (check-expect (wtree-contains? t3 2) #true)
                                    (check-expect (wtree-contains? t3 3) #true)
                                    (check-expect (wtree-contains? t3 4) #true)
                                    (check-expect (wtree-contains? t3 6) #true)
                                    (check-expect (wtree-contains? t3 7) #true)
                                    (check-expect (wtree-contains? t3 8) #true)
                                    (check-expect (wtree-contains? t3 9) #false)"""), [None] * 16)

class TestRacketInterpreter(unittest.TestCase):
    def test_basics(self):
        self.assertEqual(runRexp("(+ 1 1)"), 2)
        self.assertEqual(runRexp("(* 1 3)"), 3)
        self.assertEqual(runRexp("(- 3 2)"), 1)
        self.assertEqual(runRexp("(/ 9 3)"), 3)
        self.assertEqual(runRexp("(add1 5)"), 6)
        self.assertEqual(runRexp("(sub1 5)"), 4)
        self.assertEqual(runRexp("(modulo 11 2)"), 1)
        self.assertEqual(runRexp("(equal? 1 2)"), False)
        self.assertEqual(runRexp("(equal? 1 1)"), True)
        self.assertEqual(runRexp("(equal? (list 1 2) (list 1 2 3))"), False)
        self.assertEqual(runRexp("(equal? (list 1 2) (list 1 2))"), True)
        self.assertEqual(runRexp("(>= 3 2)"), True)
        self.assertEqual(runRexp("(>= 3 3)"), True)
        self.assertEqual(runRexp("(>= 3 4)"), False)
        self.assertEqual(runRexp("(> 3 2)"), True)
        self.assertEqual(runRexp("(< 3 3)"), False)
        self.assertEqual(runRexp("(<= 3 3)"), True)

    def test_lambda(self):
        self.assertEqual(runRexp("""((lambda (abs) (list (abs (- 5 7))
                                                         (abs (- 7 5))))
                                     (lambda (x) (if ( < x 0) (- 0 x) x)))"""), [2, 2])

    def test_stringManipulation(self):
        self.assertEqual(runRexp("Hello World!"), "Hello World!")
        self.assertEqual(runRexp("(integer->char 65)"), "A")
        self.assertEqual(runRexp("(list->string (list \"a\" \"b\" \"c\"))"), "abc")

    def test_control(self):
        self.assertEqual(runRexp("(cond ((> 5 0) 5) (else 6))"), 5)
        self.assertEqual(runRexp("(cond ((< 5 0) 5) (else 6))"), 6)
        self.assertEqual(runRexp("(if (> 5 0) 5 6)"), 5)
        self.assertEqual(runRexp("(if (< 5 0) 5 6)"), 6)

    def test_define(self):
        self.assertEqual(runRexp("(define abs (lambda (x) (if (< x 0) (- 0 x) x)))"), None)
        self.assertEqual(runRexp("(list (abs (- 7 5)) (abs (- 5 7)))"), [2, 2])

    def test_local(self):
        self.assertEqual(runRexp("(local ((define a 5)(define b 1)) (+ a b))"), 6)
        self.assertEqual(runRexp("(define abs (lambda (x) x))"), None) # To show that shadowing works in local
        self.assertEqual(runRexp("(local ((define abs (lambda (x) (if (< x 0) (- 0 x) x)))) (abs -2))"), 2)
        self.assertEqual(runRexp("(define abs (lambda (x) (local [(define flipped (- 0 x))] (if (< x 0) flipped x))))"),
                         None)
        self.assertEqual(runRexp("(abs 2)"), 2)
        self.assertEqual(runRexp("(abs -2)"), 2)
        self.assertEqual(runRexp("(define abs (lambda (x) (if (< x 0) (local [(define flipped (- 0 x))] flipped) x)))"),
                         None)
        self.assertEqual(runRexp("(abs 2)"), 2)
        self.assertEqual(runRexp("(abs -2)"), 2)

    def test_logic(self):
        self.assertEqual(runRexp("#true"), True)
        self.assertEqual(runRexp("#false"), False)
        self.assertEqual(runRexp("(not #true)"), False)
        self.assertEqual(runRexp("(not #false)"), True)
        self.assertEqual(runRexp("(and #true #true #true)"), True)
        self.assertEqual(runRexp("(or #true #true #true)"), True)
        self.assertEqual(runRexp("(and #true #false #true)"), False)
        self.assertEqual(runRexp("(or #true #false #true)"), True)
        self.assertEqual(runRexp("(or #false #false #false)"), False)

    def test_list_functions(self):
        self.assertEqual(runRexp("(list 1 2 3 4)"), [1, 2, 3, 4])
        self.assertEqual(runRexp("(cons 0 (list 1 2 3 4))"), [0, 1, 2, 3, 4])
        self.assertEqual(runRexp("(empty? empty)"), True)
        self.assertEqual(runRexp("(empty? (cons 0 empty))"), False)
        self.assertEqual(runRexp("(rest (list 1 2 3 4))"), [2, 3, 4])
        self.assertEqual(runRexp("(first (list 1 2 3 4))"), 1)
        self.assertEqual(runRexp("(second (list 1 2 3 4))"), 2)
        self.assertEqual(runRexp("(third (list 1 2 3 4))"), 3)
        self.assertEqual(runRexp("(reverse (list 1 2 3))"), [3, 2, 1])
        self.assertEqual(runRexp("(explode \"abcde\")"), ["a", "b", "c", "d", "e"])
        self.assertEqual(runRexp("(list-ref (list \"a\" \"b\" \"c\") 1)"), "b")
        self.assertEqual(runRexp("(build-list 5 (lambda (x) x))"), [0, 1, 2, 3, 4])
        self.assertEqual(runRexp("(build-list 5 (lambda (x) (* 2 x)))"), [0, 2, 4, 6, 8])

    def test_struct(self):
        self.assertEqual(runRexp("(define-struct boat (capacity passengers))"), None)
        self.assertEqual(runRexp("(define boat (make-boat 100 90))"), None)
        self.assertEqual(runRexp("(boat-capacity boat)"), 100)
        self.assertEqual(runRexp("(boat-passengers boat)"), 90)
        self.assertEqual(runRexp("(boat? boat)"), True)
        self.assertEqual(runRexp("(define-struct train (capacity passengers))"), None)
        self.assertEqual(runRexp("(define train (make-train 100 90))"), None)
        self.assertEqual(runRexp("(train-capacity train)"), 100)
        self.assertEqual(runRexp("(train-passengers train)"), 90)
        self.assertEqual(runRexp("(boat? train)"), False)
        self.assertEqual(runRexp("(train? train)"), True)
        self.assertEqual(runRexp("(train? boat)"), False)

    def test_loopingConstructs(self):
        self.assertEqual(runRexp("(map odd? (list 1 2 3 4 5))"), [True, False, True, False, True])
        self.assertEqual(runRexp("(filter odd? (list 1 2 3 4 5))"), [1, 3, 5])
        self.assertEqual(runRexp("(foldr + 0 (list 1 2 3 4))"), 1 + 2 + 3 + 4)
        self.assertEqual(runRexp("(foldl + 0 (list 1 2 3 4))"), 1 + 2 + 3 + 4)
        self.assertEqual(runRexp("(foldr / 1 (list 4 2))"), 4 / 2)
        self.assertEqual(runRexp("(foldl / 1 (list 4 2))"), 1 / 2 / 4)
        self.assertEqual(runRexp("(ormap odd? (list 0 2 4))"), False)
        self.assertEqual(runRexp("(ormap odd? (list 0 1 2 4))"), True)
        self.assertEqual(runRexp("(andmap even? (list 0 2 4))"), True)
        self.assertEqual(runRexp("(andmap even? (list 0 1 2 4))"), False)

    def test_begin(self):
        self.assertEqual(runRexp("(begin (+ 1 1))"), 2)
        self.assertEqual(runRexp("(begin (* 2 2) (+ 1 1))"), 2)
        self.assertEqual(runRexp("(begin (* 3 3) (/ 2 2) (+ 1 1))"), 2)

    def test_display(self):
        import sys
        from io import StringIO
        out = StringIO()
        sys.stdout = out
        runRexp("(display 1)")
        self.assertEqual(out.getvalue().strip(), "1")
        out = StringIO()
        sys.stdout = out
        runRexp("(display \"Hello\")")
        self.assertEqual(out.getvalue().strip(), "Hello")
        out = StringIO()
        sys.stdout = out
        runRexp("(display (list 1 2 3 4 5))")
        self.assertEqual(out.getvalue().strip(), "[1, 2, 3, 4, 5]")

    def test_quote(self):
        self.assertEqual(runRexp("(quote 1 2 3)"), [1,2,3])
        self.assertEqual(runRexp("(quote 1 2 3 a)"), [1, 2, 3, "a"])
        self.assertEqual(runRexp("(quote (quote 1 2 3))"), [["quote", 1, 2, 3]])

    def test_checkExpect(self):
        try:
            runRexp("(check-expect #true #false)")
        except AssertionError as e:
            self.assertEqual(e.args[0], "check-expect failed. True doesn't equal False")
        try:
            runRexp("(check-expect 1 2)")
        except AssertionError as e:
            self.assertEqual(e.args[0], "check-expect failed. 1 doesn't equal 2")
        try:
            runRexp("(check-expect (list 1 2 3) (list 1 2 3 4))")
        except AssertionError as e:
            self.assertEqual(e.args[0], "check-expect failed. [1, 2, 3] doesn't equal [1, 2, 3, 4]")
        self.assertEqual(runRexp("(check-expect #true #true)"), None)
        try:
            runRexp("(error \"testTitle\" \"msg1\" \"msg2\" \"msg3\")")
        except Exception as e:
            self.assertEqual(e.args[0], "testTitle msg1 msg2 msg3")

if __name__ == '__main__':
    unittest.main()