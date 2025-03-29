#lang racket

; Created by prof, provided for assignment as best I can tell
(define prompt?
   (let [(args (current-command-line-arguments))]
     (cond
       [(= (vector-length args) 0) #t]
       [(string=? (vector-ref args 0) "-b") #f]
       [(string=? (vector-ref args 0) "--batch") #f]
       [else #t])))

; Checks if given character is any in list
(define (is-any ch lst)
  (cond
    [(null? lst) #f]
    [(char=? ch (car lst)) #t]
    [else (is-any ch (cdr lst))]))

; Converts char to string, gives error if not
(define (string-char str)
  (if (equal? (string-length str) 1)
    (string-ref str 0)
    (error "Error: Not a valid input")))

; get length
(define (my_length lst)
  (if (pair? lst)
    2
    (length lst)))

; reverse
(define (my_reverse lst)
  (if (pair? lst)
    (cons (last lst) (first lst))
    (reverse lst)))

; Checks if char is one of our operators (+, -, *, /)
(define (operator? ch) (if (equal? (string-length ch) 1)
  (is-any (string-char ch) '(#\+ #\- #\* #\/))
  #f))
; Checks if element is a number
(define (is-number? str)
  (if (char-numeric? (string-ref str 0))
    (if (> (string-length str) 1)
      (is-number? (substring str 1 (string-length str)))
      #t)
    #f))
; Checks if element is  reference to prior output
(define (prev_ref? str) (if (equal? (string-ref str 0) #\$)
  (if (is-number? (substring str 1 (string-length str)))
    #t
    #f)
  #f))

; Check if is a valid element
(define (valid_element? str)
  (if (prev_ref? str) #t
    (if (is-number? str) #t
      (if (operator? str) #t #f))))

; Validate the whole line
(define (validate_line_elements line_lst) (andmap valid_element? line_lst))

; Split the line properly
(define (split_line line)
  (filter non-empty-string?
    (string-split 
       (string-replace (
        string-replace (
        string-replace (
        string-replace (
        string-replace line
          "+" " + ")
          "-" " - ")
          "*" " * ")
          "/" " / ")
          "$" " $") " ")))

; get the reference val
(define (get_ref ref line_lst)
  (if (< (string->number (substring ref 1 (string-length ref)) (my_length line_lst)))
    (string->number (substring ref 1 (string-length ref)))
    (error "Error: Invalid reference to previous result")))

; Replace the $num instances with values
(define (replace_refs current_line line_lst)
  (if (prev_ref? (car current_line))
    (append (get_ref (car current_line) (replace_refs (cdr current_line))))
    (append (car current_line) (replace_refs (cdr current_line)))))

; get last 2 elements
(define (last_two numbers) (list (car (my_reverse numbers)) (car (cdr (my_reverse numbers)))))
(define (replace_last_two numbers new_val) (append (my_reverse (cdr (cdr (my_reverse numbers)))) new_val))

; Apply the operators
(define (apply_operator operator numbers)
  (cond
    [(equal? operator "+") (replace_last_two numbers (+ (car (last_two numbers)) (cdr (last_two numbers))))]
    [(equal? operator "-") (replace_last_two numbers (- (car (last_two numbers)) (cdr (last_two numbers))))]
    [(equal? operator "*") (replace_last_two numbers (* (car (last_two numbers)) (cdr (last_two numbers))))]
    [(equal? operator "/") (replace_last_two numbers (/ (car (last_two numbers)) (cdr (last_two numbers))))]))

; Evaluate recursively
(define (eval_recurs current_line numbers)
  (displayln numbers)
  (if (empty? current_line)
    (if (equal? (my_length (list numbers)) 1)
      (car numbers)
      (error "Error: More numbers than operators"))
    (if (is-number? (car current_line))
      (eval_recurs (cdr current_line) (append numbers (list (real->double-flonum (string->number (car current_line))))))
      (if (>= (my_length numbers) 2)
        (eval_recurs (cdr current_line) (apply_operator (car current_line) numbers))
        (error "Error: Not enough numbers in stack")))))

; Evaluate the current line
(define (eval_line current_line line_lst)
  (eval_recurs (reverse current_line) '()))

; Get user input
(define (line_input line_lst)
  (if (not (empty? line_lst)) (displayln (car (reverse (list line_lst)))) (display ""))
  (when prompt?
    (display "Prefix Operation: "))
  (define input_line (read-line (current-input-port) 'any))
  (if (not (equal? input_line "quit"))
      (line_input
        (append line_lst
          (if (validate_line_elements (split_line input_line))
              (list (eval_line (split_line input_line) line_lst))
              (error "Error: Improper line syntax"))))
      line_lst
  ))


(line_input '())
