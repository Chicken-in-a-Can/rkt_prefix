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
  (if (number? lst)
    1
    (if (null? lst)
      0
      (+ 1 (my_length (cdr lst))))))

; reverse
(define (my_reverse lst)
  (cond
    [(equal? (my_length lst) 0) '()]
    [(equal? (my_length lst) 1) (car lst)]
    [(equal? (my_length lst) 2) (list (last lst) (car lst))]
    [else (reverse lst)]))

(define (next_to_last lst)
  (if (< (my_length lst) 2)
    (error "Error: List has too few elements for this operation")
    (if (equal? (my_length lst) 2)
      (car lst)
      (second (my_reverse lst)))))

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
    (if (equal? (string-ref str 0) #\.)
      (if (> (string-length str) 1)
        (is-number? (substring str 1 (string-length str)))
        #f)
      #f)))
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
  (if (<= (string->number (substring ref 1)) (my_length line_lst))
    (number->string (list-ref line_lst (- (string->number (substring ref 1)) 1)))
    (error "Error: Invalid reference to previous result")))

; Replace the $num instances with values
(define (replace_refs current_line line_lst)
  (if (empty? current_line)
    current_line
    (if (prev_ref? (car current_line))
      (cons (get_ref (car current_line) line_lst) (replace_refs (cdr current_line) line_lst))
      (cons (car current_line) (replace_refs (cdr current_line) line_lst)))))

; replace last 2 elements
(define (replace_last_two numbers new_val)
  (if (equal? (my_length numbers) 2)
    (append '() new_val)
    (list (my_reverse (cddr (my_reverse numbers))) new_val )))

; Apply the operators
(define (apply_operator operator numbers)
  (if (>= (my_length numbers) 2)
  (replace_last_two numbers (cond
    [(equal? operator "+") (+ (last numbers) (next_to_last numbers))]
    [(equal? operator "-") (- (last numbers) (next_to_last numbers))]
    [(equal? operator "*") (* (last numbers) (next_to_last numbers))]
    [(equal? operator "/") (/ (last numbers) (next_to_last numbers))]))
  (error "Error: Not enough numbers in stack to apply operator")))

; Evaluate recursively
(define (eval_recurs current_line numbers)
  (if (empty? current_line)
    (if (equal? (my_length numbers) 1)
      numbers
      (error "Error: Too few operators"))
    (if (is-number? (car current_line))
      (eval_recurs (cdr current_line) (append numbers (list (real->double-flonum (string->number (car current_line))))))
      (if (>= (my_length numbers) 2)
        (eval_recurs (cdr current_line) (apply_operator (car current_line) numbers))
        (error "Error: Not enough numbers in stack")))))

; Evaluate the current line
(define (eval_line current_line line_lst)
  (eval_recurs (reverse (replace_refs current_line line_lst)) '()))

; Get user input
(define (line_input line_lst)
  (if (not (empty? line_lst)) (displayln (last line_lst)) (display ""))
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
