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
    (error "Not a char")))

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

(define (valid_element? str)
  (if (prev_ref? str) #t
    (if (is-number? str) #t
      (if (operator? str) #t #f))))

(define (validate_line_elements line_lst) (andmap valid_element? line_lst))

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

(define (line_input line_lst)
  (when prompt?
    (display "Prefix Operation: "))
  (define input_line (read-line (current-input-port) 'any))
  (if (not (equal? input_line "quit"))
      (line_input (append line_lst (list (validate_line_elements (split_line input_line)))))
      line_lst
  ))


(line_input '())
