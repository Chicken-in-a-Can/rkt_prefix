#lang racket

; Created by prof, provided for assignment as best I can tell
(define prompt?
   (let [(args (current-command-line-arguments))]
     (cond
       [(= (vector-length args) 0) #t]
       [(string=? (vector-ref args 0) "-b") #f]
       [(string=? (vector-ref args 0) "--batch") #f]
       [else #t])))

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
      (line_input (append line_lst (list (split_line input_line))))
      line_lst
  ))


(line_input '())
