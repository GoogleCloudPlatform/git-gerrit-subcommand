;; Copyright 2023 Google LLC
;; Author: Jun Sheng
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     https://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.


#lang racket

(provide and-let*-values
	 and-let*-values-check
	 and-let*-tag-values-check)

;; (and-let*-values
;;    (((bind0 ... bindn) (values ...))
;;     ...)
;;  body ...
;;  (else ...)
;; Bind values to bind0 to bindn and check if bind0 is true,
;; then execute next binding-set and repeatly check the first bind
;; if all success, body is executed, else the else is executed.
      


(define-syntax and-let*-values
  (lambda (stx)
    (syntax-case stx (else)
      ((_ (?bind0 ?bind1 ...) ?body0 ... (else ?expr0 ...))
       (syntax-case #'?bind0 ()
	 (((?b0 ?b1 ...) ?exp)
	  (identifier? #'?b0)
	  #`(let-values (?bind0)
	      (if ?b0
		  (and-let*-values (?bind1 ...) ?body0 ... (else ?expr0 ...))
		  (begin ?expr0 ...))))))
      ((_ () ?body0 ... (else ?expr0 ...))
       #'(begin ?body0 ...))
      ((_ (?bind0 ...) ?body0 ...)
       #'(and-let*-values (?bind0 ...) ?body0 ... (else #f))))))

;; (and-let*-values-check
;;    (((val bind1 ... bindn) (values ...))
;;     ...)
;;  body ...
;;  (else ...)
;; Check if first value equal? to val, if true,
;; bind the rest of values to bind1 to bindn 
;; then execute next binding-set and repeatly check the first value
;; if all success, body is executed, else the else is executed.

(define-syntax and-let*-values-check
  (lambda (stx)
    (syntax-case stx (else)
      ((kw (?bind0 ?bind1 ...) ?body0 ... (else ?expr0 ...))
       (syntax-case #'?bind0 ()
	 (((?b0 ?b1 ...) ?exp)
	  (let* ((?check-b0 (car (generate-temporaries (list #'?b0)))))
	    #`(let-values (((#,?check-b0 ?b1 ...) ?exp))
		(if (equal? #,?check-b0 ?b0)
		    (kw (?bind1 ...) ?body0 ... (else ?expr0 ...))
		    (begin ?expr0 ...)))))))
      ((_ () ?body0 ... (else ?expr0 ...))
       #'(begin ?body0 ...))
      ((kw (?bind0 ...) ?body0 ...)
       #'(kw (?bind0 ...) ?body0 ... (else #f))))))


;; (and-let*-values-check
;;    (((val bind1 ... bindn) tag (values ...))
;;     ...)
;;  body ...
;;  (else proc)
;; Check if first value equal? to val, if true bind the rest of values
;; to bind1 to bindn,
;; then execute next binding-set and repeatly check the first value,
;; if all success, body is executed, else the proc is executed with tag
;; as argument.


(define-syntax and-let*-tag-values-check
  (lambda (stx)
    (syntax-case stx (else)
      ((kw (?bind0 ?bind1 ...) ?body0 ... (else ?proc))
       (syntax-case #'?bind0 ()
	 (((?b0 ?b1 ...) ?tag ?exp)
	  (let* ((?check-b0 (car (generate-temporaries (list #'?b0)))))
	    #`(let-values (((#,?check-b0 ?b1 ...) ?exp))
		(if (equal? #,?check-b0 ?b0)
		    (kw (?bind1 ...) ?body0 ... (else ?proc))
		    (?proc ?tag)))))))
      ((_ () ?body0 ... (else ?proc))
       #'(begin ?body0 ...))
      ((kw (?bind0 ...) ?body0 ...)
       #'(kw (?bind0 ...) ?body0 ... (else (lambda (t) #f)))))))
