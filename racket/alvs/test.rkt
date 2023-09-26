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

(require "and-let-values.rkt")

(module+ test
 (and-let*-values-check
  [(('("t12" 13 #t) c1)
    (values
     (and-let*-values
      (((t11 t12 t13 ) (values #t "t12" 13))
       ((t21 t22) (values #t #t)))
      (list t12 t13 t22))
     ;;=> ("t12" 13 #t)
     "testcase1"))
   ((#f c2)
    (values 
     (and-let*-values
      (((t11 t12 t13 ) (values #f "t12" 13))
       ((t21 t22) (values #t #t)))
      (list t12 t13 t22))
     ;;=> #f
     "testcase2"))
   ((#f c3)
    (values
     (and-let*-values-check
      (((12 t11) (values 12 "foo"))
       ((12 t21) (values 13 "bar")))
      (cons t11 t21))
     ;;=> #f
     "testcase3"))
   (('("foo" "bar" "baz" "t32baz") c4)
    (values
     (and-let*-values-check
      ((('ok t11) (values 'ok "foo"))
       (('ok t21) (values 'ok "bar"))
       (('ok t31 t32) (values 'ok "baz" "t32baz")))
      (list t11 t21 t31 t32))
     ;;=> ("foo" "bar" "baz" "t32baz")
     "testcase4"))
   (('("foo" . "bar") c5)
    (values
     (and-let*-values-check
      ((('ok t11) (values 'ok "foo"))
       (('ok t21) (values 'ok "bar")))
      (cons t11 t21))
     ;;=> ("foo" . "bar")
     "testcase5"))
   (('("foo" . "bar") c6)
    (values
     (and-let*-values-check
      (((#f t11) (values #f "foo"))
       ((#f t21) (values #f "bar")))
      (cons t11 t21))
     ;;=> ("foo" . "bar")
     "testcase6"))
   (('("foo" . "bar") c7)
    (values
     (let ((va 1)
	   (vb 2))
       (and-let*-values-check
	(((va t11) (values 1 "foo"))
	 ((vb t21) (values 2 "bar")))
	(cons t11 t21)))
     ;;=> ("foo" . "bar")
     "testcase7"))
   (('("foo" . "bar") c8)
    (values
     (let ((va 1))
       (and-let*-values-check
	(((va t11) (values 1 "foo"))
	 ((t11 t21) (values "foo" "bar")))
	(cons t11 t21)))
     ;;=> ("foo" . "bar")
     "testcase8"))
   (('stillpass c9)
    (values
     (let ((va 1))
       (and-let*-values-check
	(((va t11) (values 2 "foo"))
	 ((t11 t21) (values "foo" "bar")))
	(cons t11 t21)
	(else 'stillpass)))
     ;;=> stillpass
     "testcase9"))]
  (display
   (format "All these tests:\n ~s \n are finished successfully \n" 
	   (list c1 c2 c3 c4 c5 c6 c7 c8 c9))))

 (and-let*-tag-values-check
  ((('bar c10)
    'c10
    (values
     (and-let*-tag-values-check
      (((12 t11) 'foo (values 12 "foo"))
       ((12 t21) 'bar (values 13 "bar")))
      (cons t11 t21)
      (else (lambda (t)
	      t)))
     ;;=> #f
     "testcase10"))
   (('foo c11)
    'c11
    (values
     (and-let*-tag-values-check
      (((12 t11) 'foo (values 13 "foo"))
       ((12 t21) 'bar (values 12 "bar")))
      (cons t11 t21)
      (else (lambda (t)
	      t)))
     ;;=> #f
     "testcase11")))
  (display
   (format "test cases ~a are finished successfully\n"
	   (list c10 c11)))
  (else (lambda (t)
	  (display
	   (format "test failed at ~a\n" t))))))


