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
;(current-print void)
(require "scsh-repf.rkt")

(module+ test
  (define (test)
    (if
     (equal? '("hello") (run/strings (racket -t scsh-repf.rkt) (<< '(run (echo "hello")))))
     (display "test 1 passed\n")
     (exit 1))
    (let*-values (((status out) (run/status+strings (sed "s/abc/ABC") (<< "abc"))))
      (if (eq? 0 status)
	  (exit 1)
	  (display "test 2 passed\n"))
      (display
       (format " out is ~a\n status is ~a\n" out status)))
    (let ((cmd "sed"))
      (let*-values (((status out )
		     (run/status+strings (,cmd "s/abc/ABC/") (<< "abc"))))
	(if (eq? 0 status)
	    (display "test 3 passed\n")
	    (exit 1))
	(display
	 (format " out is ~a\n status is ~a\n" out status)))
      )
    (void))
  (test))

(module+ main
  (define (main)
   (run (ls))
   (void))
  (main))



;; (find-system-path 'run-file)



