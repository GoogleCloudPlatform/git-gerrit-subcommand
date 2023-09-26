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

;; repf: reduced extensible process form
;; - only "<<" redirection
;; - no process combinations
;; - only run, run/strings, run/strings+status, run/port+proc
;; - no begin form in run

#lang racket
(provide (all-defined-out))

(define scsh-trace
  (make-parameter #f))

(define (stringify thing)
  (cond ((string? thing) thing)
	((symbol? thing)
	 (symbol->string thing))
					;	((symbol? thing)
					;	 (list->string (map char-downcase
					;			    (string->list (symbol->string thing)))))
	((equal? thing -i)
	 "-i")
	((integer? thing)
	 (number->string thing))
	((boolean? thing)
	 (format "~a" thing))
	(else (error "Can only stringify strings, symbols, and integers."
		     thing))))

(define (run/open-process* path-args stdin-obj stdout-redir stderr-redir)
  (let*-values
      (((stdin-redir) (if stdin-obj
			  #t
			  #f))
       ((command) (find-executable-path (car path-args)))
       ((args) (cdr path-args))
       ((stdout-port)
	(if stdout-redir
	    #f
	    (current-output-port)))
       ((stderr-port)
	(if stderr-redir
	    #f
	    (current-error-port)))
       ((stdin-port)
	(if stdin-redir
	    #f
	    (current-input-port)))
       ((proc out in err) (apply subprocess `(,stdout-port
					      ,stdin-port
					      ,stderr-port
					      ,command
					      ,@args))))
    (begin
      (if (scsh-trace)
	  (display (format "~a :=>\n" path-args) (current-error-port))
	  (void))
      (if stdin-redir
	  (begin
	    (display stdin-obj in)
	    (close-output-port in))
	  #f)
      (if stderr-redir
	  (values proc out err)
	  (values proc out)))))

(struct failed-process-exception (pid status proc))

(define (wait proc)
  (let ([status
	 (begin
	   (subprocess-wait proc)
	   (subprocess-status proc))])
    (if (scsh-trace)
	(display (format "~a ~a\n" (if (eq? status 0) "+" "-") status)
		 (current-error-port))
	status)
    status))

(define-syntax run/open-process
  (syntax-rules ()
    ((_ repf stdin-obj stdout-redir)
     (let ((path-args
	    (map stringify (quasiquote repf))))
       (run/open-process* path-args stdin-obj stdout-redir #f)))))

(define-syntax run/open-process-with-error-port
  (syntax-rules ()
    ((_ repf stdin-obj stdout-redir)
     (let ((path-args
	    (map stringify (quasiquote repf))))
       (run/open-process* path-args stdin-obj stdout-redir #t)))))


(define-syntax run
  (syntax-rules (<<)
    ((_ repf)
     (let-values (((proc out) (run/open-process repf #f #f)))
       (wait proc)))
    ((_ repf (<< schemeobj))
     (let-values (((proc out) (run/open-process repf schemeobj #f)))
       (wait proc)))))



(define-syntax run/proc+port
  (syntax-rules (<<)
    ((_ repf)
      (run/open-process repf #f #t))
    ((_ repf (<< schemeobj))
      (run/open-process repf schemeobj #t))))

(define-syntax run/proc+port+error-port
  (syntax-rules (<<)
    ((_ repf)
      (run/open-process-with-error-port repf #f #t))
    ((_ repf (<< schemeobj))
      (run/open-process-with-error-port repf schemeobj #t))))


(define-syntax run/strings
  (syntax-rules ()
    ((_ repf ...)
     (let-values (((proc outport)
		   (run/proc+port repf ...)))
       (let
	   ((out (port->lines outport))
	    (status (wait proc)))
	 (begin
	   (close-input-port outport)
	   (if (equal? 0 status)
	       out
	       (raise (failed-process-exception
		       (subprocess-pid proc)
		       status
		       proc)))))))))

(define-syntax run/status+strings
  (syntax-rules ()
    ((_ repf ...)
     (let-values (((proc outport)
	    (run/proc+port repf ...)))
       (let* ((out (port->lines outport))
	      (_ (close-input-port outport))
	      (status (wait proc)))
	 (values status out))))))

(define-syntax run/status+car-strings
  (syntax-rules ()
    ((_ repf ...)
     (let-values (((proc outport)
	    (run/proc+port repf ...)))
       (let* ((lines (port->lines outport))
	      (out (if (pair? lines)
		       (car lines)
		       (begin
			 (if (scsh-trace)
			     (display "| Got empty output\n"
				      (current-error-port))
			     (void))
			 #f)))
	      (_ (close-input-port outport))
	      (status (wait proc)))
	 (values status out))))))

(define-syntax run/status+strings-suppress-error
  (syntax-rules ()
    ((_ repf ...)
     (let-values (((proc outport errorport)
	    (run/proc+port+error-port repf ...)))
       (let* ((lines (port->lines outport))
	      (errs  (port->lines errorport))
	      (_ (close-input-port outport))
	      (_ (close-input-port errorport))	      
	      (status (wait proc)))
	 (when (scsh-trace)
	   (display (format "e ~a\n" errs)
		    (current-error-port)))
	 (values status lines))))))

(define (with-cwd* dir thunk)
  (let ((cwd-save (current-directory)))
    (dynamic-wind
      (lambda ()
	(current-directory dir))
      thunk
      (lambda ()
	(current-directory cwd-save)))))

(define-syntax with-cwd
  (syntax-rules ()
    ((_ dir body ...)
     (with-cwd*
      dir
      (lambda ()
	body ...)))))

(module+ main
  (define (main)
    (eval (read))
    (void))
  (main))
