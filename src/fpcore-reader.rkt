#lang racket

(require "common.rkt" "fpcore-checker.rkt" "fpcore-interpreter.rkt")
(provide read-fpcore)

;;
;; Parser
;;

(define/contract (check-syntax stx ctx)
  (-> syntax? (listof argument?) expr?)
  (match (syntax-e stx)
   [(? number? val)    val]
   [(? hex? val)       val]
   [(? constant? val)  val]
   [(? symbol? var)
     (unless (set-member? ctx var)
       (raise-syntax-error #f "Undefined variable" stx))
     var]
   [(list (app syntax-e 'digits) m e b)   ; digits
     (define m* (check-syntax m ctx)) 
     (define e* (check-syntax e ctx))
     (define b* (check-syntax b ctx))
     (unless (and (integer? m*) (integer? e*) (integer? b*))
       (raise-syntax-error #f "Values of digits must be integers" stx))
     (unless (>= b* 2)
       (raise-syntax-error #f "Base of digits must be greater than 1" stx))
    `(digits ,m* ,e* ,b*)]
   [(list (app syntax-e 'if) test ift iff)  ; if
    `(if ,(check-syntax test ctx) ,(check-syntax ift ctx) ,(check-syntax iff ctx))]
   [(cons (app syntax-e 'if) _)             ; if (invalid)
     (raise-syntax-error #f "Invalid conditional statement" stx)]
   [(list (app syntax-e 'let) (app syntax-e (list (app syntax-e (list vars vals)) ...)) body)  ; let
     (define vars*
       (for/list ([var vars])
         (unless (symbol? (syntax-e var))
           (raise-syntax-error #f "Only variables may be bound by let binding" stx var))
         (syntax-e var)))
     (define vals* (map (curryr check-syntax ctx) vals))
     (define ctx* (set-union ctx vars*))
     (define body* (check-syntax body ctx*))
    `(let (,@(map list vars* vals*)) ,body*)]
   [(list (app syntax-e 'let*) (app syntax-e (list (app syntax-e (list vars vals)) ...)) body)  ; let*
     (define vars*
       (for/list ([var vars])
         (unless (symbol? (syntax-e var))
           (raise-syntax-error #f "Only variables may be bound by let* binding" stx var))
         (syntax-e var)))
     (define-values (ctx* vals*)
       (for/fold ([ctx ctx] [vals '()]) ([var vars*] [val vals])
         (define val* (check-syntax val ctx))
         (define ctx* (set-add ctx var))
         (values ctx* (cons val* vals))))
     (define body* (check-syntax body ctx*))
    `(let* (,@(map list vars* (reverse vals*))) ,body*)]
   [(cons (app syntax-e (or 'let 'let*)) _)    ; let, let* (invalid)
     (raise-syntax-error #f "Invalid let bindings" stx)]
   [(list (app syntax-e 'while) test (app syntax-e (list (app syntax-e (list vars inits updates)) ...)) body) ; while
     (define vars*
       (for/list ([var vars])
         (unless (symbol? (syntax-e var))
           (raise-syntax-error #f "Only variables may be bound by while loop" stx var))
         (syntax-e var)))
     (define inits* (map (curryr check-syntax ctx) inits))
     (define ctx* (set-union ctx vars*))
     (define test* (check-syntax test ctx*))
     (define updates* (map (curryr check-syntax ctx*) updates))
     (define body* (check-syntax body ctx*))
    `(while ,test* (,@(map list vars* inits* updates*)) ,body*)]
   [(list (app syntax-e 'while*) test (app syntax-e (list (app syntax-e (list vars inits updates)) ...)) body) ; while*
     (define vars*
       (for/list ([var vars])
         (unless (symbol? (syntax-e var))
           (raise-syntax-error #f "Only variables may be bound by while loop" stx var))
         (syntax-e var)))
     (define-values (ctx* inits*)
       (for/fold ([ctx ctx] [inits* '()] #:result (values ctx (reverse inits*)))
                 ([var vars*] [init inits])
         (define init* (check-syntax init ctx))
         (define ctx* (set-add ctx var))
         (values ctx* (cons init* inits*))))
     (define test* (check-syntax test ctx*))
     (define updates* (map (curryr check-syntax ctx*) updates))
     (define body* (check-syntax body ctx*))
    `(while* ,test* (,@(map list vars* inits* updates*)) ,body*)]
   [(cons (app syntax-e (or 'while 'while*)) _)                 ; while, while* (invalid)
     (raise-syntax-error #f "Invalid while loop" stx)]
   [(list (app syntax-e 'for) (app syntax-e (list (app syntax-e (list vars vals)) ...)) (app syntax-e (list (app syntax-e (list accums inits updates)) ...)) body) ; for
     (define vars*
       (for/list ([var vars])
         (unless (symbol? (syntax-e var))
           (raise-syntax-error #f "Only variables may be bound by for binding" stx var))
         (syntax-e var)))
     (define vals* (map (curryr check-syntax ctx) vals))
     (define ctx* (set-union ctx vars*))
     (define accums*
       (for/list ([accum accums])
         (unless (symbol? (syntax-e accum))
           (raise-syntax-error #f "Only variables may be bound by for binding" stx accum))
         (syntax-e accum)))
     (define inits* (map (curryr check-syntax ctx*) inits))
     (define ctx** (set-union ctx* accums*))
     (define updates* (map (curryr check-syntax ctx**) updates))
     (define body* (check-syntax body ctx**))
    `(for (,@(map list vars* vals*)) (,@(map list accums* inits* updates*)) ,body*)]
   [(list (app syntax-e 'for*) (app syntax-e (list (app syntax-e (list vars vals)) ...)) (app syntax-e (list (app syntax-e (list accums inits updates)) ...)) body) ; for*
     (define vars*
       (for/list ([var vars])
         (unless (symbol? (syntax-e var))
           (raise-syntax-error #f "Only variables may be bound by for binding" stx var))
         (syntax-e var)))
     (define vals* (map (curryr check-syntax ctx) vals))
     (define ctx* (set-union ctx vars*))
     (define accums*
       (for/list ([accum accums])
         (unless (symbol? (syntax-e accum))
           (raise-syntax-error #f "Only variables may be bound by for binding" stx accum))
         (syntax-e accum)))
     (define-values (ctx** inits*)
       (for/fold ([ctx* ctx*] [inits* '()] #:result (values ctx* (reverse inits*)))
                 ([var accums*] [init inits])
         (define init* (check-syntax init ctx*))
         (define ctx** (set-add ctx* var))
         (values ctx** (cons init* inits*))))
     (define updates* (map (curryr check-syntax ctx**) updates))
     (define body* (check-syntax body ctx**))
    `(for* (,@(map list vars* vals*)) (,@(map list accums* inits* updates*)) ,body*)]
   [(cons (app syntax-e (or 'for 'for*)) _)               ; for, for* (invalid)
     (raise-syntax-error #f "Invalid for loop" stx)]
   [(list (app syntax-e 'tensor) (app syntax-e (list (app syntax-e (list vars vals)) ...)) body)  ; tensor
     (define vars*
       (for/list ([var vars])
         (unless (symbol? (syntax-e var))
           (raise-syntax-error #f "Only variables may be bound by tensor binding" stx var))
         (syntax-e var)))
     (define vals* (map (curryr check-syntax ctx) vals))
     (define ctx* (set-union ctx vars*))
     (define body* (check-syntax body ctx*))
    `(tensor (,@(map list vars* vals*)) ,body*)]
    [(list (app syntax-e 'tensor*) (app syntax-e (list (app syntax-e (list vars vals)) ...)) (app syntax-e (list (app syntax-e (list accums inits updates)) ...)) body)  ; tensor*
     (define vars*
       (for/list ([var vars])
         (unless (symbol? (syntax-e var))
           (raise-syntax-error #f "Only variables may be bound by tensor binding" stx var))
         (syntax-e var)))
     (define vals* (map (curryr check-syntax ctx) vals))
     (define ctx* (set-union ctx vars*))
     (define accums*
       (for/list ([accum accums])
         (unless (symbol? (syntax-e accum))
           (raise-syntax-error #f "Only variables may be bound by tensor binding" stx accum))
         (syntax-e accum)))
     (define-values (ctx** inits*)
       (for/fold ([ctx* ctx*] [inits* '()] #:result (values ctx* (reverse inits*)))
                 ([var accums*] [init inits])
         (define init* (check-syntax init ctx*))
         (define ctx** (set-add ctx* var))
         (values ctx** (cons init* inits*))))
     (define updates* (map (curryr check-syntax ctx**) updates))
     (define body* (check-syntax body ctx**))
    `(tensor* (,@(map list vars* vals*)) (,@(map list accums* inits* updates*)) ,body*)]
   [(cons (app syntax-e (or 'tensor 'tensor*)) _)               ; tensor tensor* (invalid)
     (raise-syntax-error #f "Invalid tensor construction" stx)]
   [(list (app syntax-e '!) props ... expr)                     ; !
     (define expr* (check-syntax expr ctx))
     (define props* (map syntax-e-rec props))
    `(! ,@props* ,expr*)]
   [(list (app syntax-e op) args ...)                                         ; ops
     (define children (map (curryr check-syntax ctx) args))
    `(,op ,@children)]))

(define (parse-fpcore* name vars properties body stx)
  (define-values (annotated-args args)
    (for/fold ([annot-args '()] [args '()] #:result (values (reverse annot-args) args))
              ([var vars])
      (let ([var* (syntax-e-rec var)])
        (unless (argument? var*)
          (raise-syntax-error #f "FPCore parameters must be variables" stx var))
        (match var*
         [`(,(? symbol? name) ,(or (? number? sizes) (? symbol? sizes)) ...) 
          (values (cons var* annot-args) (cons name (append (filter symbol? sizes) args)))]
         [(? list?) 
          (values (cons var* annot-args) (cons (last var*) args))]
         [_ (values (cons var* annot-args) (cons var* args))]))))
         
  (define properties*
    (let loop ([properties properties])
      (match properties
        [(list) (list)]
        [(list prop) (raise-syntax-error #f "Property with no value" prop)]
        [(list (app syntax-e (? property? prop)) value rest ...)
        (cons (cons prop value) (loop rest))]
        [(list prop _ ...) (raise-syntax-error #f "Invalid property" prop)])))

  (if name
     `(FPCore ,name (,@annotated-args)
         ,@(apply append
                  (for/list ([(prop val) (in-dict properties*)])
                    (list prop (syntax->datum val))))
         ,(check-syntax body args))
     `(FPCore (,@annotated-args)
         ,@(apply append
                  (for/list ([(prop val) (in-dict properties*)])
                    (list prop (syntax->datum val))))
         ,(check-syntax body args))))

(define/contract (parse-fpcore stx)
  (-> syntax? fpcore?)
  (match (syntax-e stx)
   [(list (app syntax-e 'FPCore) (app syntax-e name) (app syntax-e (list vars ...)) properties ... body)
    (unless (symbol? name)
      (raise-syntax-error #f "FPCore identifier must be a symbol" stx name))
    (parse-fpcore* name vars properties body stx)]
   [(list (app syntax-e 'FPCore) (app syntax-e (list vars ...)) properties ... body)
    (parse-fpcore* #f vars properties body stx)]))

;;
;;  Reader
;;

; Adapted from Racket source:
; https://github.com/racket/racket/blob/master/racket/src/cs/bootstrap.scheme-readtable.rkt
(define ((paren closer) c in src line col pos)
  (let loop ()
    (define c (peek-char in))
    (cond
      [(eqv? closer c)
       (read-char in)
       null]
      [(char-whitespace? c)
       (read-char in)
       (loop)]
      [(eqv? #\# c)   ;; Syntactic sugar expansion: '#' -> '! precision integer'
       (read-char in)
       `(! :precision integer ,(read/recursive in) . ,(loop))]
      [(and (eqv? #\. c)
            (char-whitespace? (peek-char in 1)))
       (read-char in)
       (begin0
         (read/recursive in)
         (let loop ()
           (define c (read-char in))
           (cond
             [(char-whitespace? c) (loop)]
             [(eqv? c closer) (void)]
             [else (error 'parens "unexpected: ~s" c)])))]
      [else
       (define v (read/recursive in))
       (if (special-comment? v)
           (loop)
           (cons v (loop)))])))

(define fpcore-readtable
  (make-readtable
   #f
   #\( 'terminating-macro (paren #\))))

(define/contract (read-fpcore name p)
  (-> any/c input-port? (or/c fpcore? eof-object?))
  (parameterize ([read-decimal-as-inexact #f] 
                 [current-readtable fpcore-readtable])
    (define stx (read-syntax name p))
    (if (eof-object? stx) stx (parse-fpcore stx))))