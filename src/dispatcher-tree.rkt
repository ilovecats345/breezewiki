#lang racket/base
(require "syntax.rkt"
         (for-syntax racket/base)
         racket/string
         net/url
         (prefix-in host: web-server/dispatchers/dispatch-host)
         (prefix-in pathprocedure: web-server/dispatchers/dispatch-pathprocedure)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         web-server/private/connection-manager
         "config.rkt"
         "static-data.rkt"
         "url-utils.rkt")

(provide
 ; syntax to make the hashmap from names
 dispatcher-tree
 ; procedure to make the tree from the hashmap
 make-dispatcher-tree)

; make a hashmap out of the provided names and call make-dispatcher-tree with it
(define-syntax (dispatcher-tree stx)
  ; the arguments, which are names of dispatcher variables
  (define names (cdr (syntax->list stx)))
  ; map each name to syntax of a '(name . ,name)
  (define alist (map (λ (xe) ; xe is the syntax of a name
                       ; return instead syntax of a cons cell
                       (datum->syntax stx `(cons ',xe ,xe)))
                     names))
  ; make syntax to make the hash
  (define ds (datum->syntax stx `(make-hasheq (list ,@alist))))
  ; don't forget that I'm returning *code* - return a call to the function
  (datum->syntax stx `(make-dispatcher-tree ,ds)))

(define (write-bytes* out . bs)
  (for ([b bs]) (write-bytes b out)))

;; HTTP 103 Early Hints https://datatracker.ietf.org/doc/html/rfc8297
;; a dispatcher services a request or calls next-dispatcher
(define ((early-hints:make original-dispatcher) conn req)
  ;; normally this would call output-response/method to write output based on the method
  ;; e.g. if it's HEAD then only output the headers
  (define o (connection-o-port conn))
  (write-bytes* o
                #"HTTP/1.1 103 Early Hints\r\n"
                #"Link: " (string->bytes/latin-1 link-header) #"\r\n"
                #"\r\n")
  (flush-output o)
  ;; after doing early hints, I need to run the original dispatcher to generate the real response.
  ;; dispatchers do not return anything.
  (original-dispatcher conn req))

(define (make-dispatcher-tree ds)
  (host:make
   (λ (host-sym)
     (if/out (config-true? 'canonical_origin)
             (let* ([host-header (symbol->string host-sym)]
                    [splitter (string-append "." (url-host (string->url (config-get 'canonical_origin))))]
                    [s (string-split host-header splitter #:trim? #f)])
               (if/in (and (eq? 2 (length s)) (equal? "" (cadr s)))
                      ((hash-ref ds 'subdomain-dispatcher) (car s))))
             (sequencer:make
              (pathprocedure:make "/" (hash-ref ds 'page-home))
              (pathprocedure:make "/proxy" (hash-ref ds 'page-proxy))
              (pathprocedure:make "/search" (hash-ref ds 'page-global-search))
              (pathprocedure:make "/buddyfight/wiki/It_Doesn't_Work!!" (hash-ref ds 'page-it-works))
              (filter:make (pregexp (format "^/~a/wiki/Category:.+$" px-wikiname)) (lift:make (hash-ref ds 'page-category)))
              (filter:make (pregexp (format "^/~a/wiki/File:.+$" px-wikiname)) (lift:make (hash-ref ds 'page-file)))
              (filter:make (pregexp (format "^/~a/wiki/.+$" px-wikiname)) (early-hints:make (lift:make (hash-ref ds 'page-wiki))))
              (filter:make (pregexp (format "^/~a/search$" px-wikiname)) (lift:make (hash-ref ds 'page-search)))
              (filter:make (pregexp (format "^/~a(/(wiki(/)?)?)?$" px-wikiname)) (lift:make (hash-ref ds 'redirect-wiki-home)))
              (hash-ref ds 'static-dispatcher)
              (lift:make (hash-ref ds 'page-not-found)))))))
