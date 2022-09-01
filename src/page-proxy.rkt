#lang racket/base
(require racket/dict
         racket/match
         racket/port
         ; libs
         (prefix-in easy: net/http-easy)
         ; web server libs
         net/url
         web-server/http
         (only-in web-server/dispatchers/dispatch next-dispatcher)
         "url-utils.rkt"
         "xexpr-utils.rkt")

(provide
 page-proxy)

(define (page-proxy req)
  (match (dict-ref (url-query (request-uri req)) 'dest #f)
    [(? string? dest)
     (if (is-fandom-url? dest)
         (response-handler
          (let ([dest-r (easy:get dest #:stream? #t)])
            (response/output
             #:code (easy:response-status-code dest-r)
             #:mime-type (easy:response-headers-ref dest-r 'content-type)
             (λ (out)
               (copy-port (easy:response-output dest-r) out)))))
         (next-dispatcher))]
    [#f (next-dispatcher)]))
