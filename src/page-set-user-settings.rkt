#lang racket/base
(require racket/dict
         net/url
         web-server/http
         "application-globals.rkt"
         "data.rkt"
         "url-utils.rkt"
         "xexpr-utils.rkt")

(provide
 page-set-user-settings)

(define (page-set-user-settings req)
  (response-handler
   (define next-location (dict-ref (url-query (request-uri req)) 'next_location))
   (define new-settings (read (open-input-string (dict-ref (url-query (request-uri req)) 'new_settings))))
   (define headers (user-cookies-setter new-settings))
   (generate-redirect next-location #:headers headers)))
