#lang racket/base
(require racket/dict
         racket/list
         racket/match
         racket/string
         (prefix-in easy: net/http-easy)
         ; html libs
         html-parsing
         html-writing
         ; web server libs
         net/url
         web-server/http
         (only-in web-server/dispatchers/dispatch next-dispatcher)
         #;(only-in web-server/http/redirect redirect-to)
         "application-globals.rkt"
         "config.rkt"
         "data.rkt"
         "page-wiki.rkt"
         "../lib/syntax.rkt"
         "../lib/thread-utils.rkt"
         "../lib/url-utils.rkt"
         "whole-utils.rkt"
         "../lib/xexpr-utils.rkt")

(provide
 page-category)

(module+ test
  (require rackunit
           "test-utils.rkt")
  (define category-json-data
    '#hasheq((batchcomplete . #t) (continue . #hasheq((cmcontinue . "page|4150504c45|41473") (continue . "-||"))) (query . #hasheq((categorymembers . (#hasheq((ns . 0) (pageid . 25049) (title . "Item (entity)")) #hasheq((ns . 0) (pageid . 128911) (title . "3D")) #hasheq((ns . 0) (pageid . 124018) (title . "A Very Fine Item")) #hasheq((ns . 0) (pageid . 142208) (title . "Amethyst Shard")) #hasheq((ns . 0) (pageid . 121612) (title . "Ankle Monitor")))))))))

(define (generate-results-page
         #:req req
         #:source-url source-url
         #:wikiname wikiname
         #:title title
         #:members-data members-data
         #:page page
         #:head-data [head-data #f]
         #:siteinfo [siteinfo #f])
  (define members (jp "/query/categorymembers" members-data))
  (generate-wiki-page
   #:req req
   #:source-url source-url
   #:wikiname wikiname
   #:title title
   #:head-data head-data
   #:siteinfo siteinfo
   `(div
     ,(update-tree-wiki page wikiname)
     (hr)
     (h2 ,(format "All Pages in ~a" title))
     (div (@ (class "mw-parser-output"))
          (ul (@ (class "my-category-list"))
              ,@(map
                 (λ (result)
                   (define title (jp "/title" result))
                   (define page-path (page-title->path title))
                   `(li
                     (a (@ (href ,(format "/~a/wiki/~a" wikiname page-path)))
                        ,title)))
                 members))))))

(define (page-category req)
  (response-handler
   (cond
     [(config-true? 'feature_offline::only)
      (response/output #:code 503
                       #:headers (build-headers always-headers)
                       (λ (out) (write-html '(p "Sorry, category pages are temporarily disabled. I hope to have them back soon.") out)))]
     [else
      (define wikiname (path/param-path (first (url-path (request-uri req)))))
      (define prefixed-category (string-join (map path/param-path (cddr (url-path (request-uri req)))) "/"))
      (define origin (format "https://~a.fandom.com" wikiname))
      (define source-url (format "~a/wiki/~a" origin prefixed-category))

      (define-values (members-data page-data siteinfo)
        (thread-values
         (λ ()
           (define dest-url
             (format "~a/api.php?~a"
                     origin
                     (params->query `(("action" . "query")
                                      ("list" . "categorymembers")
                                      ("cmtitle" . ,prefixed-category)
                                      ("cmlimit" . "max")
                                      ("formatversion" . "2")
                                      ("format" . "json")))))
           (log-outgoing dest-url)
           (define dest-res (easy:get dest-url #:timeouts timeouts))
           (easy:response-json dest-res))
         (λ ()
           (define dest-url
             (format "~a/api.php?~a"
                     origin
                     (params->query `(("action" . "parse")
                                      ("page" . ,prefixed-category)
                                      ("prop" . "text|headhtml|langlinks")
                                      ("formatversion" . "2")
                                      ("format" . "json")))))
           (log-outgoing dest-url)
           (define dest-res (easy:get dest-url #:timeouts timeouts))
           (easy:response-json dest-res))
         (λ ()
           (siteinfo-fetch wikiname))))

      (define title (preprocess-html-wiki (jp "/parse/title" page-data prefixed-category)))
      (define page-html (preprocess-html-wiki (jp "/parse/text" page-data "")))
      (define page (html->xexp page-html))
      (define head-data ((head-data-getter wikiname) page-data))
      (define body (generate-results-page
                    #:req req
                    #:source-url source-url
                    #:wikiname wikiname
                    #:title title
                    #:members-data members-data
                    #:page page
                    #:head-data head-data
                    #:siteinfo siteinfo))

      (when (config-true? 'debug)
        ; used for its side effects
        ; convert to string with error checking, error will be raised if xexp is invalid
        (xexp->html body))
      (response/output
       #:code 200
       #:headers (build-headers always-headers)
       (λ (out)
         (write-html body out)))])))
(module+ test
  (check-not-false ((query-selector (attribute-selector 'href "/test/wiki/Ankle_Monitor")
                                    (generate-results-page
                                     #:req test-req
                                     #:source-url ""
                                     #:wikiname "test"
                                     #:title "Category:Items"
                                     #:members-data category-json-data
                                     #:page '(div "page text"))))))
