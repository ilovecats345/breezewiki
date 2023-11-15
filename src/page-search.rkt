#lang racket/base
(require racket/dict
         racket/list
         racket/string
         (prefix-in easy: net/http-easy)
         ; html libs
         html-writing
         ; web server libs
         net/url
         web-server/http
         (only-in web-server/dispatchers/dispatch next-dispatcher)
         #;(only-in web-server/http/redirect redirect-to)
         "application-globals.rkt"
         "config.rkt"
         "data.rkt"
         "../lib/syntax.rkt"
         "../lib/thread-utils.rkt"
         "../lib/url-utils.rkt"
         "whole-utils.rkt"
         "../lib/xexpr-utils.rkt")

(provide
 page-search)

(module+ test
  (require rackunit
           "test-utils.rkt")
  (define search-json-data
    '#hasheq((batchcomplete . #t) (query . #hasheq((search . (#hasheq((ns . 0) (pageid . 219) (size . 1482) (snippet . "") (timestamp . "2022-08-21T08:54:23Z") (title . "Gacha Capsule") (wordcount . 214)) #hasheq((ns . 0) (pageid . 201) (size . 1198) (snippet . "") (timestamp . "2022-07-11T17:52:47Z") (title . "Badges") (wordcount . 181)))))))))

;; this takes the info we gathered from fandom and makes the big fat x-expression page
(define (generate-results-page req dest-url wikiname query data #:siteinfo [siteinfo #f])
  (define search-results (jp "/query/search" data))
  ;; this is *another* helper that builds the wiki page UI and lets me put the search results (or whatever else) in the middle
  (generate-wiki-page
   ;; so I provide my helper function with the necessary context...
   #:req req
   #:source-url dest-url
   #:wikiname wikiname
   #:title query
   #:siteinfo siteinfo
   ;; and here's the actual results to display in the wiki page layout
   `(div (@ (class "mw-parser-output"))
         ;; header before the search results showing how many we found
         (p ,(format "~a results found for " (length search-results))
            (strong ,query))
         ;; *u*nordered *l*ist of matching search results
         (ul ,@(map
                (λ (result) ;; for each result, run this code...
                  (let* ([title (jp "/title" result)]
                         [page-path (page-title->path title)]
                         [timestamp (jp "/timestamp" result)]
                         [wordcount (jp "/wordcount" result)]
                         [size (jp "/size" result)])
                    ;; and make this x-expression...
                    `(li (@ (class "my-result"))
                         (a (@ (class "my-result__link") (href ,(format "/~a/wiki/~a" wikiname page-path))) ; using unquote to insert the result page URL
                            ,title) ; using unquote to insert the result page title
                         (div (@ (class "my-result__info")) ; constructing the line under the search result
                              "last edited "
                              (time (@ (datetime ,timestamp)) ,(list-ref (string-split timestamp "T") 0))
                              ,(format ", ~a words, ~a kb"
                                       wordcount
                                       (exact->inexact (/ (round (/ size 100)) 10)))))))
                search-results)))))

;; will be called when the web browser asks to load the page
(define (page-search req)
  ;; this just means, catch any errors and display them in the browser. it's a function somewhere else
  (response-handler
   (cond
     [(config-true? 'feature_offline::only)
      (response/output #:code 503
                       #:headers (build-headers always-headers)
                       (λ (out) (write-html '(body (p "Sorry, full search is temporarily broken, but I have a plan to fix it.") (p "In the meantime, please use the popup search suggestions below the search box.")) out)))]
     [else
      ;; the URL will look like "/minecraft/wiki/Special:Search?q=Spawner"
      ;; grab the first part to use as the wikiname, in this case, "minecraft"
      (define wikiname (path/param-path (first (url-path (request-uri req)))))
      ;; grab the part after ?q= which is the search terms
      (define query (dict-ref (url-query (request-uri req)) 'q #f))
      ;; constructing the URL where I want to get fandom data from...
      (define origin (format "https://~a.fandom.com" wikiname))
      ;; the dest-URL will look something like https://minecraft.fandom.com/api.php?action=query&list=search&srsearch=Spawner&formatversion=2&format=json
      (define dest-url
        (format "~a/api.php?~a"
                origin
                (params->query `(("action" . "query")
                                 ("list" . "search")
                                 ("srsearch" . ,query)
                                 ("formatversion" . "2")
                                 ("format" . "json")))))

      ;; simultaneously get the search results from the fandom API, as well as information about the wiki as a whole (its license, icon, name)
      (define-values (dest-res siteinfo)
        (thread-values
         (λ ()
           (log-outgoing dest-url)
           (easy:get dest-url #:timeouts timeouts)) ;; HTTP request to dest-url for search results
         (λ ()
           (siteinfo-fetch wikiname)))) ;; helper function in another file to get information about the wiki

      ;; search results are a JSON string. parse JSON into racket data structures
      (define data (easy:response-json dest-res))
      ;; calling my generate-results-page function with the information so far in order to get a big fat x-expression
      ;; big fat x-expression goes into the body variable
      (define body (generate-results-page req dest-url wikiname query data #:siteinfo siteinfo))
      ;; error checking
      (when (config-true? 'debug)
        ; used for its side effects
        ; convert to string with error checking, error will be raised if xexp is invalid
        (xexp->html body))
      ;; convert body to HTML and send to browser
      (response/output
       #:code 200
       #:headers (build-headers always-headers)
       (λ (out)
         (write-html body out)))])))
(module+ test
  (parameterize ([(config-parameter 'feature_offline::only) "false"])
    (check-not-false ((query-selector (attribute-selector 'href "/test/wiki/Gacha_Capsule")
                                      (generate-results-page test-req "" "test" "Gacha" search-json-data))))))
