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

(provide page-file)

(module+ test
  (require rackunit
           "test-utils.rkt")
  (define test-media-detail
    '#hasheq((fileTitle . "Example file")
             (videoEmbedCode . "")
             (imageUrl . "https://static.wikia.nocookie.net/examplefile")
             (rawImageUrl . "https://static.wikia.nocookie.net/examplefile")
             (userName . "blankie")
             (isPostedIn . #t)
             (smallerArticleList . (#hasheq((titleText . "Test:Example article"))))
             (articleListIsSmaller . 0)
             (exists . #t)
             (imageDescription . #f))))

(define (url-content-type url)
  (log-outgoing url)
  (define dest-res (easy:head url #:timeouts timeouts))
  (easy:response-headers-ref dest-res 'content-type))

(define (get-media-html url content-type)
  (define maybe-proxied-url (if (config-true? 'strict_proxy) (u-proxy-url url) url))
  (cond
    [(eq? content-type #f) `""]
    [(regexp-match? #rx"(?i:^image/)" content-type) `(img (@ (src ,maybe-proxied-url)))]
    [(regexp-match? #rx"(?i:^audio/|^application/ogg(;|$))" content-type)
     `(audio (@ (src ,maybe-proxied-url) (controls)))]
    [(regexp-match? #rx"(?i:^video/)" content-type) `(video (@ (src ,maybe-proxied-url) (controls)))]
    [else `""]))

(define (generate-results-page #:req req
                               #:source-url source-url
                               #:wikiname wikiname
                               #:title title
                               #:media-detail media-detail
                               #:image-content-type image-content-type
                               #:siteinfo [siteinfo #f])
  (define video-embed-code (jp "/videoEmbedCode" media-detail ""))
  (define raw-image-url (jp "/rawImageUrl" media-detail))
  (define image-url (jp "/imageUrl" media-detail raw-image-url))
  (define username (jp "/userName" media-detail))
  (define is-posted-in (jp "/isPostedIn" media-detail #f))
  (define smaller-article-list (jp "/smallerArticleList" media-detail))
  (define article-list-is-smaller (jp "/articleListIsSmaller" media-detail))
  (define image-description (jp "/imageDescription" media-detail #f))
  (define maybe-proxied-raw-image-url
    (if (config-true? 'strict_proxy) (u-proxy-url raw-image-url) raw-image-url))
  (generate-wiki-page
   #:req req
   #:source-url source-url
   #:wikiname wikiname
   #:title title
   #:siteinfo siteinfo
   `(div ,(if (non-empty-string? video-embed-code)
              (update-tree-wiki (html->xexp (preprocess-html-wiki video-embed-code)) wikiname)
              (get-media-html image-url image-content-type))
         (p ,(if (non-empty-string? video-embed-code)
                 `""
                 `(span (a (@ (href ,maybe-proxied-raw-image-url)) "View original file") ". "))
            "Uploaded by "
            (a (@ (href ,(format "/~a/wiki/User:~a" wikiname username))) ,username)
            ".")
         ,(if (string? image-description)
              (update-tree-wiki (html->xexp (preprocess-html-wiki image-description)) wikiname)
              ; file license info might be displayed in the description, example: /lgbtqia/wiki/File:Rainbow_Flag1.svg
              `(p "This file is likely protected by copyright. Consider the file's license and fair use law before reusing it."))
         ,(if is-posted-in
              `(p "This file is used in "
                  ,@(map (λ (article)
                           (define title (jp "/titleText" article))
                           (define page-path (regexp-replace* #rx" " title "_"))
                           `(span ,(if (eq? (car smaller-article-list) article) "" ", ")
                                  (a (@ (href ,(format "/~a/wiki/~a" wikiname page-path)))
                                     ,title)))
                         smaller-article-list)
                  ,(if (eq? article-list-is-smaller 1) "…" "."))
              `""))))

(define (page-file req)
  (response-handler
   (cond
     [(config-true? 'feature_offline::only)
      (response/output #:code 503
                       #:headers (build-headers always-headers)
                       (λ (out) (write-html '(p "Sorry, file pages are temporarily disabled. I hope to have them back soon.") out)))]
     [else
      (define wikiname (path/param-path (first (url-path (request-uri req)))))
      (define prefixed-title (path/param-path (caddr (url-path (request-uri req)))))
      (define origin (format "https://~a.fandom.com" wikiname))
      (define source-url (format "~a/wiki/~a" origin prefixed-title))

      (define-values (media-detail siteinfo)
        (thread-values
         (λ ()
           (define dest-url
             (format "~a/wikia.php?~a"
                     origin
                     (params->query `(("format" . "json") ("controller" . "Lightbox")
                                                          ("method" . "getMediaDetail")
                                                          ("fileTitle" . ,prefixed-title)))))
           (log-outgoing dest-url)
           (define dest-res (easy:get dest-url #:timeouts timeouts))
           (easy:response-json dest-res))
         (λ ()
           (siteinfo-fetch wikiname))))
      (if (not (jp "/exists" media-detail #f))
          (next-dispatcher)
          (response-handler
           (define file-title (jp "/fileTitle" media-detail ""))
           (define title
             (if (non-empty-string? file-title) (format "File:~a" file-title) prefixed-title))
           (define image-content-type
             (if (non-empty-string? (jp "/videoEmbedCode" media-detail ""))
                 #f
                 (url-content-type (jp "/imageUrl" media-detail))))
           (define body
             (generate-results-page #:req req
                                    #:source-url source-url
                                    #:wikiname wikiname
                                    #:title title
                                    #:media-detail media-detail
                                    #:image-content-type image-content-type
                                    #:siteinfo siteinfo))
           (when (config-true? 'debug)
             ; used for its side effects
             ; convert to string with error checking, error will be raised if xexp is invalid
             (xexp->html body))
           (response/output #:code 200
                            #:headers (build-headers always-headers)
                            (λ (out) (write-html body out)))))])))
(module+ test
  (parameterize ([(config-parameter 'strict_proxy) "true"])
    (check-equal? (get-media-html "https://static.wikia.nocookie.net/a" "image/jpeg")
                  `(img (@ (src "/proxy?dest=https%3A%2F%2Fstatic.wikia.nocookie.net%2Fa"))))
    (check-equal? (get-media-html "https://static.wikia.nocookie.net/b" "audio/mp3")
                  `(audio (@ (src "/proxy?dest=https%3A%2F%2Fstatic.wikia.nocookie.net%2Fb")
                             (controls)))))
  (parameterize ([(config-parameter 'strict_proxy) "false"])
    (check-equal? (get-media-html "https://static.wikia.nocookie.net/c" "application/ogg")
                  `(audio (@ (src "https://static.wikia.nocookie.net/c")
                             (controls))))
    (check-equal? (get-media-html "https://static.wikia.nocookie.net/d" "video/mp4")
                  `(video (@ (src "https://static.wikia.nocookie.net/d")
                             (controls)))))
  (check-equal? (get-media-html "https://example.com" "who knows") `"")
  (check-equal? (get-media-html "https://example.com" #f) `""))
(module+ test
  (parameterize ([(config-parameter 'strict_proxy) "true"])
    (check-not-false
     ((query-selector
       (attribute-selector 'src "/proxy?dest=https%3A%2F%2Fstatic.wikia.nocookie.net%2Fexamplefile")
       (generate-results-page #:req test-req
                              #:source-url ""
                              #:wikiname "test"
                              #:title "File:Example file"
                              #:media-detail test-media-detail
                              #:image-content-type "image/jpeg"))))))
