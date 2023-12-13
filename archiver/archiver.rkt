#lang racket/base
(require racket/file
         racket/format
         racket/function
         racket/list
         racket/path
         racket/sequence
         racket/string
         net/url
         net/mime
         file/sha1
         net/http-easy
         db
         json
         "archiver-database.rkt"
         "../lib/html-parsing/main.rkt"
         "../lib/mime-types.rkt"
         "../lib/syntax.rkt"
         "../lib/tree-updater.rkt"
         "../lib/url-utils.rkt"
         "../lib/xexpr-utils.rkt"
         "../lib/archive-file-mappings.rkt")

(provide
 basename->name-for-query
 image-url->values
 hash->save-dir
 all-stages)

(module+ test
  (require rackunit))

(define archive-root (anytime-path ".." "storage/archive"))
(make-directory* archive-root)

(define sources '#hasheq((style . 1) (page . 2)))

(define (get-origin wikiname)
  (format "https://~a.fandom.com" wikiname))

(define (insert-wiki-entry wikiname)
  (define dest-url
    (format "https://~a.fandom.com/api.php?~a"
            wikiname
            (params->query '(("action" . "query")
                             ("meta" . "siteinfo")
                             ("siprop" . "general|rightsinfo|statistics|namespaces")
                             ("format" . "json")
                             ("formatversion" . "2")))))
  (define data (response-json (get dest-url)))
  (define content-nss
    (sort
     (for/list ([(k v) (in-hash (jp "/query/namespaces" data))]
                #:do [(define id (hash-ref v 'id))]
                #:when (and (id . < . 2900) ; exclude maps namespace
                            (hash-ref v 'content))) ; exclude non-content and talk namespaces
       id)
     <))
  (define exists? (query-maybe-value* "select progress from wiki where wikiname = ?" wikiname))
  (if (and exists? (not (sql-null? exists?)))
      (query-exec* "update wiki set sitename = ?, basepage = ?, license_text = ?, license_url = ? where wikiname = ?"
                  (jp "/query/general/sitename" data)
                  (second (regexp-match #rx"/wiki/(.*)" (jp "/query/general/base" data)))
                  (jp "/query/rightsinfo/text" data)
                  (jp "/query/rightsinfo/url" data)
                  wikiname)
      (query-exec* "insert into wiki (wikiname, progress, sitename, basepage, license_text, license_url) values (?, 0, ?, ?, ?, ?)"
                  wikiname
                  (jp "/query/general/sitename" data)
                  (second (regexp-match #rx"/wiki/(.*)" (jp "/query/general/base" data)))
                  (jp "/query/rightsinfo/text" data)
                  (jp "/query/rightsinfo/url" data)))
  (values (jp "/query/statistics/articles" data)
          content-nss))


(define (check-style-for-images wikiname path)
  (define content (file->string path))
  (define urls (regexp-match* #rx"url\\(\"?'?([^)]*)'?\"?\\)" content #:match-select cadr))
  (for/list ([url urls]
             #:when (not (or (equal? url "")
                             (equal? url "'")
                             (string-suffix? url "\"")
                             (string-contains? url "/resources-ucp/")
                             (string-contains? url "/fonts/")
                             (string-contains? url "/drm_fonts/")
                             (string-contains? url "//db.onlinewebfonts.com/")
                             (string-contains? url "//bits.wikimedia.org/")
                             (string-contains? url "mygamercard.net/")
                             (string-contains? url "dropbox")
                             (string-contains? url "only=styles")
                             (string-contains? url "https://https://")
                             (regexp-match? #rx"^%20" url)
                             (regexp-match? #rx"^data:" url)
                             (regexp-match? #rx"^file:" url))))
    (cond
      [(string-prefix? url "https://") url]
      [(string-prefix? url "http://") (regexp-replace #rx"http:" url "https:")]
      [(string-prefix? url "httpshttps://") (regexp-replace #rx"httpshttps://" url "https://")]
      [(string-prefix? url "//") (string-append "https:" url)]
      [(string-prefix? url "/") (format "https://~a.fandom.com~a" wikiname url)]
      [else (raise-user-error "While calling check-style-for-images, this URL had an unknown format and couldn't be saved:" url path)])))

(define (download-styles-for-wiki wikiname callback)
  (define save-dir (build-path archive-root wikiname "styles"))
  (make-directory* save-dir)
  (define theme (λ (theme-name)
                  (cons (format "https://~a.fandom.com/wikia.php?controller=ThemeApi&method=themeVariables&variant=~a" wikiname theme-name)
                        (build-path save-dir (format "themeVariables-~a.css" theme-name)))))
  ;; (Listof (Pair url save-path))
  (define styles
    (list
     (theme "default")
     (theme "light")
     (theme "dark")
     (cons (format "https://~a.fandom.com/load.php?lang=en&modules=site.styles%7Cskin.fandomdesktop.styles%7Cext.fandom.PortableInfoboxFandomDesktop.css%7Cext.fandom.GlobalComponents.CommunityHeaderBackground.css%7Cext.gadget.site-styles%2Csound-styles&only=styles&skin=fandomdesktop" wikiname)
           (build-path save-dir "site.css"))))
  (for ([style styles]
        [i (in-naturals)])
    (callback i (length styles) "styles...")
    (define r (get (car style)))
    (define body (response-body r))
    (display-to-file body (cdr style) #:exists 'replace)
    ;; XXX: how the HELL do I deal with @import?? would need some kind of recursion here. how will the page server know where to look up the style file to be able to serve them again? do I add another link-stylesheet tag to the main page? what about the remaining stuck @import url?
    )
  (callback (length styles) (length styles) "styles...")
  styles)

(define (hash->save-dir wikiname hash)
  (build-path archive-root wikiname "images" (substring hash 0 1) (substring hash 0 2)))

(define (image-url->values i)
  ;; TODO: handle case where there is multiple broken cb parameter on minecraft wiki
  ;; TODO: ensure it still "works" with broken &amp; on minecraft wiki
  (define no-cb (regexp-replace #rx"\\cb=[0-9]+&?" i "")) ; remove cb url parameter which does nothing
  (define key (regexp-replace #rx"[&?]$" no-cb "")) ; remove extra separator if necessary
  (define hash (sha1 (string->bytes/utf-8 key)))
  (cons key hash))


;; 1. Download list of wiki pages and store in database, if not done yet for that wiki
(define (if-necessary-download-list-of-pages wikiname callback)
  (define wiki-progress (query-maybe-value* "select progress from wiki where wikiname = ?" wikiname))
  ;; done yet?
  (unless (and (real? wiki-progress) (wiki-progress . >= . 1))
    ;; Count total pages
    (define-values (num-pages namespaces) (insert-wiki-entry wikiname))
    ;; Download the entire index of pages
    (for*/fold ([total 0])
               ([namespace namespaces]
                [redir-filter '("nonredirects" "redirects")])
      (let loop ([apcontinue ""]
                 [basenames null])
        (cond
          [apcontinue
           (define url (format "https://~a.fandom.com/api.php?~a"
                               wikiname
                               (params->query `(("action" . "query")
                                                ("list" . "allpages")
                                                ("apnamespace" . ,(~a namespace))
                                                ("apfilterredir" . ,redir-filter)
                                                ("aplimit" . "500")
                                                ("apcontinue" . ,apcontinue)
                                                ("format" . "json")
                                                ("formatversion" . "2")))))
           ;; Download the current listing page
           (define res (get url))
           (define json (response-json res))
           ;; Content from this page
           (define current-basenames
             (for/list ([page (jp "/query/allpages" json)])
               (title->basename (jp "/title" page))))
           (when ((length current-basenames) . > . 0)
             ;; Report
             (if (equal? redir-filter "nonredirects")
                 (callback (+ (length basenames) (length current-basenames) total) num-pages (last current-basenames))
                 (callback total num-pages (last current-basenames))))
           ;; Loop
           (loop (jp "/continue/apcontinue" json #f) (append basenames current-basenames))]
          [else
           ;; All done with this (loop)! Save those pages into the database
           ;; SQLite can have a maximum of 32766 parameters in a single query
           (begin0
               ;; next for*/fold
               (if (equal? redir-filter "nonredirects")
                   (+ (length basenames) total)
                   total) ; redirects don't count for the site statistics total
             (call-with-transaction
              (get-slc)
              (λ ()
                (for ([slice (in-slice 32760 basenames)])
                  (define query-template
                    (string-join #:before-first "insert or ignore into page (wikiname, redirect, basename, progress) values "
                                 (make-list (length slice) "(?1, ?2, ?, 0)") ", "))
                  (apply query-exec* query-template wikiname (if (equal? redir-filter "redirects") 1 sql-null) slice)))))])))
    ;; Record that we have the complete list of pages
    (query-exec* "update wiki set progress = 1 where wikiname = ?" wikiname)))


;; 2. Download each page via API and:
;; * Save API response to file
(define max-page-progress 1)
(define (save-each-page wikiname callback)
  ;; prepare destination folder
  (define save-dir (build-path archive-root wikiname))
  (make-directory* save-dir)
  ;; gather list of basenames to download (that aren't yet complete)
  (define basenames (query-list* "select basename from page where wikiname = ? and progress < ? and redirect is null"
                                wikiname max-page-progress))
  ;; counter of complete/incomplete basenames
  (define already-done-count
    (query-value* "select count(*) from page where wikiname = ? and progress = ?"
                 wikiname max-page-progress))
  (define not-done-count
    (query-value* "select count(*) from page where wikiname = ? and progress < ?"
                  wikiname max-page-progress))
  (define total-count (+ already-done-count not-done-count))
  ;; set initial progress
  (callback already-done-count total-count "")
  ;; loop through basenames and download
  (for ([basename basenames]
        [i (in-naturals (add1 already-done-count))])
    (define name-for-query (basename->name-for-query basename))
    (define dest-url
      (format "https://~a.fandom.com/api.php?~a"
              wikiname
              (params->query `(("action" . "parse")
                               ("page" . ,name-for-query)
                               ("prop" . "text|headhtml|langlinks")
                               ("formatversion" . "2")
                               ("format" . "json")))))
    (define r (get dest-url))
    (define body (response-body r))
    (define filename (string-append basename ".json"))
    (define save-path
      (cond [((string-length basename) . > . 240)
             (define key (sha1 (string->bytes/latin-1 basename)))
             (query-exec* "insert into special_page (wikiname, key, basename) values (?, ?, ?)"
                         wikiname key basename)
             (build-path save-dir (string-append key ".json"))]
            [#t
             (build-path save-dir (string-append basename ".json"))]))
    (display-to-file body save-path #:exists 'replace)
    (query-exec* "update page set progress = 1 where wikiname = ? and basename = ?"
                wikiname basename)
    (callback i total-count basename))
  ;; save redirects as well
  (save-redirects wikiname callback (+ already-done-count (length basenames)) total-count)
  ;; saved all pages, register that fact in the database
  (query-exec* "update wiki set progress = 2 where wikiname = ? and progress <= 2" wikiname))


;; 2.5. Download each redirect-target via API and save mapping in database
(define (save-redirects wikiname callback already-done-count total-count)
  (define basenames (query-list* "select basename from page where wikiname = ? and progress < ? and redirect = 1"
                                 wikiname max-page-progress))
  ;; loop through basenames, in slices of 50 (MediaWiki API max per request), and download
  (for ([basename basenames]
        [i (in-naturals (add1 already-done-count))])
    (define dest-url
      (format "https://~a.fandom.com/api.php?~a"
              wikiname
              (params->query `(("action" . "query")
                               ("prop" . "links")
                               ("titles" . ,(basename->name-for-query basename))
                               ("format" . "json")
                               ("formatversion" . "2")))))
    (define res (get dest-url))
    (define json (response-json res))
    (define dest-title (jp "/query/pages/0/links/0/title" json #f))
    (callback i total-count basename)
    (cond
      [dest-title
       ;; store it
       (define dest-basename (title->basename dest-title))
       (query-exec* "update page set progress = 1, redirect = ? where wikiname = ? and basename = ?" dest-basename wikiname basename)]
      [else
       ;; the page just doesn't exist
       (query-exec* "delete from page where wikiname = ? and basename = ?" wikiname basename)])))


;; 3. Download CSS and:
;; * Save CSS to file
;; * Record style images to database
(define (if-necessary-download-and-check-styles wikiname callback)
  (define wiki-progress (query-maybe-value* "select progress from wiki where wikiname = ?" wikiname))
  (unless (and (number? wiki-progress) (wiki-progress . >= . 3))
    (define styles (download-styles-for-wiki wikiname callback))
    (define unique-image-urls
      (remove-duplicates
       (map image-url->values
            (flatten
             (for/list ([style styles])
               (check-style-for-images wikiname (cdr style)))))
       #:key cdr))
    (for ([pair unique-image-urls])
      (query-exec* "insert or ignore into image (wikiname, url, hash, ext, source, progress) values (?, ?, ?, NULL, 1, 0)" wikiname (car pair) (cdr pair)))
    (query-exec* "update wiki set progress = 3 where wikiname = ?" wikiname)))


;; 4: From downloaded pages, record URLs of image sources and inline style images to database
(define (check-json-for-images wikiname path)
  (define data (with-input-from-file path (λ () (read-json))))
  (define page (html->xexp (preprocess-html-wiki (jp "/parse/text" data))))
  (define tree (update-tree-wiki page wikiname))
  null
  #;(remove-duplicates
     (for/list ([element (in-producer
                          (query-selector
                           (λ (t a c)
                             (and (eq? t 'img)
                                  (get-attribute 'src a)))
                           tree)
                          #f)])
       (image-url->values (get-attribute 'src (bits->attributes element))))))


;; 5. Download image sources and style images according to database
(define (save-each-image wikiname callback)
  (define source (hash-ref sources 'style)) ;; TODO: download entire wiki images instead?
  ;; gather list of basenames to download (that aren't yet complete)
  (define rows (query-rows* "select url, hash from image where wikiname = ? and source <= ? and progress < 1"
                                wikiname source))
  ;; counter of complete/incomplete basenames
  (define already-done-count
    (query-value* "select count(*) from image where wikiname = ? and source <= ? and progress = 1"
                 wikiname source))
  (define not-done-count
    (query-value* "select count(*) from image where wikiname = ? and source <= ? and progress < 1"
                 wikiname source))
  ;; set initial progress
  (callback already-done-count (+ already-done-count not-done-count) "")
  ;; loop through urls and download
  (for ([row rows]
        [i (in-naturals 1)])
    ;; row fragments
    (define url (vector-ref row 0))
    (define hash (vector-ref row 1))
    ;; check
    #;(printf "~a -> ~a~n" url hash)
    (define r (get url #:timeouts (make-timeout-config #:connect 15)))
    (define declared-type (response-headers-ref r 'content-type))
    (define final-type (if (equal? declared-type #"application/octet-stream")
                           (let ([sniff-entity (message-entity (mime-analyze (response-body r)))])
                             (string->bytes/latin-1 (format "~a/~a" (entity-type sniff-entity) (entity-subtype sniff-entity))))
                           declared-type))
    (define ext
      (with-handlers ([exn:fail:contract? (λ _ (error 'save-each-image "no ext found for mime type `~a` in file ~a" final-type url))])
        (bytes->string/latin-1 (mime-type->ext final-type))))
    ;; save
    (define save-dir (hash->save-dir wikiname hash))
    (make-directory* save-dir)
    (define save-path (build-path save-dir (string-append hash "." ext)))
    (define body (response-body r))
    (display-to-file body save-path #:exists 'replace)
    (query-exec* "update image set progress = 1, ext = ? where wikiname = ? and hash = ?"
                ext wikiname hash)
    (callback (+ already-done-count i) (+ already-done-count not-done-count) (string-append (substring hash 0 6) "..." ext)))
  ;; saved all images, register that fact in the database
  (query-exec* "update wiki set progress = 4 where wikiname = ?" wikiname))

(define all-stages
  (list
   if-necessary-download-list-of-pages
   save-each-page
   if-necessary-download-and-check-styles
   ;; check-json-for-images
   save-each-image))

(module+ test
  (check-equal? (html->xexp "<img src=\"https://example.com/images?src=Blah.jpg&amp;width=150\">")
                '(*TOP* (img (@ (src "https://example.com/images?src=Blah.jpg&width=150")))))
  #;(download-list-of-pages "minecraft" values)
  #;(save-each-page "minecraft" values)
  #;(check-json-for-images "chiki" (build-path archive-root "chiki" "Fiona.json"))
  #;(do-step-3 "gallowmere")
  #;(save-each-image "gallowmere" (hash-ref sources 'style) (λ (a b c) (printf "~a/~a ~a~n" a b c)))

  #;(for ([wikiname (query-list* "select wikiname from wiki")])
      (println wikiname)
      (insert-wiki-entry wikiname))

  #;(for ([wikiname (query-list* "select wikiname from wiki")])
      (println wikiname)
      (do-step-3 wikiname)
      (save-each-image wikiname (hash-ref sources 'style) (λ (a b c) (printf "~a/~a ~a~n" a b c)))))

; (for ([stage all-stages]) (stage "create" (λ (a b c) (printf "~a/~a ~a~n" a b c))))
