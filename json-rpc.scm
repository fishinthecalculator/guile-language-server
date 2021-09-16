(define-module (json-rpc)
  #:use-module (web http)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 iconv)
  #:use-module (json)

  #:export (<response>
            make-response
            response?
            response-id
            response-result
            response-error

            <request>
            make-request
            request?
            request-id
            request-method
            request-params

            read-message
            send-message
            send-result
            send-error
            send-notification

            parse-error
            invalid-request
            method-not-found
            invalid-params
            internal-error
            server-error-start
            server-error-end
            server-not-initialized
            unknown-error-code))

(define-record-type <response>
  (make-response id result error)
  response?
  (id response-id)
  (result response-result)
  (error response-error))

(define-record-type <request>
  (make-request id method params)
  request?
  (id request-id)
  (method request-method)
  (params request-params))

(define (read-to-crnl port)
  (define (aux acc)
    (define b (get-u8 port))
    (if (and (= (char->integer #\cr) b)
             (= (char->integer #\lf) (lookahead-u8 port)))
        (begin
          (get-u8 port) ;; Consume \n
          (bytevector->string (u8-list->bytevector (reverse acc)) "ascii"))
        (aux (cons b acc))))
  (aux '()))

(define (read-headers port)
  (define (aux acc)
    (define line (read-to-crnl port))
    (if (equal? line "")
        acc
        (let* ((i (string-index line #\:))
               (key (string-take line i))
               (value (string-drop line (+ i 2))))
          (aux (cons (cons key value) acc)))))
  (aux '()))

(define (get-content-length headers)
  (string->number (assoc-ref headers "Content-Length")))

(define (get-content-encoding headers)
  (define header (assoc-ref headers "Content-Type"))
  (define encoding
    (if (eq? header #f)
        "utf-8"
        (or (assoc-ref (cdr (parse-header 'content-type header)) 'charset)
            "utf-8")))
  ;; for backwards compatibility treat utf8 as utf-8
  (if (equal? encoding "utf8") "utf-8" encoding))

(define (read-content headers port)
  (define len (get-content-length headers))
  (define encoding (get-content-encoding headers))
  (bytevector->string (get-bytevector-n port len) encoding))

(define (parse-content content)
  (define root (json-string->scm content))
  ;; TODO verify jsonrpc version
  (if (eq? (hash-ref root "method") #f)
      (make-response
       (hash-ref root "id")
       (hash-ref root "result")
       (hash-ref root "error"))
      (make-request
       (hash-ref root "id")
       (hash-ref root "method")
       (hash-ref root "params"))))

(define (read-message port)
  (define headers (read-headers port))
  (define content (read-content headers port))
  (define tmp (parse-content content))
  (display tmp) (display "\n")
  tmp)

(define (send-message port response)
  (define json (cons '(jsonrpc . "2.0") response))
  (define encoding "utf-8")
  (define body (string->bytevector (scm->json-string json) encoding))
  (define header-string
    (string-append
     "Content-Length: " (number->string (bytevector-length body)) "\r\n"
     "Content-Type: application/vscode-jsonrpc; charset=" encoding "\r\n"
     "\r\n"))
  (define header (string->bytevector header-string "ascii"))
  (put-bytevector port header)
  (put-bytevector port body)
  (force-output port))

(define (send-notification port method params)
  (send-message port `((method . ,method) (params . ,params))))

;; Defined by JSON RPC
(define parse-error -32700)
(define invalid-request -32600)
(define method-not-found -32601)
(define invalid-params -32602)
(define internal-error -32603)
(define server-error-start -32099)
(define server-error-end -32000)
(define server-not-initialized -32002)
(define unknown-error-code -32001)

(define* (send-error port requestId errorId errorMessage #:optional (data #nil))
  (define error
    `((code . ,errorId)
      (message . ,errorMessage)
      ,@(if (eq? data #nil) '() `(data . ,data))))
  (send-message port `((id . ,requestId) (error . ,error))))

(define (send-result port requestId result)
  (send-message port `((id . ,requestId) (result . ,result))))
