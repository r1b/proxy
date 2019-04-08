; Blatant ripoff of https://github.com/expressjs/body-parser

(include "body-parser-interface")
(include "transfer-encoding")

(functor (body-parser (M body-parser-interface))

  (parse-body
    parse-json
    ; parse-multipart
    parse-string
    parse-urlencoded
    unparse-body
    unparse-json
    ; unparse-multipart
    unparse-string
    unparse-urlencoded)

  (import M medea scheme transfer-encoding uri-common)

  (define (message-length headers)
    (header-value 'content-length headers))

  (define (has-transfer-encoding? value headers)
    (memq value (header-values 'transfer-encoding headers)))

  (define (read-message-body message)
    (let* ((port (message-port message))
           (headers (message-headers message))
           (raw-body (if (has-transfer-encoding? 'chunked headers)
                         ; XXX: We delegate chunk handling to intarweb
                         (read-string #f port)
                         ; XXX: Reading #f or any l > Content-Length blocks!
                         (read-string (message-length headers) port))))
      (cond
        ((has-transfer-encoding? 'deflate headers) (decode-deflate raw-body))
        ((has-transfer-encoding? 'gzip headers) (decode-gzip raw-body))
        (else raw-body))))

  (define (write-message-body raw-body message)
    (let* ((port (message-port message))
           (headers (message-headers message))
           (encoded-body
             (cond
               ((has-transfer-encoding? 'deflate headers) (encode-deflate raw-body))
               ((has-transfer-encoding? 'gzip headers) (encode-gzip raw-body))
               (else raw-body))))
      (if (has-transfer-encoding? 'chunked headers)
          (write-string encoded-body #f port)
          (write-string (message-length headers) port))))

  ; application/json
  (define (parse-json message)
    (read-json (read-message-body message)))

  (define (unparse-json json message)
    (write-message-body (json->string json) message)))
