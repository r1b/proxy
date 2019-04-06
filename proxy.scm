(module proxy (start)
  (import (chicken base)
          (chicken condition)
          (chicken io)
          (chicken port)
          (chicken tcp)
          intarweb
          scheme
          srfi-18)

  ; --------------------------------------------------------------------------

  ; i/o
  ; FIXME: Connection: Keep-Alive

  (define-record session client server)

  (define (current-session) (thread-specific (current-thread)))
  (define (current-client) (session-client (current-session)))
  (define (current-server) (session-server (current-session)))

  ; void -> void
  (define (cleanup)
      (begin
        (close-input-port (current-client))
        (close-output-port (current-client))
        (when (port? (current-server))
          (close-input-port (current-server)))
        (when (port? (current-server))
          (close-output-port (current-server)))))

  ; --------------------------------------------------------------------------

  ; message utilities

  (define (message-length headers)
    (header-value 'content-length headers))

  (define (chunked-message? headers)
    (memq 'chunked (header-values 'transfer-encoding headers)))

  (define (write-message-body body-length chunkedp input-port output-port)
    (display (if chunkedp
                 ; XXX: We delegate chunk handling to intarweb
                 (read-string #f input-port)
                 (read-string body-length
                              input-port))
             output-port))

  ; --------------------------------------------------------------------------

  ; core

  ; Request -> void
  (define (forward-request proxy-request)
    (let ((host-port (car (header-values 'host (request-headers proxy-request)))))
      (let*-values (((host port) (values (car host-port) (cdr host-port)))
                    ((in out) (tcp-connect host (or port 80))))
        (begin
          (thread-specific-set! (current-thread)
                                (make-session (current-client)
                                              (make-bidirectional-port in out)))
          (let* ((server-request (write-request (update-request proxy-request
                                                                port: (current-server)))))
            (begin
              (when ((request-has-message-body?) server-request)
                (write-message-body (message-length (request-headers server-request))
                                    (chunked-message? (request-headers server-request))
                                    (request-port proxy-request)
                                    (request-port server-request))
                (finish-request-body server-request))
              server-request))))))

  ; Response -> void
  (define (forward-response proxy-response request)
    (let* ((client-response (write-response (update-response proxy-response
                                                             port: (current-client))))
           (_ (when ((response-has-message-body-for-request?) client-response request)
                (write-message-body (message-length (response-headers client-response))
                                    (chunked-message? (response-headers client-response))
                                    (response-port proxy-response)
                                    (response-port client-response))
                (finish-response-body client-response))))
      (void)))

  ; --------------------------------------------------------------------------

  ; rules

  ; TODO: implement `trap & modify`
  ; `trap` will be like:
  ; * if host matches whatever or request line matches whatever then trap
  ; `modify` will be like:
  ; * write request sexpr to /tmp/request-sexpr
  ; * lock & block on $EDITOR /tmp/request-sexpr
  ; * eval & forward /tmp/request-sexpr

  ; Request -> Request
  (define (apply-rules-to-request request) request)

  ; Response -> Response
  (define (apply-rules-to-response response) response)

  ; --------------------------------------------------------------------------

  ; process / thread entry points
  ; FIXME: Cleanup client / server i/o on exceptions

  ; void -> void
  (define (handle-connection)
    (begin
      (let ((request (read-request (current-client))))
        (when request
          (let ((forwarded-request (forward-request (apply-rules-to-request request)))
                (response (read-response (current-server))))
            (when response
              (forward-response (apply-rules-to-response response) forwarded-request)))))
      (cleanup)))

  ; TODO: signals
  (define (serve-forever listener)
    (let-values (((in out) (tcp-accept listener)))
      (let ((t (make-thread handle-connection 'handle-connection)))
        (begin
          (thread-specific-set! t (make-session (make-bidirectional-port in out) (void)))
          (thread-start! t)
          (serve-forever listener)))))

  ; number? number? string? -> void
  (define (start #!optional (port 4242) (backlog 1000) (host "127.0.0.1"))
    (let ((listener (tcp-listen port backlog host)))
      (serve-forever listener))))
