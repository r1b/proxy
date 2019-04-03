(module proxy (start)
  (import (chicken base)
          (chicken condition)
          (chicken io)
          (chicken port)
          intarweb
          scheme
          srfi-18
          tcp6)

  ; --------------------------------------------------------------------------

  ; a thread-local storage is a pair of pairs:
  ; ((client-in . client-out) . (or (server-in . server-out) void))

  ; FIXME dedupe

  ; void -> Port
  (define (client-in) (car (car (thread-specific (current-thread)))))

  ; void -> Port
  (define (client-out) (cdr (car (thread-specific (current-thread)))))

  ; void -> Port
  (define (server-in)
    (let ((server-ports (cdr (thread-specific (current-thread)))))
      (if (pair? server-ports) (car server-ports) (void))))

  ; void -> Port
  (define (server-out)
    (let ((server-ports (cdr (thread-specific (current-thread)))))
      (if (pair? server-ports) (cdr server-ports) (void))))

  ; --------------------------------------------------------------------------

  ; utilities for dealing with bodies
  ; FIXME dedupe

  (define (chunked-request? request)
    (memq 'chunked (header-values 'transfer-encoding (request-headers request))))

  (define (request-length request)
    (header-value 'content-length (request-headers request)))

  (define (chunked-response? response)
    (memq 'chunked (header-values 'transfer-encoding (response-headers response))))

  (define (response-length response)
    (header-value 'content-length (response-headers response)))

  ; XXX This is confusing as fuck but we need the original request / response
  ;     so that intarweb will handle chunking for us. Revisit this.

  (define (write-request-body server-request proxy-request)
    (display (if (chunked-request? server-request)
                 (read-string #f (request-port proxy-request))
                 (read-string (request-length proxy-request)
                              (request-port proxy-request)))
             (request-port server-request)))

  (define (write-response-body client-response proxy-response)
    (display (if (chunked-response? client-response)
                 (read-string #f (response-port proxy-response))
                 (read-string (response-length proxy-response)
                              (response-port proxy-response)))
             (response-port client-response)))


  ; --------------------------------------------------------------------------

  ; void -> void
  (define (cleanup)
    (begin
      (close-input-port (client-in))
      (close-output-port (client-out))
      (when (port? (server-in))
        (close-input-port (server-in)))
      (when (port? (server-out))
        (close-output-port (server-out)))))

  ; Request -> void
  (define (forward-request proxy-request)
    (let ((host-port (car (header-values 'host (request-headers proxy-request)))))
      (let*-values (((host port) (values (car host-port) (cdr host-port)))
                    ((in out) (tcp-connect host (or port 80))))
        (begin
          (thread-specific-set! (current-thread)
                                (cons (cons (client-in) (client-out))
                                      (cons in out)))
          (let* ((server-request
                   (write-request (update-request proxy-request port: (server-out)))))
            (begin
              (when ((request-has-message-body?) server-request)
                (write-request-body server-request proxy-request)
                (finish-request-body server-request))
              server-request))))))

  ; Response -> void
  (define (forward-response proxy-response request)
    (let* ((client-response (write-response (update-response proxy-response port: (client-out))))
           (_ (when ((response-has-message-body-for-request?) client-response request)
                (write-response-body client-response proxy-response)
                (finish-response-body client-response))))
      (void)))

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

  ; TODO: implement CONNECT for e.g HTTPS
  ; XXX: intarweb (perhaps wisely) does not support CONNECT
  ; we can add this ourselves

  ; FIXME: Cleanup client / server i/o on exceptions

  ; void -> void
  (define (handle-connection)
    (begin
      (let ((request (read-request (client-in))))
        (when request
          (let ((forwarded-request (forward-request (apply-rules-to-request request)))
                (response (read-response (server-in))))
            (when response
              (forward-response (apply-rules-to-response response) forwarded-request)))))
      (cleanup)))

  ; TODO: signals
  (define (serve-forever listener)
    (let-values (((in out) (tcp-accept listener)))
      (let ((t (make-thread handle-connection 'handle-connection)))
        (begin
          (thread-specific-set! t (cons (cons in out) (void)))
          (thread-start! t)
          (serve-forever listener)))))

  ; number? number? string? -> void
  (define (start #!optional (port 4242) (backlog 1000) (host "127.0.0.1"))
    (let ((listener (tcp-listen port backlog host)))
      (serve-forever listener))))
