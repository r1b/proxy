(module proxy (start)
  (import (chicken base)
          (chicken condition)
          (chicken port)
          intarweb
          scheme
          srfi-18
          tcp6)

  ; XXX: thread-local storage is a pair (client . server)

  ; void -> Port
  (define (client) (car (thread-specific (current-thread))))

  ; void -> Port
  (define (server) (cdr (thread-specific (current-thread))))

  ; void -> void
  (define (cleanup)
    (begin
      (close-input-port (client))
      (close-output-port (client))
      (close-input-port (server))
      (close-output-port (server))))

  ; Request -> void
  (define (forward-request request)
    (let*-values (((host port) (header-values 'host (request-headers request)))
                  ((in out) (tcp-connect host (or port 80))))
      (begin
        (thread-specific-set! (current-thread)
                              (cons (client)
                                    (make-bidirectional-port in out)))
        (let* ((server-request
                 (write-request (update-request request '(port ,(server))))))
          (if (request-has-message-body? server-request)
              (finish-request-body server-request)
              server-request)))))

  ; Response -> void
  (define (forward-response response request)
    (let* ((proxy-response (write-response (update-response response '(port ,(client)))))
           (_ (if (response-has-message-body-for-request? proxy-response request)
                  (finish-response-body proxy-response)
                  proxy-response)))
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
    (let ((request (read-request (client))))
      (when request
        (let ((forwarded-request (forward-request (apply-rules-to-request request)))
              (response (read-response (server))))
          (when response
            (forward-response (apply-rules-to-response response) forwarded-request)))
        (cleanup))))

  ; TODO: signals
  (define (serve-forever listener)
    (let-values (((in out) (tcp-accept listener)))
      (let ((t (make-thread handle-connection 'handle-connection)))
        (begin
          (thread-specific-set! t (cons (make-bidirectional-port in out) (void)))
          (thread-start! t)
          (serve-forever listener)))))

  ; number? number? string? -> void
  (define (start #!optional (port 4242) (backlog 1000) (host "127.0.0.1"))
    (let ((listener (tcp-listen port backlog host)))
      (serve-forever listener))))
