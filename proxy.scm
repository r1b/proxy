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

  ; i/o

  (define (read-with-buffer port)
    (read-string #f port))

  (define (write-with-buffer chunk port)
    (write-string chunk #f port))

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
  (define (forward-request request)
    (let ((host-port (car (header-values 'host (request-headers request)))))
      (let*-values (((host port) (values (car host-port) (cdr host-port)))
                    ((in out) (tcp-connect host (or port 80))))
        (begin
          ; FIXME
          (thread-specific-set! (current-thread)
                                (cons (cons (client-in) (client-out))
                                      (cons in out)))
          (let* ((server-request
                   (write-request (update-request request port: (server-out)))))
            (if ((request-has-message-body?) server-request)
                (begin
                  (write-string (read-string #f (client-in)) #f (server-out))
                  (finish-request-body server-request))
                server-request))))))

  ; Response -> void
  (define (forward-response response request)
    (let* ((proxy-response (write-response (update-response response port: (client-out))))
           (_ (when ((response-has-message-body-for-request?) proxy-response request)
                  ; FIXME this is soooo slow
                  (write-string (read-string #f (server-in)) #f (client-out))
                (finish-response-body proxy-response))))
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
