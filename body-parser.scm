(include "body-parser-functor")

(module request-body-parser = body-parser-functor
  (import intarweb scheme)
  (define message-port request-port)
  (define message-headers request-headers))

(module response-body-parser = body-parser-functor
  (import intarweb scheme)
  (define message-port response-port)
  (define message-headers response-headers))
