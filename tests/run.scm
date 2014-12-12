(use transit test utils)


(define example.sexp (with-input-from-file "example.sexp" read))
(define example.json (with-input-from-file "example.json" read-all))
(define example.verbose.json (with-input-from-file "example.verbose.json" read-all))

(test-begin "transit test")

(test "parsing transit"
      #t
      (equal? example.sexp
	      (with-input-from-file "example.json" read-transit)))

(test "parsing verbose transit"
      #t
      (equal? example.sexp
	      (with-input-from-file "example.verbose.json" read-transit)))

(test "generating transit"
      example.json
      (with-output-to-string
	(lambda () 
	  (write-transit example.sexp)
	  (newline))))

(test "generating verbose transit"
      example.verbose.json
      (with-output-to-string
	(lambda () 
	  (write-transit example.sexp #t)
	  (newline))))

(test-end)
(test-exit)
