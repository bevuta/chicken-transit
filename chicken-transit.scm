;;;; simple command-line client for transit encoding/decoding


(module main ()
  (import scheme chicken miscmacros matchable)
  (use transit extras)

  (define (detect-mode in)
    (let loop ()
      (let ((c (peek-char in)))
	(cond ((eof-object? c) #f)
	      ((char-whitespace? c) 
	       (read-char in)
	       (loop))
	      ((memq c '(#\[ #\})) 'decode)
	      (else 'encode)))))

  (define-record-printer (tagged-value x port)
    (fprintf port "#<tagged-value ~s : ~s>" 
      (tagged-value-tag x)
      (tagged-value-value x)))

  (define (usage code)
    (print "usage: chicken-transit [-h] [-d] [-e] [FILENAME ...]")
    (exit code))

  (define (main args)
    (let ((mode #f)
	  (files '()))
      (define (encode/decode files)
	(for-each
	 (lambda (fn)
	   (let ((in (if (string=? "-" fn)
			 (current-input-port)
			 (open-input-file fn))))
	     (unless mode
	       (set! mode (detect-mode in)))
	     (case mode
	       ((encode) (encode in))
	       ((decode) (decode in)))
	     (unless (string=? "-" fn)
	       (close-input-port in))))
	 files))
      (define (decode port)
	(pp (read-transit port)))
      (define (encode port)
	(let loop ()
	  (let ((x (read port)))
	    (unless (eof-object? x)
	      (write-transit x)
	      (loop)))))
      (let loop ((args args))
	(match args
	  (() (encode/decode (reverse files)))
	  (((or "-h" "-help" "--help") . _)
	   (usage 0))
	  (("-d" . more)
	   (set! mode 'decode)
	   (loop more))
	  (("-e" . more)
	   (set! mode 'encode)
	   (loop more))
	  ((fn . more)
	   (cond ((and (> (string-length fn) 1)
		       (char=? #\- (string-ref fn 0)))
		  (usage 1))
		 (else
		  (push! fn files)
		  (loop more))))))))

  (main (command-line-arguments)))
