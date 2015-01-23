;;; I/O


; Questions:
;
; - What makes a value "cacheable"?
; - Are cache refs always raw strings?
; - Encode/decode is not representation preserving
; - Verbose mode encoding ignores backrefs
; - map-parsing is not tail-recursive


;; port-I/O

(define (read-transit #!optional (port (current-input-port)))
  (receive (result remainder)
    (read-json port)
    (values (and result (transit-decode result))
            remainder)))

(define (write-transit data #!optional verbose (port (current-output-port)))
  (write-json (transit-encode data verbose: verbose) port))


;; encoding of data to transit

(define current-transit-write-handlers (make-parameter '()))

(defstruct write-handler
  tag
  rep
  string-rep
  (verbose-handler #f))			;XXX currently unused


;; condition type for encode-errors

(define (encoding-error msg . args)
  (abort 
   (make-composite-condition 
    (make-property-condition 'transit)
    (make-property-condition 'encode)
    (make-property-condition 'exn 'message msg 'arguments args))))

(cond-expand
  (compiling
   (begin
     (define isnan? (foreign-lambda bool "C_isnan" double))
     (define isinf? (foreign-lambda bool "C_isinf" double))))
  (else
   (begin
     (define isnan? (constantly #f))
     (define isinf? (constantly #f)))))

(define (transit-encode data #!key
			(toplevel #t)
			(verbose #f)
			(write-handlers (current-transit-write-handlers)))
  (let ((cache '())
	(cache-index 0))
    (define (key->symbol x)
      (cond ((symbol? x) x)
	    ((string? x) (string->symbol x))
	    (else (error "bad key" x))))
    (define (cached data default)
      (cond ((and (not verbose) (assq data cache)) => cdr)
	    (default (encache data default))
	    (else data)))
    (define (lookup-handler data)
      (let loop ((hs write-handlers))
	(match hs
	  (() #f)
	  (((pred . wh) . more)
	   (if (pred data) 
	       wh
	       (loop more))))))
    (define (encache key val)
      (if (not verbose)
	  (let ((i cache-index))
	    (inc! cache-index)
	    (push! (cons key (index->cache-code i)) cache)
	    val)
	  val))
    (define (encode-key/val pair)
      (match pair
	((key . val) 
	 (values
	  (let ((key2 (cond ((assq key cache) => cdr)
			    (else
			     (if (and (symbol? key)
				      (not (keyword? key)))
				 (if (> (string-length (symbol->string key)) 3)
				     (encache key key)
				     key)
				 (encode key #t))))))
	    (if verbose
		(key->symbol key2)
		key2))
	  (encode val #f)))
	(_ (encoding-error "invalid object data" data))))
    (define (encode data key?)
      ;(pp `(ENCODE: ,data ,cache))
      (cond ((eq? data 'null) (if key? "~_" 'null))
	    ((boolean? data)
	     (if key?
		 (string-append "~?" (if data "t" "f"))
		 data))
	    ((number? data)
	     (cond ((isnan? data) "~zNaN")
		   ((isinf? data) (if (positive? data) "~zINF" "~z-INF"))
		   ((not key?) data)
		   ((exact? data)
		    (string-append "~i" (number->string data)))
		   (else (string-append "~d" (number->string data)))))
	    ((keyword? data) 
	     (cached data (string-append "~:" (symbol->string data))))
	    ((symbol? data)
	     (cached data (string-append "~$" (symbol->string data))))
	    ((lookup-handler data) =>
	     (lambda (wh)
	       (let* ((tag ((write-handler-tag wh) data))
		      (rep ((if key?
				(write-handler-string-rep wh)
				(write-handler-rep wh)) data)))
		 (if key?
		     rep
		     (let ((t (string->symbol (string-append "~#" tag))))
		       (vector (cached t #f) rep))))))
	    ((pair? data)
	     (if verbose
		 (map (lambda (k/v)
			(let-values (((k v) (encode-key/val k/v)))
			  (cons k v)))
		      data)
		 (let* ((len (length data))
			(vec (make-vector (add1 (* 2 len)) "^ ")))
		   (do ((i 1 (+ i 2))
			(data data (cdr data)))
		       ((null? data))
		     (let-values (((k v) (encode-key/val (car data))))
		       (vector-set! vec i k)
		       (vector-set! vec (add1 i) v)))
		   vec)))
	    ((vector? data)
	     (let* ((len (vector-length data))
		    (data2 (make-vector len)))
	       (do ((i 0 (add1 i)))
		   ((>= i len))
		 (vector-set! data2 i (encode (vector-ref data i) #f)))
	       data2))
	    (else data)))
    (let ((x (encode data #f)))
      ;(pp `(CACHE: ,cache))
      (if (and toplevel (not (vector? x)) (not (pair? x)))
	  (vector "~#'" x)
	  x))))


;; condition type for decode-errors

(define (decoding-error msg . args)
  (abort 
   (make-composite-condition 
    (make-property-condition 'transit)
    (make-property-condition 'decode)
    (make-property-condition 'exn 'message msg 'arguments args))))


;; fallback record for unknown tagged values

(define-record-type tagged-value
  (make-tagged-value tag value)
  tagged-value?
  (tag tagged-value-tag)
  (value tagged-value-ref))


;; decoding of transit to data

(define current-transit-read-handlers (make-parameter '()))

(define (transit-decode data #!optional (read-handlers (current-transit-read-handlers)))
  (let* ((cache-len 100)
	 (cache-index 0)
	 (cache (make-vector cache-len #f)))
    (define (ensure-length data len min max)
      (when (if max (or (< len min) (> len max)) (= len min))
	(decoding-error "datum encoded in string of invalid length" data)))
    (define (lookup-handler tag)
      (cond ((assq tag read-handlers) => cdr)
	    (else #f)))
    (define (encache x)
      (when (>= cache-index cache-len)
	(set! cache-len (* 2 cache-len))
	(set! cache (vector-resize cache cache-len)))
      (vector-set! cache cache-index x)
      (inc! cache-index)
      x)
    (define (cached ref)
      (or (vector-ref cache (cache-code->index ref))
	  (decoding-error "invalid cache reference" ref)))
    (define (decode data key?)
      (cond ((symbol? data)
	     (if key?
		 (let ((x (decode (symbol->string data) #t)))
		   (if (string? x)
		       (string->symbol x)
		       x))
		 data))
	    ((string? data)
	     (let ((len (string-length data)))
	       (if (positive? len)
		   (case (string-ref data 0)
		     ((#\~)
		      (if (> len 1)
			  (let ((tag (string-ref data 1)))
			    (case tag
			      ((#\~ #\^ #\`) (substring data 1))
			      ((#\_) 'null)
			      ((#\s)
			       (let ((str (substring data 2)))
				 (if (and key? (> (string-length str) 3))
				     (encache str)
				     str)))
			      ((#\?) 
			       (ensure-length data len 3 #f)
			       (case (string-ref data 2)
				 ((#\t) #t)
				 ((#\f) #f)
				 (else (decoding-error "invalid encoding" data))))
			      ((#\i #\d) (string->number (substring data 2)))
			      ((#\b) (base64-decode (substring data 2)))
			      ((#\:) (encache (string->keyword (substring data 2))))
			      ((#\$) (encache (string->symbol (substring data 2))))
			      ((#\f) 
			       ((or (lookup-handler #\f)
				    (lambda _
				      (decoding-error
				       "unable to decode arbitrary precision decimal"
				       data)))
				(substring data 2)))
			      ((#\n)
			       ((or (lookup-handler #\n)
				    (lambda _
				      (decoding-error
				       "unable to decode arbitrary precision integer"
				       data)))
				(substring data 2)))
			      ((#\m)
			       ((or (lookup-handler #\m) string->number)
				(substring data 2)))
			      ((#\t)
			       ((or (lookup-handler #\t)
				    (lambda _
				      (decoding-error
				       "unable to decode point in time"
				       data)))
				(substring data 2)))
			      ((#\u #\r) 
			       ((or (lookup-handler tag) identity)
				(substring data 2)))
			      ((#\c) 
			       (ensure-length data len 3 #f)
			       (string-ref data 2))
			      ((#\z)
			       (let ((str (substring data 2)))
				 (cond ((string-ci=? "nan" str) +nan.0)
				       ((string-ci=? "+inf" str) +inf.0)
				       ((string-ci=? "+inf" str) -inf.0)
				       (else 
					(decoding-error
					 "unable to decode special number" data)))))
			      ((#\#) data) ; return unchanged
			      (else
			       (if (char-upper-case? tag)
				   ((or (lookup-handler tag)
					(lambda _
					  (decoding-error 
					   "unable to decode app-specific data"
					   data)))
				    (substring data 2))
				   (decoding-error
				    "unable to decode - unreconized tag" data)))))
			  (decoding-error "unescaped ~" data)))
		     ((#\^)
		      (if (string=? "^ " data)
			  data
			  (cached data)))
		     ((#\`)
		      (decoding-error "unable to decode backquote extension" data))
		     (else (if key? (encache data) data)))
		   (if (and key? (> (string-length data) 3))
		       (encache data)
		       data))))
	    ((vector? data)
	     (let ((len (vector-length data)))
	       (if (> len 1)
		   (let ((head (decode (vector-ref data 0) #f)))
		     (cond ((equal? "~#'" head) (vector-ref data 1))
			   ((equal? "^ " head)
			    (let loop ((lst (cdr (vector->list data))))
			      (match lst
				(() '())
				((k v . more)
				 (alist-cons
				  (decode k #t)
				  (decode v #f)
				  (loop more)))
				(_ (decoding-error
				    "can not decode map with uneven key/value elements"
				    data)))))
			   ((and (string? head)
				 (> (string-length head) 1)
				 (string=? "~#" (substring head 0 2)))
			    (let ((tag (string->symbol (substring head 2))))
			      (if (= 2 len)
				  (let ((val (decode (vector-ref data 1) #f)))
				    ((or (lookup-handler tag)
					 (lambda _ (make-tagged-value tag val)))
				     val))
				  (decoding-error
				   "can not decode tagged value - invalid length"
				   data))))
			   (else
			    (list->vector
			     (cons head (map (cut decode <> #f)
					     (cdr (vector->list data))))))))
		   data)))
	    ((pair? data)
	     (let ((data (map (lambda (a)
				(cons (decode (car a) #t) (decode (cdr a) #f)))
			      data)))
	       (if (and (null? (cdr data)) 
			(pair? (car data))
			(symbol? (caar data)))
		   (let ((head (symbol->string (caar data))))
		     (if (and (> (string-length head) 1)
			      (string=? "~#" (substring head 0 2)))
			 (let ((tag (string->symbol (substring head 2)))
			       (val (cdar data)))
			   ((or (lookup-handler tag)
				(lambda _ (make-tagged-value tag val)))
			    val))
			 (decoding-error
			  "can not decode tagged value - invalid length"
			  data)))
		   data)))
	    (else data)))
    (decode data #f)))


;; cache-code calculation

(define (index->cache-code index)
  (string-append
   "^" 
   (let ((hi (quotient index 44)))
     (if (zero? hi)
	 ""
	 (string (integer->char (+ 48 hi)))))
   (let ((lo (remainder index 44)))
     (string (integer->char (+ 48 lo))))))

(define (cache-code->index ccode)
  (match (string->list ccode)
    ((#\^ c) (- (char->integer c) 48))
    ((#\^ hi lo)
     (+ (* 44 (- (char->integer hi) 48))
	(- (char->integer lo) 48)))
    (_ (error "invalid cache code" ccode))))
