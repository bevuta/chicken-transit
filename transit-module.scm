(module transit (read-transit
		 write-transit
		 transit-encode
		 transit-decode
		 current-transit-write-handlers
		 current-transit-read-handlers
		 make-tagged-value
		 tagged-value?
		 tagged-value-tag
		 tagged-value-ref
		 make-write-handler)
  (import scheme chicken miscmacros matchable defstruct foreign)
  (use medea base64 data-structures srfi-1)
  (include "transit.scm"))
