;; Manages a cache of previous stock prices.
;; Single snapshots from last time this was ran.
(require 'uiop)
(ql:quickload "osicat")

;; Cache file name
(defconstant cache-file "~/.stonks.cache")
(defconstant day-names
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday"))

(defun write-cache (stocks-list)
  (with-open-file
      (stream cache-file
	      :direction :output
	      :if-exists :supersede
	      :if-does-not-exist :create)
      ;; First write the date/time that we got this data.
      (write-string (format nil "~a~%" (get-universal-time)) stream)
      ;; Then write in the stock information.
      ;; Each stock on a seperate line terminated by a newline.
      (loop for stock in stocks-list
	  do 
	     (write-string (format nil "~{~a ~}~%" stock) stream)
	  )))

(defun read-cache ()
  (let ((split-string '())
	(cache-hash-table (make-hash-table :test 'equal)))
    (with-open-file
	(stream cache-file
		:direction :input
		:if-does-not-exist :error
		)
	;; Get universal time string from cache
	;; then decode and put into human readable
	;; format.
	(setf (gethash "date" cache-hash-table)
	      (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
	 (decode-universal-time
	  (parse-integer (read-line stream nil)))
		(declare (ignore dst-p))
		(format nil "Cache Date: ~2,'0d:~2,'0d:~2,'0d on ~a, ~d/~2,'0d/~d (GMT~@d)"
			hour minute second (nth day-of-week day-names) date month year (- tz))))

	;; Get everything else after the first line
	;; which should be stock information.
	(loop for line = (read-line stream nil)
	    until (eq line nil)
	    do (progn (setq split-string (last (uiop:split-string line :separator " ") 3))
		       (setf (gethash (elt split-string 0) cache-hash-table) (elt split-string 1))
		       )
	    
		   )
    )
    cache-hash-table))


;;; Tests
(defun test-write-cache ()
  (let ((test-list
	  '(
	    ("Bitcoin USD (BTC-USD)" "27,710.71")
	    ("Bitcoin GBP (BTC-GBP)" "20,379.15")
	    ("Ethereum GBP (ETH-GBP)" "531.59")
	    ("Apple Inc. (AAPL)" "134.87"))))
    (write-cache test-list)
    ))
