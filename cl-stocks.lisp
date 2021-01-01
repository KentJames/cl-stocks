(ql:quickload '("dexador" "lquery"))
(load "cl-stocks-cache.lisp") ; Has (require 'uiop) internally (used below)

;; Default stock url config file.
(defconstant url-file "~/.stonksrc") 
;; Default set of stocks to check if ~/.stonksrc doesn't exist.
(defconstant stock-url-list '("https://finance.yahoo.com/quote/FB"
			      "https://finance.yahoo.com/quote/AAPL/"
			      "https://finance.yahoo.com/quote/AMZN/"
			      "https://finance.yahoo.com/quote/NFLX"
			      "https://finance.yahoo.com/quote/GOOG/"
			      "https://finance.yahoo.com/quote/^GSPC"))


(defun get-url-list ()
  ;; We look for a list of url's in url-file (nominally ~/.stonksrc)
  ;; If the file doesn't exist return stock-url-list which has FAANG
  ;; stocks + S&P 500. (Make sure there's no blank lines after the
  ;; last url!)
  (handler-case (with-open-file
      (stream url-file
	      :direction :input
	      :if-does-not-exist :error)
    (loop for line = (read-line stream nil)
	  until (eq line nil)
	  collect line)
		  )
    ;; If there is no url-file then return our default.
    (error (c)
      stock-url-list))
  )


(defun get-stock-info (url)
  ;; Get HTML body for Url. Return NIL if no connection.
  (let ((html-body (handler-case (lquery:$ (initialize (dex:get url)))
		     (usocket:ns-host-not-found-error ()
		       (return-from get-stock-info)))))
    ;; Check HTML Body is not NIL. Return NIL if it is. 
    (if (eq html-body nil) (cons nil nil)
	;; Return a list of (STOCK-NAME STOCK-PRICE)
	`(,(elt (lquery:$ html-body "#quote-header-info h1" (text)) 0)
	  ,(elt (lquery:$ html-body "#quote-header-info span" (text)) 3)))
    ))

(defun get-last-stock-price (name-and-ticker cache-hash-table)
  ;; Split the name/ticker by space 
  (let ((split-name-and-ticker (uiop:split-string name-and-ticker :separator " "))
	(hash-value-return '()))
    ;; Check if our cache-hash-table is nil. If nil then there's no
    ;; history to work with, so return "No History"
    (if (eq cache-hash-table nil) (return-from get-last-stock-price "No History"))
    ;; Next check if the key we want exists in the hash table.
    ;; If it does not exist then return "No History" like above
    ;; Otherwise return the value.
    (setq hash-value-return (gethash (car (last split-name-and-ticker 1)) cache-hash-table))
    (if (eq hash-value-return nil)
	"No History"
	hash-value-return)
  ))

(defun print-stocks ()
  ;; Fetch HTML from URL's in url-list.
  (let ((stock-information (mapcar 'get-stock-info (get-url-list)))
	(stock-information-cached (ignore-errors (read-cache))))
    (if (eq (car (car stock-information)) nil)
	(format t "Connection to Yahoo Finance failed!~%")
	;; Print stocks and prices
	(progn
	  (unless (eq stock-information-cached nil)
	  (format t "~a ~%" (gethash "date" stock-information-cached)))
	  (loop for stock in stock-information
	      do (format t "~a Price: ~a Last (Cache) Price: ~a ~%"
			 (car stock)
			 (elt stock 1)
			 ;; Get history from cache.
			 (get-last-stock-price
			  (car stock)
			  stock-information-cached)))
	  (write-cache stock-information)
	  ))))


(defun main ()
  (print-stocks))
  
(main)





