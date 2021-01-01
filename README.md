# cl-stocks

This is a basic stock price reader written in Common Lisp, as
a demonstration of some of the great libraries available
in CL. It web scrapes Yahoo Finance and cache's the result for
reference later.

This was tested to work with SBCL. Did not work very well with ECL.

# Configuration

We expect a file (`~/.stonksrc`) with a list of Yahoo finance URL's
such as:
```
https://finance.yahoo.com/quote/FB
https://finance.yahoo.com/quote/AAPL
https://finance.yahoo.com/quote/AMZN
https://finance.yahoo.com/quote/NFLX
https://finance.yahoo.com/quote/GOOG
https://finance.yahoo.com/quote/^GSPC
```

Make sure there's no extra lines after the last url entry. 

# Running

Run cl-stocks.lisp with your favorite implementation and 
it should work out of the box unless Yahoo changes their
HTML table. 

Example invocation:
```
sbcl --load cl-stocks.lisp --eval "(quit)"
```

Example output:
```
Cache Date: 09:49:49 on Friday, 1/01/2021 (GMT+0) 
Facebook, Inc. (FB) Price: 273.16 Last (Cache) Price: 273.16 
Apple Inc. (AAPL) Price: 132.69 Last (Cache) Price: 132.69 
Amazon.com, Inc. (AMZN) Price: 3,256.93 Last (Cache) Price: 3,256.93 
Netflix, Inc. (NFLX) Price: 540.73 Last (Cache) Price: 540.73 
Alphabet Inc. (GOOG) Price: 1,751.88 Last (Cache) Price: 1,751.88 
S&P 500 (^GSPC) Price: 3,756.07 Last (Cache) Price: 3,756.07 
```

Using SBCL, compilation to an executable can be done as so (didn't work with ECL). Expected output executable size is roughly 15MB:
```
sbcl --load cl-stocks.lisp --eval "(sb-ext:save-lisp-and-die \"stonks\" :toplevel 'main :executable t :compression 9)"
```
