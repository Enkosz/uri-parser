(load "./uri-parse.lisp")

(defparameter passed 0)
(defparameter failed 0)
(defparameter tot 0)

(defun test (name value expected) 
  (if (equal value expected)
      (setq passed (1+  passed))
      (and (setq failed (1+ failed))
	   (format T "~%Test ~A failed --> Expected: ~A Found: ~A" name expected value)))
  (setq tot (1+ tot))
)

(test "test-scheme-1"
    (uri-scheme (uri-parse "http://google.com"))
    "http"
)
(test "test-scheme-2"
    (uri-scheme (uri-parse "h11ps://google.com"))
    "h11ps"
)
(test "test-scheme-3"
    (uri-scheme (uri-parse "_http_://google.com"))
    "_http_"
)

(format t "~%Run ~D tests: passed ~D, failed ~D" tot passed failed)