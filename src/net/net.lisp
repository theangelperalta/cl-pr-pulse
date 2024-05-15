(in-package :net)

(defmacro with-json-decoding (&optional () &body body)
  `(let ((drakma:*text-content-types* (list* '("application" . "json") drakma:*text-content-types*)))
     (cl-json:decode-json-from-string
      (progn ,@body))))

(defun http-request-with-json-decoding (url)
  "Makes an http request and return content-type as json"
  (with-json-decoding ()
      (drakma:http-request url)))
