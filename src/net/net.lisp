(in-package :net)

(defmacro with-json-decoding (&optional () &body body)
  `(let ((drakma:*text-content-types* (list* '("application" . "json") drakma:*text-content-types*)))
     (cl-json:decode-json-from-string
      (progn ,@body))))

(defun http-request-with-json-decoding (url)
  "Makes an http request and return content-type as json"
  (with-json-decoding ()
    #+(or)
    (drakma:http-request url)
    (example-json)))

(defun example-json ()
  (uiop:read-file-string (concatenate 'string (or (uiop:getenv "CL_PROJECTS_PATH") "~/Developer/cl") "/cl-pr-pulse/example-pr.json")))
