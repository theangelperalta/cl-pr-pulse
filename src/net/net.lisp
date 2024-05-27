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
    (example-json url)))

(defun example-json (url)
  (cond
    ((equalp url "TEAM_1_URL") (uiop:read-file-string (concatenate 'string (or (uiop:getenv "CL_PROJECTS_PATH") "~/Developer/cl") "/cl-pr-pulse/user.json")))
    ((equalp url "TEAM_2_URL") (uiop:read-file-string (concatenate 'string (or (uiop:getenv "CL_PROJECTS_PATH") "~/Developer/cl") "/cl-pr-pulse/user-eu.json")))
    ((equalp url "NON_TEAM_URL") (uiop:read-file-string (concatenate 'string (or (uiop:getenv "CL_PROJECTS_PATH") "~/Developer/cl") "/cl-pr-pulse/non-user.json")))
    (t (uiop:read-file-string (concatenate 'string (or (uiop:getenv "CL_PROJECTS_PATH") "~/Developer/cl") "/cl-pr-pulse/example-pr.json")))))
