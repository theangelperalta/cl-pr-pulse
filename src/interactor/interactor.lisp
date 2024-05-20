(in-package :interactor)

(defstruct author-review-stats
  author
  reviews)

(defun collect-stats (url)
  (let* ((github-stats (parse-pull-request-reviews (flatten (collect-pull-request-reviews (decode:decode-stats (net:http-request-with-json-decoding url))))))
         (list-of-github-stats (sort (hash-table-values github-stats) (lambda (lhs rhs) (> (list-length (author-review-stats-reviews lhs)) (list-length (author-review-stats-reviews rhs)))))))
    (loop for value in list-of-github-stats
          do (let ((review-count (list-length (author-review-stats-reviews value))))
               (format t "~S: #Reviews: ~D, Median Time of Review: ~A~%" (or (model::user-name (author-review-stats-author value)) (model::user-login (author-review-stats-author value))) review-count (median-time-to-review review-count value))))))

(defun hash-table-values (hash-table)
  (loop for key being the hash-keys of hash-table
          using (hash-value value)
        collect value))

(defun average-time-to-review (review-count review)
  (let* ((time-to-review (/ (if (> review-count 1) (reduce (lambda (lhs rhs) (+ (time-to-review-or-num lhs) (time-to-review-or-num rhs))) (author-review-stats-reviews review)) (model::review-time-to-review-secs (first (author-review-stats-reviews review)))) review-count)))
    (format-review-time time-to-review)))

(defun median-time-to-review (review-count review)
  (let* ((time-to-review (/ (if (> review-count 1) (model::review-time-to-review-secs (nth (- (floor review-count 2) 1) (author-review-stats-reviews review))) (model::review-time-to-review-secs (first (author-review-stats-reviews review)))) review-count)))
    (format-review-time time-to-review)))

(defun format-review-time (time)
  (format nil "~,2F ~A" (format-time time) (format-abrv time)))

(defun format-time (time)
  (cond
    ((> time 3600) (/ time 3600.0))
    ((> time 60) (/ time 60.0))
    (t time)))

(defun format-abrv (time)
  (cond
    ((> time 3600) "hr")
    ((> time 60)  "mins")
    (t "secs")))

(defun time-to-review-or-num (value)
  (if (model::review-p value) (model::review-time-to-review-secs value) value))

(defun collect-pull-request-reviews (pull-requests)
  (loop for pull-request in pull-requests
        collect (model::pull-request-reviews pull-request)))

(defun parse-pull-request-reviews (reviews)
  (let ((github-stats (make-hash-table)))
    (progn
      (loop for review in (remove nil reviews)
            do (let* ((review-stats (or (gethash (model::user-id (model::review-author review)) github-stats) (make-author-review-stats :author (model::review-author review) :reviews nil)))
                      (updated-review-stats (progn (setf (author-review-stats-reviews review-stats) (append (author-review-stats-reviews review-stats) (list review))) review-stats)))
                 (setf (gethash (model::user-id (model::review-author review)) github-stats) updated-review-stats)))
      github-stats)))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))
