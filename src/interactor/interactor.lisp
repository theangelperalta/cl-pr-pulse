(in-package :interactor)

(defstruct author-review-stats
  author
  (num-us-reviews 0)
  (num-eu-reviews 0)
  reviews)

(defun diff-team-members (team-1 team-2)
  (set-difference team-1 team-2 :test #'(lambda (m1 m2) (equalp (model::user-id m1) (model::user-id m2)))))

;; Team 1 - US
;; Team 2 - EU
;;
(defconstant +CELL-SMALL-WIDTH+ 20)

(defun collect-stats (url team-1 team-2 non-team)
  (let* ((non-team (decode:decode-team-members (net:http-request-with-json-decoding non-team)))
         (team-2 (diff-team-members (decode:decode-team-members (net:http-request-with-json-decoding team-2)) non-team))
         (team-1 (diff-team-members (diff-team-members (decode:decode-team-members (net:http-request-with-json-decoding team-1)) non-team) team-2))
         (github-stats (parse-pull-request-reviews (decode:decode-stats (net:http-request-with-json-decoding url)) team-1 team-2))
         (list-of-github-stats (sort (hash-table-values github-stats) (lambda (lhs rhs) (> (list-length (author-review-stats-reviews lhs)) (list-length (author-review-stats-reviews rhs)))))))
    (progn
    (format t "|~v:@<~A~>|~v:@<~A~>|~v:@<~A~>|~v:@<~A~>|~v:@<~A~>|~v:@<~A~>|~v:@<~A~>|~v:@<~A~>|~v:@<~A~>|~v:@<~A~>|~%" 25 "Team Memeber" +CELL-SMALL-WIDTH+ "# Reviews" +CELL-SMALL-WIDTH+ "Median TTR" +CELL-SMALL-WIDTH+ "Average TTR" +CELL-SMALL-WIDTH+ "Median TTR U.S" +CELL-SMALL-WIDTH+ "Average TTR U.S" +CELL-SMALL-WIDTH+ "Median TTR E.U." +CELL-SMALL-WIDTH+ "Average TTR E.U." +CELL-SMALL-WIDTH+ "% U.S. Reviews" +CELL-SMALL-WIDTH+ "% E.U. Reviews")
    (loop for value in list-of-github-stats
          do (let* ((review-count (list-length (author-review-stats-reviews value)))
                    (reviews-us (remove-if-not (lambda (review) (remove nil (mapcar (lambda (team-member) (equalp (model::user-id (model::review-pull-request-author review)) (model::user-id team-member))) team-1))) (author-review-stats-reviews value)))
                    (reviews-eu (remove-if-not (lambda (review) (remove nil (mapcar (lambda (team-member) (equalp (model::user-id (model::review-pull-request-author review)) (model::user-id team-member))) team-2))) (author-review-stats-reviews value))))
               (format t "|~vA|~v:@<~A~>|~v:@<~A~>|~v:@<~A~>|~v:@<~A~>|~v:@<~A~>|~v:@<~A~>|~v:@<~A~>|~v:@<~,2F~>|~v:@<~,2F~>|~%"  25 (or (model::user-name (author-review-stats-author value)) (model::user-login (author-review-stats-author value))) +CELL-SMALL-WIDTH+ review-count +CELL-SMALL-WIDTH+ (median-time-to-review review-count (author-review-stats-reviews value)) +CELL-SMALL-WIDTH+ (average-time-to-review review-count (author-review-stats-reviews value)) +CELL-SMALL-WIDTH+ (if (> (length reviews-us) 0) (median-time-to-review (length reviews-us) reviews-us) "n/a") +CELL-SMALL-WIDTH+ (if (> (length reviews-us) 0) (average-time-to-review (length reviews-us) reviews-us) "n/a") +CELL-SMALL-WIDTH+ (if (> (length reviews-eu) 0) (median-time-to-review (length reviews-eu) reviews-eu) "n/a") +CELL-SMALL-WIDTH+ (if (> (length reviews-eu) 0) (average-time-to-review (length reviews-eu) reviews-eu) "n/a") +CELL-SMALL-WIDTH+ (* (/ (author-review-stats-num-us-reviews value) review-count) 100.0) +CELL-SMALL-WIDTH+ (* (/ (author-review-stats-num-eu-reviews value) review-count) 100.0)))))))

(defun hash-table-values (hash-table)
  (loop for key being the hash-keys of hash-table
          using (hash-value value)
        collect value))

(defun average-time-to-review (review-count reviews)
  (let* ((time-to-review (/ (if (> review-count 1) (reduce (lambda (lhs rhs) (+ (time-to-review-or-num lhs) (time-to-review-or-num rhs))) reviews) (model::review-time-to-review-secs (first reviews))) review-count)))
    (format-review-time time-to-review)))

(defun median-time-to-review (review-count reviews)
  (let* ((time-to-review (/ (if (> review-count 1) (model::review-time-to-review-secs (nth (- (floor review-count 2) 1) reviews)) (model::review-time-to-review-secs (first reviews))) review-count)))
    (format-review-time time-to-review)))

(defun format-review-time (time)
  (format nil "~,2F ~A" (format-time time) (format-abrv time)))

(defun format-time (time)
  (cond
    ((> time 86400) (/ time 86400.0))
    ((> time 3600) (/ time 3600.0))
    ((> time 60) (/ time 60.0))
    (t time)))

(defun format-abrv (time)
  (cond
    ((> time 86400) "days")
    ((> time 3600) "hrs")
    ((> time 60)  "mins")
    (t "secs")))

(defun time-to-review-or-num (value)
  (if (model::review-p value) (model::review-time-to-review-secs value) value))

(defun collect-pull-request-reviews (pull-requests)
  (loop for pull-request in pull-requests
        collect (model::pull-request-reviews pull-request)))

(defun parse-pull-request-reviews (pull-requests team-1 team-2)
  (let ((github-stats (make-hash-table)))
      (loop for pull-request in pull-requests
            do (let ((reviews (remove nil (model::pull-request-reviews pull-request))))
                 (loop for review in reviews
            do (let* ((review-stats (or (gethash (model::user-id (model::review-author review)) github-stats) (make-author-review-stats :author (model::review-author review) :reviews nil)))
                      (updated-review-stats (progn (setf (author-review-stats-reviews review-stats) (append (author-review-stats-reviews review-stats) (list review)) (author-review-stats-num-us-reviews review-stats) (accumulate-review-count-for-team (model::pull-request-author pull-request) (author-review-stats-num-us-reviews review-stats) review team-1) (author-review-stats-num-eu-reviews review-stats) (accumulate-review-count-for-team (model::review-pull-request-author review) (author-review-stats-num-eu-reviews review-stats) review team-2)) review-stats)))
                 (setf (gethash (model::user-id (model::review-author review)) github-stats) updated-review-stats)))))
      github-stats))

(defun accumulate-review-count-for-team (pr-author current-count review team-members)
  (let* ((author-id (model::user-id pr-author))
        (contains-author (remove-if-not (lambda (team-member) (equalp author-id (model::user-id team-member))) team-members)))
    (if contains-author (1+ current-count) current-count)))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))
