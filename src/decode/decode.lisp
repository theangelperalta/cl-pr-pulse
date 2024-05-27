(in-package :decode)

(defun commit-date-before-pr-review (submitted-at commits)
  ;; 1. Get all commits before PR review submit date
  ;; 2. Get closest timestamp
  (let ((all-commits-before-submitted-at (remove-if-not (lambda (commit) (local-time:timestamp< (local-time:parse-timestring (assoc-path commit '(:committed-date))) submitted-at)) commits)))
    (reduce #'local-time:timestamp-maximum (mapcar (lambda (current-commit) (local-time:parse-timestring (assoc-path current-commit '(:committed-date)))) all-commits-before-submitted-at))))

(defun decode-commits-pull-request (pull-request)
  (mapcar (lambda (current-commit) (assoc-path current-commit '(:commit))) (assoc-path pull-request '(:commits :nodes))))

(defun decode-user (user)
  (let* ((id (assoc-path user '(:database-id)))
         (name (assoc-path user '(:name)))
         (url (assoc-path user '(:url)))
         (login (assoc-path user '(:login)))
         (avatar-url (assoc-path user '(:avatar-url))))
    (model:make-user :id id :name name :url url :login login :avatar-url avatar-url)))

(defun decode-review (pull-request data)
  (let* ((id (assoc-path data '(:id)))
         (author (decode-user (assoc-path data '(:author))))
         (is-own-pull (equalp (model::user-login author) (assoc-path pull-request '(:author-login))))
         (submitted-at (local-time:parse-timestring (assoc-path data '(:submitted-at))))
         ;; (commit-date (local-time:parse-timestring (assoc-path data '(:commit :committed-date))))
         (commit-date (commit-date-before-pr-review submitted-at (decode-commits-pull-request pull-request)))
         (start-date (local-time:timestamp-maximum commit-date (local-time:parse-timestring (assoc-path pull-request '(:published-at)))))
         (comment-count (assoc-path data '(:comments :total-count)))
         (time-to-review-secs (local-time:timestamp-to-unix (diff-timestamp submitted-at start-date))))
    (model:make-review :id id :author author :is-own-pull is-own-pull :submitted-at submitted-at :comment-count comment-count :time-to-review-secs time-to-review-secs)))

(defun decode-pull-requests (data)
  (mapcar (lambda (pull-request) (model:make-pull-request :id (assoc-path pull-request '(:id)) :title (assoc-path pull-request '(:title)) :url (assoc-path pull-request '(:url)) :published-at (local-time:parse-timestring (assoc-path pull-request '(:published-at))) :author (decode-user (assoc-path pull-request '(:author))) :reviews (mapcar #'(lambda (data) (decode-review pull-request data)) (assoc-path pull-request '(:reviews :nodes))))) (assoc-path data '(:data :repository :pull-requests :nodes))))

(defun decode-team-members (data)
  (mapcar (lambda (user) (decode-user user)) (assoc-path data '(:data :organization :team :members :nodes))))

(defun decode-stats (data)
  (decode-pull-requests data))

(defun assoc-path (alist path &key (key #'identity) (test #'eql) (default nil))
  "Retrieve the value in the given ALIST represented by the given PATH"
  (or (reduce (lambda (alist k)
                (cdr (assoc k alist :key key :test test)))
              path
              :initial-value alist)
      default))

(defun diff-timestamp (lhs rhs)
  (local-time:unix-to-timestamp (- (local-time:timestamp-to-unix lhs) (local-time:timestamp-to-unix rhs))))
