(in-package :model)

(defstruct user
  id
  url
  login
  avatar-url)

(defstruct review
  id
  author
  is-own-pull
  submitted-at
  comment-count ;; get(data, 'comments.totalCount')
  time-to-review-secs) ;; submittedAt - startDate

(defstruct pull-request
  id
  title
  published-at
  author
  reviews)
