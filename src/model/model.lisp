(in-package :model)

(defstruct user
  id
  name
  url
  login
  avatar-url)

(defstruct review
  id
  author
  state
  is-own-pull
  pull-request-author
  submitted-at
  comment-count ;; get(data, 'comments.totalCount')
  time-to-review-secs) ;; submittedAt - startDate

(defstruct pull-request
  id
  title
  url
  published-at
  author
  reviews)
