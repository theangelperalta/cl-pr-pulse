(in-package :cl-pr-pulse)


(defun main ()
  (decode:decode-stats (net:http-request-with-json-decoding "https://raw.githubusercontent.com/sky-uk/core-video-sdk-ios/develop/CVReferenceApp/CVMockedAssets/Sources/Fixtures/VAM/vodFreewheelParams.json?token=GHSAT0AAAAAACG4W33LGJFZW5CPXQSKHLOAZSFF5WQ")))
