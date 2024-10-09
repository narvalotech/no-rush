(ql:quickload "cl-json")
(ql:quickload "drakma")
(ql:quickload "local-time")

;; https://api.entur.io/graphql-explorer/journey-planner-v3?query=%0A%23+Welcome+to+GraphiQL%0A%23%23%23%23%23%23%23%23%23%23%23%23%23%23%23%23%23%23%0A%23+GraphiQL+is+an+in-browser+IDE+for+writing%2C+validating%2C+and%0A%23+testing+GraphQL+queries.%0A%23%0A%23+Type+queries+into+this+side+of+the+screen%2C+and+you+will%0A%23+see+intelligent+typeaheads+aware+of+the+current+GraphQL+type+schema+and%0A%23+live+syntax+and+validation+errors+highlighted+within+the+text.%0A%23%0A%23+To+bring+up+the+auto-complete+at+any+point%2C+just+press+Ctrl-Space.%0A%23%0A%23+Press+the+run+button+above%2C+or+Cmd-Enter+to+execute+the+query%2C+and+the+result%0A%23+will+appear+in+the+pane+to+the+right.%0A%23%0A%23%0A%23%23%23%23%23%23%23%23%23%23%23%23%23%23%23%23%23%23+Example+query%0Aquery+%7B%0A++stopPlace%28%0A++++id%3A+%22NSR%3AStopPlace%3A58404%22%0A++%29+%7B%0A++++name%0A++++id%0A++++estimatedCalls+%7B%0A++++++expectedDepartureTime%0A++++++actualDepartureTime%0A++++++destinationDisplay+%7B%0A++++++++frontText%0A++++++%7D%0A++++++serviceJourney+%7B%0A++++++++line+%7B%0A++++++++++publicCode%0A++++++++++transportMode%0A++++++++%7D%0A++++++%7D%0A++++%7D%0A++%7D%0A%7D%0A&variables=%7B%0A++%22id%22%3A+%22NSR%3AStopPlace%3A58404%22%0A%7D
;; https://developer.entur.org/pages-journeyplanner-journeyplanner
;; https://graphql.org/learn/queries/

;; query ($id: String!) {
;;   stopPlace(
;;     id: $id
;;   ) {
;;     name
;;     id
;;     estimatedCalls {
;;       expectedDepartureTime
;;       actualDepartureTime
;;       destinationDisplay {
;;         frontText
;;       }
;;       serviceJourney {
;;         line {
;;           publicCode
;;           transportMode
;;         } } } } }

;; TODO: make it so we don't have to type so many quotes
(defun gql-departures (nsr-id &key (max 5) (seconds (* 2 60 60)))
  (let ((quoted-id (format nil "\"~A\"" nsr-id)))
    `(("stopPlace" (("id" . ,quoted-id))
       (("name")
        ("id")
        ("estimatedCalls" (("numberOfDepartures" . ,max)
                           ("timeRange" . ,seconds))
                          (("expectedDepartureTime")
                           ;; ("actualDepartureTime") ; always empty it seems
                           ("destinationDisplay" ()
                                                 (("frontText")))
                           ("serviceJourney" ()
                                             (("line" ()
                                                      (("publicCode")
                                                       ("transportMode")))))))
        ;; Seems we have to fetch those per-quay, not per-station
        ;; ("situations" ()
        ;;               (("id")
        ;;                ("description" ()
        ;;                               (("value")
        ;;                                ("language")))))
        )))))

(defparameter test (gql-departures "NSR:StopPlace:59651"))

(defun args->str (arglist)
  (if arglist
      (format nil "(~{~A~^, ~})"
              (mapcar (lambda (arg)
                        (format nil "~A: ~A" (car arg) (cdr arg))) arglist))
      ""))

(defun sexp->gql (sx stream)
  (let* ((el (car sx))
         (name (nth 0 el))
         (params (nth 1 el))
         (children (nth 2 el)))

    (format stream " ~A" name)

    (when params
      (format stream (args->str params)))

    (when children
      (format stream " {")
      (sexp->gql children stream)
      (format stream " }"))

    (when (> (length sx) 1)
      (sexp->gql (cdr sx) stream))))

(defun make-gql (sx)
  "Make a GraphQL string from a query definition."
  (format nil "~A"
          (with-output-to-string (output)
            (format output "query {")
            (sexp->gql sx output)
            (format output " }"))))

(format t "~A" (make-gql test))
; query { stopPlace(id: "NSR:StopPlace:59651") { name id estimatedCalls { expectedDepartureTime actualDepartureTime destinationDisplay { frontText } serviceJourney { line { publicCode transportMode } } } } } => NIL

(defun package-gql (gql-str)
  "Package a GraphQL query string into a json object ready for sending."
  (json:encode-json-alist-to-string (list (cons "query" gql-str))))

(defun make-gql-json (params)
  (package-gql (make-gql params)))

(format t "~A" (make-gql test))
; query { stopPlace(id: "NSR:StopPlace:59651") { name id estimatedCalls { expectedDepartureTime actualDepartureTime destinationDisplay { frontText } serviceJourney { line { publicCode transportMode } } } } } => NIL
(format t "~A" (make-gql-json test))
; {"query":"query { stopPlace(id: \"NSR:StopPlace:59651\") { name id estimatedCalls { expectedDepartureTime actualDepartureTime destinationDisplay { frontText } serviceJourney { line { publicCode transportMode } } } } }"} => NIL

(defparameter test2
  '((:EXPECTED-DEPARTURE-TIME . "2024-10-06T23:33:00+02:00")
    (:ACTUAL-DEPARTURE-TIME)
    (:DESTINATION-DISPLAY (:FRONT-TEXT . "Ringen via Tøyen"))
    (:SERVICE-JOURNEY
     (:LINE (:PUBLIC-CODE . "5") (:TRANSPORT-MODE . "metro")))))

(assoc :service-journey test2)
 ; => (:SERVICE-JOURNEY (:LINE (:PUBLIC-CODE . "5") (:TRANSPORT-MODE . "metro")))

(defun send-query (query-json-str)
  (let* ((data query-json-str)
         (url "https://api.entur.io/journey-planner/v3/graphql"))
    (format t "Send query: ~A~%" data)

    (json:decode-json-from-string
     (octets-to-string
      (drakma:http-request url
                           :method :post
                           :content-type "application/json"
                           :accept "application/json"
                           :connection-timeout 2
                           :content data)))))

(send-query (make-gql-json (gql-departures "NSR:StopPlace:58404" :max 50)))
; Send query: {"query":"query { stopPlace(id: \"NSR:StopPlace:59651\") { name id estimatedCalls { expectedDepartureTime actualDepartureTime destinationDisplay { frontText } serviceJourney { line { publicCode transportMode } } } } }"}
;  => ((:DATA
;   (:STOP-PLACE (:NAME . "Skøyen stasjon") (:ID . "NSR:StopPlace:59651")
;    (:ESTIMATED-CALLS
;     ((:EXPECTED-DEPARTURE-TIME . "2024-10-06T23:01:04+02:00")
;      (:ACTUAL-DEPARTURE-TIME) (:DESTINATION-DISPLAY (:FRONT-TEXT . "Skøyen"))
;      (:SERVICE-JOURNEY
;       (:LINE (:PUBLIC-CODE . "130") (:TRANSPORT-MODE . "bus"))))
;     ((:EXPECTED-DEPARTURE-TIME . "2024-10-06T23:02:10+02:00")
;      (:ACTUAL-DEPARTURE-TIME)
;      (:DESTINATION-DISPLAY (:FRONT-TEXT . "Lillestrøm"))
;      (:SERVICE-JOURNEY
;       (:LINE (:PUBLIC-CODE . "L1") (:TRANSPORT-MODE . "rail"))))
;     ((:EXPECTED-DEPARTURE-TIME . "2024-10-06T23:03:00+02:00")
;      (:ACTUAL-DEPARTURE-TIME) (:DESTINATION-DISPLAY (:FRONT-TEXT . "Sandvika"))
;      (:SERVICE-JOURNEY
;       (:LINE (:PUBLIC-CODE . "130") (:TRANSPORT-MODE . "bus"))))
;     ((:EXPECTED-DEPARTURE-TIME . "2024-10-06T23:04:00+02:00")
;      (:ACTUAL-DEPARTURE-TIME)
;      (:DESTINATION-DISPLAY (:FRONT-TEXT . "Flytogbuss fra Strømsø torg"))
;      (:SERVICE-JOURNEY
;       (:LINE (:PUBLIC-CODE . "FLY1") (:TRANSPORT-MODE . "rail"))))
;     ((:EXPECTED-DEPARTURE-TIME . "2024-10-06T23:04:00+02:00")
;      (:ACTUAL-DEPARTURE-TIME) (:DESTINATION-DISPLAY (:FRONT-TEXT . "Dal"))
;      (:SERVICE-JOURNEY
;       (:LINE (:PUBLIC-CODE . "R13") (:TRANSPORT-MODE . "rail"))))))))

(defun extract-estimated-calls (response)
  (let ((calls (nth 3 (nth 1 (car response)))))
    (assert (eql :estimated-calls (car calls)))
    (cdr calls)))

(defun get-departures (nsr-id &key (max 100) (seconds (* 2 60 60)))
  "Convenience wrapper around GQL-DEPARTURES and SEND-QUERY"
  (extract-estimated-calls
   (send-query
    (make-gql-json
     (gql-departures nsr-id :max max :seconds seconds)))))

;; It's apparently possible to:
;; - whitelist transport modes
;; - whitelist line numbers
;; - specify a max departure time
;; from the GraphQL query.
;; I think it's better to fetch all and filter locally.

;; TODO: should we go full CLOS and parse immediately to an object?
;; That would mean we lose on future ulisp / picolisp compat
(defun extract-type (departure)
  (cdr
   (assoc :transport-mode
          (cdr (nth 1 (assoc :service-journey departure))))))

(defun is-transport-mode (modes element)
  "Tests if the element's :TRANSPORT-MODE matches one of the MODES"
  (let ((result nil)
        (el-mode (extract-type element)))
    (dolist (test-mode modes result)
      (setf result (or result (equalp test-mode el-mode))))))

(is-transport-mode '("metro") test2)
 ; => T
(is-transport-mode '("ferry") test2)
 ; => NIL
(is-transport-mode '("rail" "metro") test2)
 ; => T

(defun filter-by-type (departures transport-types)
  ;; don't filter if filter list is empty :)
  (if (not transport-types) departures
      (remove-if-not
       (lambda (el) (is-transport-mode transport-types el))
       departures)))

(defun extract-destination (departure)
  (cdr
   (assoc :front-text
          (cdr (assoc :destination-display departure)))))

(defun is-destination (destinations element)
  "Test if the element's destination text matches one of DESTINATIONS."
  (let ((result nil)
        (el-dest (extract-destination element)))

    (dolist (test-dest destinations result)
      (setf result (or result (equalp test-dest el-dest))))))

(defparameter *test-vestli* '((:EXPECTED-DEPARTURE-TIME . "2024-10-07T08:29:00+02:00")
                              (:ACTUAL-DEPARTURE-TIME) (:DESTINATION-DISPLAY (:FRONT-TEXT . "Vestli"))
                              (:SERVICE-JOURNEY (:LINE (:PUBLIC-CODE . "5") (:TRANSPORT-MODE . "metro")))))

(is-destination '("Vestli") *test-vestli*)
 ; => T

(defun filter-by-destination (departures destinations)
  (if (not destinations) departures
      (remove-if-not
       (lambda (el) (is-destination destinations el))
       departures)))

(defun extract-line (departure)
  (cdr
   (assoc :public-code
          (cdr (nth 1 (assoc :service-journey departure))))))

(defun is-line (lines element)
  "Tests if the element's :PUBLIC-CODE matches one of the LINES"
  (let ((result nil)
        (el-line (extract-line element)))
    (dolist (test-line lines result)
      (setf result (or result (equalp test-line el-line))))))

(is-line '("4") *test-vestli*)
 ; => NIL
(is-line '("5") *test-vestli*)
 ; => T
(is-line '("1" "34" "5") *test-vestli*)
 ; => T

(defun filter-by-line (departures lines)
  (if (not lines) departures
      (remove-if-not
       (lambda (el) (is-line lines el))
       departures)))

(defun extract-timestamp (departure)
  (cdr (assoc :expected-departure-time departure)))

(extract-timestamp *test-vestli*)
 ; => "2024-10-07T08:29:00+02:00"

(defun timestamp->human-readable (timestamp-str)
  "Converts an RFC3339 timestamp to a human-friendly format."
  (let ((time-format '((:hour 2) ":" (:min 2)))
        (time
          (local-time:parse-rfc3339-timestring
           timestamp-str)))
    (local-time:format-timestring nil time :format time-format)))

(timestamp->human-readable (extract-timestamp *test-vestli*))
 ; => "08:29"

(defun format-departure (departure)
  (format nil "~A: [~A] line ~A to ~A"
          (timestamp->human-readable
           (extract-timestamp departure))
          (extract-type departure)
          (extract-line departure)
          (extract-destination departure)))

(format-departure *test-vestli*)
 ; => "08:29: [metro] line 5 to Vestli"

;; blommenholm
(filter-by-type '("rail")
                (get-departures "NSR:StopPlace:58843"))

;; nationaltheatret
(filter-by-type '("metro")
                (get-departures "NSR:StopPlace:58404"))

