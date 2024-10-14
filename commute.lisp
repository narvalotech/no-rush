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

(defun get-departures (nsr-id &key (max 200) (seconds (* 2 60 60)))
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

(defun filter-by-type (transport-types departures)
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

(defun filter-by-destination (destinations departures)
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

(defun filter-by-line (lines departures)
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
  (format nil "~A [~A] line ~A to ~A"
          (timestamp->human-readable
           (extract-timestamp departure))
          (extract-type departure)
          (extract-line departure)
          (extract-destination departure)))

(format-departure *test-vestli*)
 ; => "08:29 [metro] line 5 to Vestli"

;; blommenholm
(filter-by-type '("rail") (get-departures "NSR:StopPlace:58843"))

;; nationaltheatret
(defparameter *test-national* (get-departures "NSR:StopPlace:58404"))

(filter-by-type '("metro") *test-national*)
(filter-by-line '("5") *test-national*)

;; TODO: refactor the FILTER-xx fns. They are very similar.
(defun filter-departures (departures
                          &key types destinations lines)
  (filter-by-line
   lines (filter-by-destination
          destinations (filter-by-type types departures))))

(defun print-departures (stream departures)
  (with-output-to-string (s)
    (mapcar (lambda (d)
              (format (if stream stream s) "~A~%" (format-departure d)))
            departures)))

(print-departures t
 (filter-departures
  *test-national*
  :types '("rail" "metro")
  :destinations '("vestli" "asker" "spikkestad")
  :lines '("5" "L1")))
; 22:29 [rail] line L1 to Asker
; 22:29 [metro] line 5 to Vestli
; 22:43 [rail] line L1 to Spikkestad
; 22:45 [metro] line 5 to Vestli
; 22:59 [rail] line L1 to Asker
;  => (NIL NIL NIL NIL NIL)

;; Geocoder API example
;; https://api.entur.io/geocoder/v1/autocomplete?text=skoyen%20stasjon&lang=en

(defun send-geocoder-query (name)
  "Send a Entur geocoder API to retrieve stops matching NAME"
  (let* ((url "https://api.entur.io/geocoder/v1/autocomplete"))
    (format t "Send geocoder query: ~A~%" name)

    (json:decode-json-from-string
     (octets-to-string
      (drakma:http-request url
                           :method :get
                           :parameters (list (cons "text" name)
                                             (cons "lang" "en"))
                           :connection-timeout 2)))))

(defparameter *test-g-oslo-s* (send-geocoder-query "oslo s"))
(defparameter *test-g-skoyen* (send-geocoder-query "skoyen stasjon"))
(defparameter *test-g-national* (send-geocoder-query "nationaltheatret stasjon"))

(defun g-get-props (query-response)
  "Extracts stop properties from a geocoder API response. Always picks the first entry."
  (cdr (assoc :properties (nth 1 (assoc :features query-response)))))

(defun g-props->plist (prop-alist)
  "Extracts geocoder properties into a nice alist."
  (flet ((get-val (key) (cdr (assoc key prop-alist))))
    (let ((id (get-val :id))
          (name (get-val :name))
          (category (get-val :category)))
      (list
       :id id
       :name name
       :types category))))

(g-props->plist (g-get-props *test-g-skoyen*))
 ; => (:ID "NSR:StopPlace:59651" :NAME "Skøyen stasjon" :TYPES
 ; ("railStation" "onstreetBus" "onstreetBus" "onstreetBus"))

;; This would be much nicer with a "stopPlace" object
(defun find-stop (text)
  "Query API and return the first result's stop-place ID"
  (getf (g-props->plist (g-get-props (send-geocoder-query text))) :id))

(find-stop "skoyen")
; Send geocoder query: skoyen
;  => "NSR:StopPlace:58223"

(time (find-stop "skoyen stasjon"))
; Send geocoder query: skoyen stasjon
;  => "NSR:StopPlace:59651"

;; .6s 237kB alloc
(time (find-stop "nationaltheatret"))
; Send geocoder query: nationaltheatret
;  => "NSR:StopPlace:58404"

;; .6s 1.5MB alloc
(time (get-departures "NSR:StopPlace:58404"))

;; ~1s for two API calls. Not that bad.
;; There's almost 2MB of allocations when API calls are fired. That's concerning.
(time
 (print-departures t
  (filter-departures
   (get-departures (find-stop "nationaltheatret"))
   :types '("rail" "metro")
   :destinations '("vestli" "asker" "spikkestad")
   :lines '("5" "L1"))))
; Send geocoder query: nationaltheatret
; Send query: {"query":"query { stopPlace(id: \"NSR:StopPlace:58404\") { name id estimatedCalls(numberOfDepartures: 100, timeRange: 7200) { expectedDepartureTime destinationDisplay { frontText } serviceJourney { line { publicCode transportMode } } } } }"}
; 09:29 [rail] L1 Asker
; 09:29 [metro] 5 Vestli
; 09:43 [rail] L1 Spikkestad
;  => (NIL NIL NIL)

(ql:quickload :clack)
(ql:quickload :cl-utilities)
(ql:quickload :quri)

(defun parse-keyword (string)
  (intern (string-upcase string) :keyword))

(defun decode-url-params (url)
  "Decode URL-encoded (and escaped) parameters to a param-list"
  (reduce #'append
          (mapcar (lambda (p)
                    (list (parse-keyword (car p))
                          (cl-utilities:split-sequence
                           #\Comma
                           (remove-if (lambda (c) (char-equal #\" c)) (cdr p)))))
                  (quri:url-decode-params url))))

(decode-url-params "station-name=nationaltheatret&types=%22rail,metro%22&destinations=%22oslo%20s,vestli,sk%C3%B8yen%22")
 ; => (:STATION-NAME ("nationaltheatret") :TYPES ("rail" "metro") :DESTINATIONS
 ; ("oslo s" "vestli" "skøyen"))

;; TODO: fix encoding to UTF-8
(defun get-station (&key station-name types destinations lines)
  (format t "Fetching: ~A types [~A] dests [~A] lines [~A]~%" station-name types destinations lines)
  (print-departures nil
   (filter-departures
    (get-departures (find-stop (car station-name)))
    :types types
    :destinations destinations
    :lines lines)))

(defun handle-departures (env)
  (list
   (apply #'get-station (decode-url-params (getf env :query-string)))))

(defun response (env)
  (format t "query-string: ~A~%" (getf env :query-string))
  ;; (break)

  (if (and (eql :get (getf env :request-method))
           (equalp "/departures" (getf env :path-info)))
      (list 200 '(:content-type "text/plain") (handle-departures env))
      (list 400 '() '("doesn't seem like anything to me.. (err 400)"))))

(defvar *handler* (clack:clackup 'response :address "0.0.0.0" :port 9003))
(clack:stop *handler*)
