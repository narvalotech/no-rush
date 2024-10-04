(ql:quickload "cl-json")
(ql:quickload "drakma")

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
;;         }
;;       }
;;     }
;;   }
;; }

(defparameter test '(:object
                     (:name "stopPlace" :arg (("id" . "\\\"NSR:StopPlace:59651\\\"")
                                              ("other" . "\"someotherstring\"")))
                     ;; list of "field" objects
                     :fields ((:name "name" :arg (("p2" . "v2")))
                              (:name "other" :arg ()))))

(defun args->str (arglist)
  (if arglist
      (format nil "(~{~A~^, ~})"
              (mapcar (lambda (arg)
                        (format nil "~A: ~A" (car arg) (cdr arg))) arglist))
      ""))

(args->str (getf (getf test :object) :arg))
(args->str nil)

(defun fields->str (fields)
  (mapcar (lambda (field) (format nil "~A~A" (getf field :name) (args->str (getf field :arg))))
          fields))

(fields->str (getf test :fields))

(defun make-gql (params)
  "Make a GraphQL string from a query definition."
  ;; Definiting schemas: ain't nobody got time fo dat
  ;; assume only one object for now
  (let ((object (getf (getf params :object) :name))
        (args (getf (getf params :object) :arg))
        (fields (getf params :fields)))
    (format nil "query { ~A~A { ~{~A~^ ~} } }"
            object (args->str args)
            (fields->str fields))))

(make-gql test)

(defun package-gql (gql-str)
  "Package a GraphQL query string into a json object ready for sending."
  (json:encode-json-alist-to-string (list (cons "query" gql-str))))

(defun make-gql-json (params)
  (package-gql (make-gql params)))

(make-gql test)

(defparameter skoyen
  '(:object (:name "stopPlace" :arg (("id" . "\"NSR:StopPlace:59651\"")))
    :fields ((:name "name" :arg nil))))

(format t "~A" (make-gql skoyen))
; query { stopPlace(id: "NSR:StopPlace:59651") { name } } => NIL
(format t "~A" (make-gql-json skoyen))
; {"query":"query { stopPlace(id: \"NSR:StopPlace:59651\") { name } }"} => NIL

; NSR:StopPlace:59651

(defun make-query ()
  "{\"query\":\"query { stopPlace(id: \\\"NSR:StopPlace:59651\\\") { name } }\"}")

(format t "~A" (make-query))

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

(send-query (make-query))
; Send query: {"query":"query { stopPlace(id: \"NSR:StopPlace:59651\") { name } }"}
;  => ((:DATA (:STOP-PLACE (:NAME . "Skøyen stasjon"))))

(send-query (make-gql-json skoyen))
; Send query: {"query":"query { stopPlace(id: \"NSR:StopPlace:59651\") { name } }"}
;  => ((:DATA (:STOP-PLACE (:NAME . "Skøyen stasjon"))))
