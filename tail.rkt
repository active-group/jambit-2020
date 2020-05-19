;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname tail) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

(check-expect (rev (list 1 2 3))
              (list 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons-to-end 
        (rev (rest list)) ; 3 2
        (first list) ; 1
   )))))

; Element hinten anhängen
(: cons-to-end ((list-of %a) %a -> (list-of %a)))

(check-expect (cons-to-end (list 1 2) 3)
              (list 1 2 3))

(define cons-to-end
  (lambda (list element)
    (cond
     ((empty? list) (cons element empty))
     ((cons? list)
      (cons (first list)
            (cons-to-end (rest list) element))))))