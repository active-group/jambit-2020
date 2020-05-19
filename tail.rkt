;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname tail) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

(check-expect (rev (list 1 2 3))
              (list 3 2 1))

(define rev-q ; quadratisch
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons-to-end 
        (rev-q (rest list)) ; 3 2
        (first list) ; 1
   )))))

(define rev
  (lambda (list)
    (rev* list empty)))

(: rev* ((list-of %a) (list-of %a) -> (list-of %a)))

(define rev* ; linear
  (lambda (list acc)
    (cond
      ((empty? list) acc)
      ((cons? list)
       ; kein Kontext: benötigt keinen Platz für rekursiven Aufruf
       ; tail call, endrekursiver Aufruf
       (rev* (rest list) (cons (first list) acc))))))

; JVM: Methodenaufruf benötigt *immer* Stackplatz
;      Stack hat (sehr) begrenzte Größe - typischerweise für ~10.000 Aufrufe

; 1. Aufruf von cons-to-end: leere Liste
; 2. Aufruf von cons-to-end: 1elementige Liste
; 3. Aufruf                : 2elementige Listen
; Funktionsaufrufe für n-elementige Liste:
; 1 + 2 + 3 + 4 + ... + n
; Gaußsche Summenformel: (n / 2) * (n + 1)
; = (n^2 + n) / 2 = O(n^2)

; Element hinten anhängen
(: cons-to-end ((list-of %a) %a -> (list-of %a)))

(check-expect (cons-to-end (list 1 2) 3)
              (list 1 2 3))

(define cons-to-end
  (lambda (list element)
    (cond
     ((empty? list) (cons element empty))
     ((cons? list)
      ; verbraucht Speicherplatz pro rekursivem Aufruf: z.B. Stack
      (cons (first list) ; Kontext: zu tun nach dem rekursiven Aufruf
            (cons-to-end (rest list) element))))))