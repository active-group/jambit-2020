;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname mike1) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Schulung funktionale Programmierung

(define mike "sperber")

(define x
  (+ 12
     (* 5 13)))


(define circle1 (circle 50 "solid" "red"))
(define star1 (star 50 "outline" "green"))

(define ov1 (overlay star1 circle1))

(define ov2 (overlay (square 70 "outline" "blue") ov1))

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside ov1 (square 100 "solid" "brown"))
 (beside (square 100 "solid" "brown") ov1))

; Kurzbeschreibung
; Zwei Bildern in quadratischem Kachelmuster anordnen

; Signatur
(: tile (image image -> image))
;^ Signaturdeklaration

(check-expect (tile circle1 star1)
              (above
               (beside circle1 star1)
               (beside star1 circle1)))

; Gerüst, Schablone, Rumpf
(define tile
  (lambda (image1 image2) ; <- Parameter
    (above
     (beside image1 image2)
     (beside image2 image1))))

; Signaturverletzung
; (tile 5 17)

; Variablen werden bei der Auswertung durch ihre Werte ersetzt.
; Insbesondere: Parameter werden durch ihre Argumente ersetzt
;(tile circle1 star1)

#|

 o.m(15);

class O {
  T m(int x) {
    x
    x = 10; // Zuweisung
    x
  }
}

x steht *nicht* für den Wert 15

x steht für eine Speicherzelle, in der steht 15.
Inhalt kann durch Zuweisung ausgetauscht werden.

|#


; Ein Haustier ist Hund oder eine Katze oder eine Schlange

; Ein Haustier ist eins der folgenden:  <--- Fallunterscheidung
; - Hund
; - Katze
; - Schlange
; ohne weitere Attribute: Aufzählung, Spezialfall Fallunterscheidung
(define pet
  (signature ; immer, wenn eine Signatur definiert wird
   (enum "Hund" "Katze" "Schlange")))

; Beispiele
(: pet1 pet)
(define pet1 "Hund")
(: pet2 pet)
(define pet2 "Katze")

; Ist Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "Hund") #t)
(check-expect (cute? "Katze") #t)
(check-expect (cute? "Schlange") #f)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone <- aus den Signaturen
(define cute?
  (lambda (pet)
    (cond ; Verzweigung, soviele Zweige wie Fälle
      ; jeder Zweig: Bedingung, Ergebnis
      ((string=? pet "Hund") #t)
      ((string=? pet "Katze") #t)
      ((string=? pet "Schlange") #f))))

; Ein Bildformat ist eins der folgenden:
; - hochkant
; - quer
; - quadratisch
; <Stück Code>

; Für ein Bild das Bildformat ermitteln
; dafür:
; (: image-width (image -> natural))
; (: image-height (image -> natural))


; Doku: 2htdp/image

(define hour
  (signature
   (combined natural
             (predicate
              (lambda (n)
                (and (natural? n)
                     (>= n 0)
                     (<= n 23)))))))

; Eine Uhrzeit besteht aus/hat folgende Eigenschaften:
; - Stunden
; - Minuten
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour   hour) ; Selektoren, getter-Funktionen
  (time-minute natural))

(: make-time (hour natural -> time))

(define t1 (make-time 11 17)) ; 11 Uhr 17
(define t2 (make-time 0 0)) ; Mitternacht
(define t3 (make-time 17 27)) ; 17 Uhr 27

(: time-hour (time -> hour))
(: time-minute (time -> natural))

; (make-time 24 15)


; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm t1) (+ (* 60 11) 17))
(check-expect (msm t2) 0)

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Minuten seit Mitternacht wieder zurückkonvertieren
(: msm->time (natural -> time))

(check-expect (msm->time (+ (* 60 11) 17)) t1)
(check-expect (msm->time 0) t2)

(define msm->time
  (lambda (m)
    (define q (quotient m 60))
    (make-time q
               (- m (* 60 q)))))
               
; Ein Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot
; - Gewicht
(define-record dillo
  make-dillo
  dillo?
  (dillo-alive? boolean)
  (dillo-weight rational))

(: dillo? (any -> boolean)) ; Prädikat

; "Zustand eines Gürteltiers zu einem bestimmten Zeitpunkt"
(define dillo1 (make-dillo #t 10)) ; Gürteltier, lebendig, 10kg
(define dillo2 (make-dillo #f 12)) ; Gürteltier, tot, 12kg

; Gürteltier überfahren

#|
class Dillo {
   bool isAlive;

   void runOver() {
     this.isAlive = false;
   }
|#

(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Kopie liefern mit anderem alive?-Feld
; "funktionaler Update"
(: dillo-with-alive? (dillo boolean -> dillo))

(check-expect (dillo-with-alive? dillo2 #t)
              (make-dillo #t 12))

(define dillo-with-alive?
  (lambda (dillo alive?)
    (make-dillo alive? (dillo-weight dillo))))
    
; Gürteltier füttern
(: feed-dillo (rational dillo -> dillo))

(check-expect (feed-dillo 2 dillo1)
              (make-dillo #t 12))
(check-expect (feed-dillo 2 dillo2)
              dillo2)

(define feed-dillo
  (lambda (amount dillo)
    (make-dillo (dillo-alive? dillo)
                (if (dillo-alive? dillo)
                    (+ (dillo-weight dillo) amount) ; "then-Zweig"
                    (dillo-weight dillo))
                #;(cond
                  ((dillo-alive? dillo)
                   (+ (dillo-weight dillo) amount))
                  (else (dillo-weight dillo))))))

; Ein Papagei hat folgende Eigenschaften:
; - Satz
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight rational))

(: parrot? (any -> boolean))

(define parrot1 (make-parrot "Hello!" 1))
(define parrot2 (make-parrot "Good bye!" 1.5))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))
(check-expect (run-over-parrot parrot2)
              (make-parrot "" 1.5))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; Ein Tier ist eins der folgenden:
; - ein Gürteltier
; - ein Papagei
; Fallunterscheidung: jeder Fall hat eine Signatur
; gemischte Daten
(define animal
  (signature (mixed dillo parrot)))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))

#;(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal)
       (run-over-dillo animal))
      ((parrot? animal)
       (run-over-parrot animal)))))

(define run-over-animal
  (lambda (animal)
    (match animal
      ((make-dillo alive? weight)
       (make-dillo #f weight))
      ((make-parrot sentence weight) ; (make-parrot "Hello!" 1) => sentence = "Hello!", weight = 1
       (make-parrot "" weight)))))

; Ein Fluss kommt entweder aus:
; - einer Quelle
; - einem Hauptfluss und einem Nebenfluss

; Ein Fluss ist eins der folgenden:  <- gemischt
; - ein Bach (aus einer Quelle) 
; - ein Zusammentreffen von Hauptfluss und einem Nebenfluss
;                                ^^^^^                ^^^^^
;                                Selbstbezug
(define river
  (signature (mixed creek confluence)))

; Ein Bach hat folgende Eigenschaften:  <- zusammengesetzt
; - Ursprungsort
(define-record creek
  make-creek
  creek?
  (creek-origin string))


; Ein Zusammentreffen hat folgende Eigenschaften:
; - Ort
; - Hauptfluss
; - Nebenfluss
(define-record confluence
  make-confluence
  confluence?
  (confluence-location  string)
  (confluence-main-stem river) ; <- Selstbezug
  (confluence-tributary river)) ; <- Selbstbezug

(: make-confluence (string river river -> confluence))
(: confluence? (any -> boolean))
(: confluence-location (confluence -> string))
(: confluence-main-stem (confluence -> river))
(: confluence-tributary (confluence -> river))

(define eschach (make-creek "Heimliswald"))
(define prim (make-creek "Dreifaltigkeitsberg"))
(define neckar-1 (make-confluence "Rottweil" eschach prim))
(define schlichem (make-creek "Tieringen"))
(define neckar-2 (make-confluence "Epfendorf" neckar-1 schlichem))

; Fließt Wasser von einem bestimmten Ort in Fluss?
(: flows-from? (string river -> boolean))

(check-expect (flows-from? "Heimliswald" eschach) #t)
(check-expect (flows-from? "Heimliswald" neckar-2) #t)
(check-expect (flows-from? "Tieringen" eschach) #f)

(define flows-from?
  (lambda (location river)
    (cond
      ((creek? river)
       (string=? location (creek-origin river)))
      ((confluence? river)
       (if (string=? location (confluence-location river))
           #t
           (or
            ; Fließt Wasser aus location in den Hauptfluss von river?
            (flows-from? location (confluence-main-stem river))
            ; Fließt Wasser aus location in den Nebenfluss von river?
            (flows-from? location (confluence-tributary river))))))))

; Repräsentation von endlichen Folgen:

; Eine Liste ist eins der folgenden:
; - die leere Liste
; - eine Cons-Liste bestehend aus erstem Element und Rest-Liste
;                                                         ^^^^^ Selbstbezug
; Stream<T>
#;(: list-of (signature -> signature))
#;(define list-of
  (lambda (element)
    (signature
     (mixed empty-list
            (cons-list-of element)))))


; (make-empty) -> empty
#;(match list
    (empty ...)
    ((cons first rest)
     ...))

; Die leere Liste hat folgende Eigenschaften:
; <keine>
#;(define-record empty-list
  make-empty
  empty?)

; Es gibt nur eine leere Liste
#;(define empty (make-empty))

; Eine Cons-Liste besteht aus:
; - erstes Element
; - Rest-Liste
#;(define-record (cons-list-of element) ; cons-list ist damit eine Funktion
  cons
  cons?
  (first element)
  (rest (list-of element))) ; <- Selbstbezug

(define list-of-numbers (signature (list-of number)))

(define list0 empty)
(define list1 (cons 5 empty)) ; 1elementige Liste: 5
(define list2 (cons 7 (cons 5 empty))) ; 2elementig: 7 5
(define list3 (cons 2 (cons 7 (cons 5 empty)))) ; 3elementig: 2 7 5
(define list4 (cons 3 list3)) ; 4elementig: 3 2 7 5

; Elemente einer Liste summieren
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list2) 12)
(check-expect (list-sum list4) 17)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0) ; "das neutrale Element bezüglich +"
      ((cons? list)
       (+ (first list) ; das erste Element
          (list-sum (rest list)) ; die Summe der restlichen Elemente
       )))))

; Elemente einer Liste multiplizieren
(: list-product ((list-of number) -> number))

(check-expect (list-product list4)
              210)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1) ; "das neutrale Element bezüglich *"
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; "fold"
(: list-operation (%b (%a %b -> %b) (list-of %a) -> %b))

(check-expect (list-operation 0 + (list 1 2 3 4 5))
              15)
(check-expect (list-operation 1 * (list 1 2 3 4 5))
              120)

; Schablone:
#;(define f
    (lambda (list)
      (cond
        ((empty? list) ...)
        ((cons? list)
         ... (first list)
         ... (f (rest list))))))
    
(define list-operation
  (lambda (for-empty for-cons list)
    (cond
      ((empty? list) for-empty)
      ((cons? list)
       (for-cons (first list)
                 (list-operation for-empty for-cons (rest list)))))))


(define list-length*
  (lambda (list)
    (list-operation 0 (lambda (first-of-list rest-length)
                        (+ 1 rest-length))
                    list)))

(define list-length**
  (lambda (list)
    (list-operation 0 +
                    (list-map (lambda (x) 1) list))))

(define list-length***
  (lambda (list)
    (list-operation 0 +
                    (list-map (const 1) list))))

(define list-length****
  (lambda (list)
    (list-operation 0
                    (uncurry
                     (const
                      ((curry +) 1))) ; inc
                    list)))

; Funktion erzeugen, die ihr Argument ignoriert
(: const (%a -> (any -> %a)))

(define const
  (lambda (x)
    (lambda (ignore)
      x)))

    

; Gerade Zahlen aus einer Liste extrahieren
(: extract-evens (list-of-numbers -> list-of-numbers))

(check-expect (extract-evens (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 empty)))))))
              (cons 2 (cons 4 (cons 6 empty))))

(define extract-evens
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (even? (first list))
           (cons (first list) ; das erste Element der Liste
                 (extract-evens (rest list))) ; die geraden Zahlen aus dem Rest der Liste
           (extract-evens (rest list)))))))

; Positiven Zahlen aus einer Liste extrahieren
(: extract-positives (list-of-numbers -> list-of-numbers))

(check-expect (extract-positives (cons -1 (cons 1 (cons 0 (cons 2 (cons -5 empty))))))
              (cons 1 (cons 2 empty)))

(define extract-positives
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (positive? (first list))
           (cons (first list)
                 (extract-positives (rest list))) ; die positiven Zahlen aus dem Rest der Liste
           (extract-positives (rest list)))))))


; generische Higher-Order-Funktion
(: extract-list ((%element -> boolean) (list-of %element) -> (list-of %element)))

(check-expect (extract-list even? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 empty)))))))
              (cons 2 (cons 4 (cons 6 empty))))
(check-expect (extract-list positive? (cons -1 (cons 1 (cons 0 (cons 2 (cons -5 empty))))))
              (cons 1 (cons 2 empty)))

(define dillo-list1 (cons dillo1 (cons dillo2 empty)))

(check-expect (extract-list dillo-alive? dillo-list1)
              (cons dillo1 empty))


(define extract-list
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list)
                 (extract-list p? (rest list)))
           (extract-list p? (rest list)))))))

; Liste aller überfahrenen Tiere liefern
(: run-over-animals ((list-of animal) -> (list-of animal)))

(check-expect (run-over-animals
               (cons dillo1
                     (cons parrot1
                           empty)))
              (cons (run-over-animal dillo1)
                    (cons (run-over-animal parrot1)
                          empty)))

#|
   for (Animal animal : list) {
     ...
   }
|#
(define run-over-animals
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       ; Reihenfolge wurst
       (cons (run-over-animal (first list))
             (run-over-animals (rest list)) ; die restlichen Tiere, schon überfahren
       )))))

(define run-over-animals*
  (lambda (list)
    (list-operation empty
                    (lambda (first-of-list result-of-recursive-call)
                      (cons (run-over-animal first-of-list)
                            result-of-recursive-call))
                    list)))

; Funktion auf alle Listenelemente anwenden
(: list-map ((%a -> %b) (list-of %a) -> (list-of %b)))

(define inc (lambda (x) (+ x 1)))

(check-expect (list-map inc
                        (cons 1 (cons 2 (cons 3 empty))))
              (cons 2 (cons 3 (cons 4 empty))))
(check-expect (list-map (lambda (x) (+ x 1))
                        (list 1 2 3))
              (list 2 3 4))                        

(define list-map
  (lambda (f list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       ; Reihenfolge wurst
       (cons (f (first list))
             (list-map f (rest list))
       )))))


; Tiere auf dem Highway füttern mit list-map

(check-expect (list-map (lambda (dillo)
                          ; feed-dillo auf 2 spezialisiert
                          (feed-dillo 2 dillo))
                        (list dillo1 dillo2))
              (list (feed-dillo 2 dillo1)
                    (feed-dillo 2 dillo2)))

(check-expect (list-map (lambda (x) (+ 1 x)) ; + auf 1 spezialisiert
                        (list 1 2 3))
              (list 2 3 4))

; (lambda (dillo) (feed-dillo 2 dillo))
; (lambda (x)     (+          1 x))

; Funktion spezialisieren
(: specialize ((%a %b -> %c) %a -> (%b -> %c)))
;              f             a      b      (f a b)
(check-expect (list-map (specialize + 1)
                        (list 1 2 3))
              (list 2 3 4))

(define specialize
  (lambda (f a)
    (lambda (b)
      (f a b))))

; OCaml, Haskell, F#: nur 1stellige Funktionen
; %a -> %b

; Funktion schönfinkeln
(: curry ((%a %b -> %c) -> (%a -> (%b -> %c))))

(define curry
  (lambda (f)
    (lambda (a)
      (lambda (b)
        (f a b)))))

; Funktion entschönfinkeln
(: uncurry ((%a -> (%b -> %c)) -> (%a %b -> %c)))

(check-expect ((uncurry (curry +)) 3 4)
              7)

(define uncurry
  (lambda (f)
    (lambda (a b)
      ; ((f a) b)
      (define fa (f a)) ; (%b -> %c)
      (fa b))))
      

(define make-feed-dillo
  (curry feed-dillo))

; Argumente 2stelliger Funktion vertauschen
(: flip ((%a %b -> %c) -> (%b %a -> %c)))

(check-expect ((flip make-tuple2) 1 2)
              (make-tuple2 2 1))

(define flip
  (lambda (f)
    (lambda (b a)
      (f a b))))

; Ein 2-Tupel besteht aus:
; - Teil 1
; - Teil 2
(define-record (tuple2-of a b)
  make-tuple2
  tuple2?
  (tuple2-1 a)
  (tuple2-2 b))



(define weight (signature rational))

(: feed-animal (animal weight -> animal))

(define feed-animal
  (lambda (animal amount)
    (feed-dillo amount animal))) ; GEMOGELT!

; Aus 2 Listen eine Liste aus 2-Tupeln machen
(: zip ((list-of %a) (list-of %b) -> (list-of (tuple2-of %a %b))))

(define zip
  (lambda (list1 list2)
    (cond
      ((empty? list1) empty)
      ((cons? list1)
       (cond
         ((empty? list2) empty)
         ((cons? list2)
          (cons (make-tuple2 (first list1) (first list2))
                (zip (rest list1) (rest list2)))))))))

; Wenn in diesen Sprachen (auch in Java/C++/JavaScript/Kotlin/Scala)
; eine Funktion aufgerufen wird, werden zunächst die Argumente berechnet.
; (: if (boolean %a %a -> %a))
; ^^^ Deswegen keine Funktion

; *nicht* so in Haskell

; Tiere füttern
(: feed-animals ((list-of animal) (list-of weight) -> (list-of animal)))

(define feed-animals
  (lambda (animals weights)
    (feed-animals*  (zip animals weights))))
    
(: feed-animals* ((list-of (tuple2-of animal weight)) -> (list-of animal)))

(check-expect (feed-animals* (list (make-tuple2 dillo1 3)
                                   (make-tuple2 dillo2 5)))
              (list (feed-animal dillo1 3)
                    (feed-animal dillo2 5)))

(define feed-animals*
  (lambda (tuples)
    (list-map (lambda (tuple)
                (feed-animal (tuple2-1 tuple) (tuple2-2 tuple)))
              tuples)))
