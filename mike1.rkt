;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname mike1) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
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

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal)
       (run-over-dillo animal))
      ((parrot? animal)
       (run-over-parrot animal)))))
