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

; Eine Uhrzeit besteht aus/hat folgende Eigenschaften:
; - Stunden
; - Minuten
; zusammengesetzte Daten
(define-record time
  make-time
  (time-hour natural)
  (time-minute natural))


