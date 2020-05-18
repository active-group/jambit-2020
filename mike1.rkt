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