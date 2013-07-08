;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname images) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

(circle 100 "solid" "red")
(rectangle 10 34 "outline" "blue")
(text "( o Y o )" 38 "pink")

(above (circle 10 "outline" "blue")
       (circle 15 "solid" "yellow")
       (circle 20 "solid" "green"))

(beside (overlay (circle 1 "solid" "black")
                 (circle 5 "outline" "black")
                 (circle 20 "solid" "pink"))
        (overlay (circle 1 "solid" "black")
                 (circle 5 "outline" "black")
                 (circle 20 "solid" "pink")))
