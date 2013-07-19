;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |editor-nearly there|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

;; Constants:

(define WIDTH  200)
(define HEIGHT  20)

(define TEXT-SIZE  18)
(define TEXT-COLOR "BLACK")

(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

(define MTS (empty-scene WIDTH HEIGHT))



;; =================================================================================
;; Data Definitions:

(define-struct editor (txt cp))
;; Editor is (make-editor String Natural)
;; interp. the current text (txt) and cursor position (cp) using a 0-based index

(define ED1 (make-editor ""       0)) ; empty
(define ED2 (make-editor "abcdef" 0)) ; cursor at beginning as in |abcdef
(define ED3 (make-editor "abcdef" 3)) ; cursor in middle of text as in abc|def
(define ED4 (make-editor "abcdef" 6)) ; cursor at end as in abcdef|

#;
(define (fn-for-editor e)
  (... (editor-txt e)
       (editor-cp e)))

;; =================================================================================
;; Functions:

;; Editor -> Editor
;; start the world with an initial state e, for example (main (make-editor "" 0))
(define (main e)
  (big-bang e
            (to-draw    render)          ; Editor -> Image
            (on-key     handle-key)))    ; Editor KeyEvent -> Editor



;; Editor -> Image
;; place text with cursor at left, middle edge of MTS
(check-expect (render (make-editor " " 0))
              (overlay/align "left" "middle" CURSOR MTS))

(check-expect (render (make-editor "abcdef" 0))
              (overlay/align "left"
                             "middle"
                             (beside CURSOR
                                     (text "abcdef" TEXT-SIZE TEXT-COLOR))
                             MTS))

(check-expect (render (make-editor "abcdef" 3))
              (overlay/align "left"
                             "middle"
                             (beside (text "abc" TEXT-SIZE TEXT-COLOR)
                                     CURSOR
                                     (text "def" TEXT-SIZE TEXT-COLOR))
                             MTS))

(check-expect (render (make-editor "abcdef" 6))
              (overlay/align "left"
                             "middle"
                             (beside (text "abcdef" TEXT-SIZE TEXT-COLOR)
                                     CURSOR)
                             MTS))

;(define (render e) MTS)     ;stub

(define (render e)
  (overlay/align "left" 
                 "middle" 
                 (beside (text (pre-c e) TEXT-SIZE TEXT-COLOR) 
                         CURSOR
                         (text (post-c e) TEXT-SIZE TEXT-COLOR))
                 MTS))


;; Editor -> String
;; produce string of characters to the left of editor-cp from provided Editor
(check-expect (pre-c (make-editor "abcdef" 3)) "abc")
(check-expect (pre-c (make-editor "abcdef" 6)) "abcdef")
(check-expect (pre-c (make-editor "abcdef" 0)) "")

;(define (pre-c e) "abc")     ;stub
;took template from Editor
#; (define (fn-for-editor e)
     (... (editor-txt e)
          (editor-cp e)))

(define (pre-c e)
  (substring (editor-txt e) 0 (editor-cp e)))

;; Editor -> String
;; produce string of characters to the right of editor-cp from provided Editor
(check-expect (post-c (make-editor "abcdef" 3)) "def")
(check-expect (post-c (make-editor "abcdef" 6)) "")
(check-expect (post-c (make-editor "abcdef" 0)) "abcdef")

;(define (post-c e) "def")     ;stub
;took template from Editor
#; (define (fn-for-editor e)
     (... (editor-txt e)
          (editor-cp e)))

(define (post-c e)
  (substring (editor-txt e) (editor-cp e)))


;; Editor KeyEvent -> Editor
;; call appropriate function for each keyboard command
(check-expect (handle-key (make-editor "abcdef" 1) "left") (make-editor "abcdef" 0))
(check-expect (handle-key (make-editor "abcdef" 0) "left") (make-editor "abcdef" 0))
(check-expect (handle-key (make-editor "abcdef" 3) "right") (make-editor "abcdef" 4))
(check-expect (handle-key (make-editor "abcdef" 6) "right") (make-editor "abcdef" 6))
(check-expect (handle-key (make-editor "abcdef" 4) "\b") (make-editor "abcef" 3))
(check-expect (handle-key (make-editor "abcdef" 6) "a") (make-editor "abcdefa" 7))
(check-expect (handle-key (make-editor "abcdef" 4) "control") (make-editor "abcdef" 4))

;(define (handle-key e key) ED2)     ;stub

(define (handle-key e key)
  (cond [(key=? key "left") (move-left e "left")]
        [(key=? key "right") (move-right e "right")]
        [(key=? key "\b") (delete e "\b")]        
        [(= (string-length key) 1) (insert e key)]
        [else e]))

;; Editor KeyEvent -> Editor
;; moves cursor one position to the left when "left" arrow pressed, if cp 0 remain at 0
(check-expect (move-left (make-editor "abcdef" 3) "left") (make-editor "abcdef" 2))
(check-expect (move-left (make-editor "abcdef" 0) "left") (make-editor "abcdef" 0))

;(define (move-left e key) ED2)     ;stub
;took template from Editor
#; (define (move-left e)
     (... (editor-txt e)
          (editor-cp e)))

(define (move-left e key)
  (if (> (editor-cp e) 0)
      (make-editor (editor-txt e) (- (editor-cp e) 1))
      e))

;; Editor KeyEvent -> Editor
;; moves cursor one position to the right when "right" arrow pressed, does not go beyond editor-txt
(check-expect (move-right (make-editor "abcdef" 3) "right") (make-editor "abcdef" 4))
(check-expect (move-right (make-editor "abcdef" 6) "right") (make-editor "abcdef" 6))

;(define (move-right e key) ED2)     ;stub
;took template from Editor
#; (define (move-right e)
     (... (editor-txt e)
          (editor-cp e)))

(define (move-right e key)
  (if (< (editor-cp e) (string-length (editor-txt e)))
      (make-editor (editor-txt e) (+ (editor-cp e) 1))
      e))

;; Editor KeyEvent -> Editor
;; delete character to the left of cursor (if there is one) and move cursor one position to left when delete/backspace pressed
(check-expect (delete (make-editor "abcdef" 4) "\b") (make-editor "abcef" 3))
(check-expect (delete (make-editor "abcdef" 7) "\b") (make-editor "abcdef" 7))
(check-expect (delete (make-editor "" 0) "\b") (make-editor "" 0))

;(define (delete e key) ED2)     ;stub
;took template from Editor
#; (define (delete e)
     (... (editor-txt e)
          (editor-cp e)))

(define (delete e key)
  (if (and (<= (editor-cp e) (string-length (editor-txt e))) (>= (string-length (editor-txt e)) 1))
      (make-editor (string-append (substring (pre-c e) 0 (- (editor-cp e) 1)) (post-c e)) 
                   (- (editor-cp e) 1))
      e))

;; Editor KeyEvent -> Editor
;; adds appropriate 1-character string after editor-cp and move cursor one position to right
(check-expect (insert (make-editor "abcdef" 3) "a") (make-editor "abcadef" 4))
(check-expect (insert (make-editor "abcdef" 0) "1") (make-editor "1abcdef" 1))
(check-expect (insert (make-editor "abcdef" 6) " ") (make-editor "abcdef " 7))

;(define (insert e key) ED2)     ;stub
;took template from Editor
#; (define (insert e)
     (... (editor-txt e)
          (editor-cp e)))

(define (insert e key)
  (make-editor (string-append
                (pre-c e)
                key
                (post-c e))
               (+ (editor-cp e) 1)))
