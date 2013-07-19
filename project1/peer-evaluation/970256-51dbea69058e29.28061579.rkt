;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor-project-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

;; editor-project-starter.rkt
;;
;; In this project you will design a simple one line text editor.  
;;
;; The screen looks like:
;; 
;;     abc|def
;;
;; where | is the cursor.
;;
;; Typing a character inserts that character before the cursor.
;; The backspace key deletes the character before the cursor.
;; The left and right arrow keys move the cursor left and right.



;; =================================================================================
;; Constants:

(define WIDTH  200)
(define HEIGHT  20)

(define TEXT-SIZE  18)
(define TEXT-COLOR "BLACK")

(define CURSOR (rectangle 1 20 "solid" "red"))

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
            (to-draw    render)                  ; Editor -> Image
            (on-key     handle-key)))            ; Editor KeyEvent -> Editor



;; Editor -> Image
;; place text with cursor at left, middle edge of MTS
(check-expect (render (make-editor "abcdef" 3))
              (overlay/align "left"
                             "middle"
                             (beside (text "abc" TEXT-SIZE TEXT-COLOR)
                                     CURSOR
                                     (text "def" TEXT-SIZE TEXT-COLOR))
                             MTS))

;(define (render e) MTS) ;stub

;use template from editor
(define (render e)
  (overlay/align "left"
                 "middle"
                 (render-helper e)
                  MTS))

;; Editor -> Image
;; produce the text image with correct cursor-position
(check-expect (render-helper (make-editor "abc" 0)) (beside CURSOR (text "abc" TEXT-SIZE TEXT-COLOR)))

;(define (render-helper e) (beside CURSOR (text "abc" TEXT-SIZE TEXT-COLOR))) ;stub 

;use template from editor  
(define (render-helper e)
  (beside 
           (text (substring (editor-txt e) 0 (editor-cp e)) TEXT-SIZE TEXT-COLOR)
           CURSOR
           (text (substring (editor-txt e) (editor-cp e)) TEXT-SIZE TEXT-COLOR)
       ))

;; Editor KeyEvent -> Editor
;; call appropriate function for each keyboard command
(check-expect (handle-key (make-editor "foo" 1) "left") (make-editor "foo" 0))
(check-expect (handle-key (make-editor "foo" 1) "right") (make-editor "foo" 2))
(check-expect (handle-key (make-editor "fo" 0) "o") (make-editor "ofo" 1))
(check-expect (handle-key (make-editor "foo" 3) "\b") (make-editor "fo" 2))
(check-expect (handle-key (make-editor "foo" 3) "shift") (make-editor "foo" 3))

;(define (handle-key e key) e) ;stub

(define (handle-key e key)
  (cond [(key=? key "left")        (move-left e)]
        [(key=? key "right")       (move-right e)]
        [(key=? key "\b")          (remove-character e)]        
        [(= (string-length key) 1) (insert-key e key)]
        [else e]))

;; Editor -> Editor
;; move the cursor one position to the left, stay at 0 if pos is already 0
(check-expect (move-left (make-editor "foo" 1)) (make-editor "foo" 0))
(check-expect (move-left (make-editor "foo" 0)) (make-editor "foo" 0))

;(define (move-left e) ED2) ;stub

(define (move-left e)
  (if(eq? (editor-cp e) 0)
     e
     (make-editor (editor-txt e) (- (editor-cp e) 1))))

;; Editor -> Editor
;; move the cursor one position to the right, stay at editor-txt max-length if pos is already max-length
(check-expect (move-right (make-editor "foo" 1)) (make-editor "foo" 2))
(check-expect (move-right (make-editor "foo" 3)) (make-editor "foo" 3))

;(define (move-right e) ED2) ;stub

(define (move-right e)
  (if(eq? (editor-cp e) (string-length (editor-txt e)))
     e
     (make-editor (editor-txt e) (+ (editor-cp e) 1))))


;; Editor Key -> Editor
;; inserts a given key in editors cursor-postion and increase cursorposition 1
(check-expect (insert-key (make-editor "fo" 2) "o") (make-editor "foo" 3)) 
(check-expect (insert-key (make-editor "fo" 0) "o") (make-editor "ofo" 1))


;(define (insert-key e key) (make-editor "foo" 3)) ;stub

;use template from editor
(define (insert-key e key)
  (make-editor (string-append (substring (editor-txt e) 0 (editor-cp e))
                              key
                              (substring (editor-txt e) (editor-cp e)))
               (+ (editor-cp e) 1))
  )
  

;; Editor -> Editor
;; removes the character before current cursorposition
(check-expect (remove-character(make-editor "foo" 0)) (make-editor "foo" 0))
(check-expect (remove-character(make-editor "foo" 1)) (make-editor "oo" 0))
(check-expect (remove-character(make-editor "foo" 3)) (make-editor "fo" 2))

;(define (remove-character e) (make-editor "foo" 2)) ;stub

;use template from editor
(define (remove-character e)
  (if(eq? (editor-cp e) 0)
    e
    (make-editor (string-append (substring (editor-txt e) 0 (- (editor-cp e) 1))
                              (substring (editor-txt e) (editor-cp e)))
               (- (editor-cp e) 1))
  ))

 