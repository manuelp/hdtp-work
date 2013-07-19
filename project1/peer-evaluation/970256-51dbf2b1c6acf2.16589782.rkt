;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 970256-51dbf2b1c6acf2.16589782) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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
;; no test required for main function
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
                             MTS))                                         ;typical case

(check-expect (render (make-editor "abcdef" 0))
              (overlay/align "left"
                             "middle"
                             (beside (text "" TEXT-SIZE TEXT-COLOR)
                                     CURSOR
                                     (text "abcdef" TEXT-SIZE TEXT-COLOR))
                             MTS))                                         ;cursor at the beginning

(check-expect (render (make-editor "abcdef" 6))
              (overlay/align "left"
                             "middle"
                             (beside (text "abcdef" TEXT-SIZE TEXT-COLOR)
                                     CURSOR
                                     (text "" TEXT-SIZE TEXT-COLOR))
                             MTS))                                         ;cursor at the end

;(define (render e) MTS) ;stub
; template from Editor
(define (render e)
  (overlay/align "left"
                 "middle"
                 (beside (text (txt-b4-cp e) TEXT-SIZE TEXT-COLOR)
                         CURSOR
                         (text (txt-af-cp e) TEXT-SIZE TEXT-COLOR))
                 MTS))


;; Editor KeyEvent -> Editor
;; call appropriate function for each keyboard command
;; since this is a one-line text editor, <return> key is not supported
(check-expect (handle-key ED4 "left" ) (make-editor "abcdef" 5) )  ;pressing left when cursor is not at the beginning
(check-expect (handle-key ED2 "left" )  ED2                     )  ;pressing left when cursor is at the beginning
(check-expect (handle-key ED2 "right") (make-editor "abcdef" 1) )  ;pressing right when cursor is not at the end
(check-expect (handle-key ED4 "right")  ED4                     )  ;pressing right when cursor is at the end
(check-expect (handle-key ED2 "\b"   )  ED2                     )  ;pressing backspace when cursor is not at the beginning
(check-expect (handle-key ED4 "\b"   ) (make-editor "abcde" 5)  )  ;pressing backspace when cursor is at the beginning
(check-expect (handle-key ED1 "\b"   )  ED1                     )  ;pressing backspace when there's not text in the editor
(check-expect (handle-key ED4 "a"    ) (make-editor "abcdefa" 7))  ;pressing a alphabetical key
(check-expect (handle-key ED4 "0"    ) (make-editor "abcdef0" 7))  ;pressing a numerical key
(check-expect (handle-key ED4 "shift")  ED4                     )  ;pressing a none-alphabetical/numerical key
;(define (handle-key e key) e) ;stub

(define (handle-key e key)
  (cond [(key=? key "left")        (moveleft e)]
        [(key=? key "right")       (moveright e)]
        [(key=? key "\b")          (handle-delete e)]        
        [(= (string-length key) 1) (insert e key)]
        [else e]))

;; Editor -> Editor
;; moves the cursor position of the editor left by 1 if it's not already at 0
(check-expect (moveleft (make-editor "abcdef" 3)) (make-editor "abcdef" 2)) ;pressing left when cursor is not at the beginning
(check-expect (moveleft (make-editor "abcdef" 0)) (make-editor "abcdef" 0)) ;pressing left when cursor is at the beginning

;(define (moveleft e) e) ;stub
; template from Editor
(define (moveleft e)
  (if (= (editor-cp e) 0)
      e
      (make-editor (editor-txt e) (- (editor-cp e) 1))))

;; Editor -> Editor
;; moves the cursor position of the editor right by 1 if it's not already at the end of the strng
(check-expect (moveright (make-editor "abcdef" 3)) (make-editor "abcdef" 4)) ;pressing right when cursor is not at the end
(check-expect (moveright (make-editor "abcdef" 6)) (make-editor "abcdef" 6)) ;pressing right when cursor is at the end

;(define (moveright e) e) ;stub
; template from Editor
(define (moveright e)
  (if (= (editor-cp e) (string-length (editor-txt e)))
      e
      (make-editor (editor-txt e) (+ (editor-cp e) 1))))

;; Editor -> Editor
;; delete a character before the cursor position
(check-expect (handle-delete (make-editor "abcdef" 3)) (make-editor "abdef"  2))  ;pressing backspace when cursor is not at the beginning
(check-expect (handle-delete (make-editor "abcdef" 0)) (make-editor "abcdef" 0))  ;pressing backspace when cursor is at the beginning
(check-expect (handle-delete (make-editor ""       0)) (make-editor ""       0))  ;pressing backspace when there's not text in the editor

;(define (handle-delete e) e) ;stub
; template from Editor
(define (handle-delete e)
  (if (= (editor-cp e) 0)
      e
      (make-editor (string-append (delete-end (txt-b4-cp e)) (txt-af-cp e)) (- (editor-cp e) 1))))

;; Editor -> String
;; grabs the substring from the beginning till where the cursor position is in the editor
(check-expect (txt-b4-cp (make-editor "abcdef" 3)) "abc"   ) ;typical case
(check-expect (txt-b4-cp (make-editor "abcdef" 0)) ""      ) ;cursor at beginning
(check-expect (txt-b4-cp (make-editor ""       0)) ""      ) ;string is empty
(check-expect (txt-b4-cp (make-editor "abcdef" 6)) "abcdef") ;cursor at the end

;(define (txt-b4-cp e) "") ;stub
; template from Editor
(define (txt-b4-cp e)
  (substring (editor-txt e) 0 (editor-cp e)))

;; Editor -> String
;; grabs the substring from the cursor position till the end of the string in the editor
(check-expect (txt-af-cp (make-editor "abcdef" 3)) "def"   ) ;typical case
(check-expect (txt-af-cp (make-editor "abcdef" 0)) "abcdef") ;cursor at beginning
(check-expect (txt-af-cp (make-editor ""       0)) ""      ) ;string is empty
(check-expect (txt-af-cp (make-editor "abcdef" 6)) ""      ) ;cursor at the end

;(define (txt-af-cp e) "") ;stub
; template from Editor
(define (txt-af-cp e)
  (substring (editor-txt e) (editor-cp e)))

;; String -> String
;; grabs the substring from the beginning till where the cursor position is
(check-expect (delete-end "abc") "ab") ;typical case
(check-expect (delete-end ""   ) ""  ) ;empty string


;(define (deleteend s) "") ;stub
(define (delete-end s)
  (if (= (string-length s) 0)
      s
      (substring s 0 (- (string-length s) 1))))

; template rules used:
; - atomic non-distinct: String


;; Editor -> Editor
;; insert a single key at the position before the cursor
;; since this is a helper function called by handle-key, it is assumed that 
;; only keys with length of 1 will be pressed
;; <tab> key is supported but <enter> key is not because this is a single line text editor
(check-expect (insert (make-editor "abdef" 2) "c" ) (make-editor "abcdef" 3)) ;typical case
(check-expect (insert (make-editor "abdef" 2) "\r" ) (make-editor "abdef" 2)) ;return is pressed


; (define (insert e key) e) ;stub
; template from Editor
(define (insert e key)
  (if (string=? key "\r")
      e
      (make-editor (string-append (txt-b4-cp e) key (txt-af-cp e)) (+ 1 (editor-cp e)))))


