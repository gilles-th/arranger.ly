\version "2.19.61"
%% version Y/M/D = 2019/01/06
%% For Lilypond 2.19 or higher
%% Is a work-around for that :
%% http://code.google.com/p/lilypond/issues/detail?id=824
%% Last features/modifications since last version:
%%   replace the function anchor->moment by anchor->list
%%   add a function musicAt

\include "extractMusic.ly"
%% you can download this file here :
%% http://gillesth.free.fr/Lilypond/extractMusic/

#(define anchor-tag (gensym))

#(define* (anchor->list music-with-anchors sym
            #:optional (pred (lambda(sym syms lst)
              (and (or (pair? lst)       ; sym has been found, add anchors after sym
                       (not sym)         ; sym = #f => adds all anchors from beginning
                       (memq sym syms))  ; sym found ?
                   syms))))              ; the value to retrieve
"Returns a list of anchor informations, beginning with anchor associated with `sym and 
following wih all other anchors found after it.
An anchor information is itself a pair, with car as a moment for the anchor position
in `music-with-anchors, and cdr as either a list of symbols associated with the anchor, or #f when the very end of 
`music-with-anchors has been reached.
Set `sym to #f to retrieve all anchors from beginning of 'music-with-anchors.
Instead of a list of symbols, the user can choose to retrieve an other kind of information
by setting pred function."
(let ((moment ZERO-MOMENT)
      (res '()))
  (let loop ((evt music-with-anchors))
    (let ((e (ly:music-property evt 'element))
          (es (ly:music-property evt 'elements))
          (name (ly:music-property evt 'name))
          (tags (ly:music-property evt 'tags)))
      (cond
        ; ((memq anchor-tag tags)
;           (if (or (pair? res)     ; sym has been found, anchors after sym
;                   (not sym)       ; sym = #f => all anchors (from beginning)
;                   (memq sym tags)); the starting-anchor
;             (set! res (cons (cons moment (delq anchor-tag tags)) res))))
        ((memq anchor-tag tags)
           (let ((val (pred sym (delq anchor-tag tags) res)))
             (if val (set! res (cons (cons moment val) res)))))
        ((eq? name 'GraceMusic)) ;; do nothing (do not increase moment)
        ((or (ly:duration? (ly:music-property evt 'duration))
             (eq? name 'EventChord))
            (set! moment (ly:moment-add moment (ly:music-length evt))))
        ((eq? name 'SimultaneousMusic)
            (let ((save-mom moment)
                  (max-mom moment))
              (while (pair? es)(begin
                  (loop (car es))
                  (if (ly:moment<? max-mom moment)(set! max-mom moment))
                  (set! moment save-mom)
                  (set! es (cdr es))))
              (set! moment max-mom)))
        (else
            (if (ly:music? e)
              (let ((count (ly:music-property evt 'repeat-count)))
                (if (and (integer? count)
                         (null? es)
                         (not (eq? name 'VoltaRepeatedMusic)))
                  (loop (make-sequential-music (make-list count e)))
                  (loop e))))
            (while (pair? es); sequential music
              (begin
               (loop (car es))
               (set! es (cdr es))))))))
 (reverse (cons (list moment #f) res))))

                  %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% anchor = #(define-music-function (sym)(symbol-list-or-symbol?)
% "Return a tagged skip, which moment position in a music can be retrieved by the
% function anchor->list."
% #{ \tag $anchor-tag \tag $sym s1*0 #})

anchor = #(define-music-function (sym)(symbol-list-or-symbol?)
"Return from a symbol, a zero length music, which moment position in a music can 
be retrieved by the function anchor->list.
A anchor reset relative mode to c'"
  #{ \tag $anchor-tag \tag $sym \resetRelativeOctave #(ly:make-pitch 0 0) #})

                  %%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define (delete-anchors music)
  (music-filter
    (lambda(x)(not (memq anchor-tag (ly:music-property x 'tags))))
    music))

musicAt = #(define-music-function (sym music)(symbol? ly:music?)
"Extract the music between the \\anchor sym and the following \\anchor"
(let ((anchors (anchor->list music sym)))
   ; (display anchors)
   (if (null? (cdr anchors))
     (make-music 'Music 'void #t)
     (let* ((from (car (first anchors)))
            (during (ly:moment-sub (car (second anchors)) from)))
       (delete-anchors (extract-during music from during))))))

                  %%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (anchor->moment music sym)
   (car (first (anchor->list music sym
                 (lambda(sym syms lst)(and (memq sym syms)
                                           syms))))))

addAt = #(define-music-function (sym music insertMusic)
                                          (symbol? ly:music? ly:music?)
"Replace the music beginning at the \\anchor sym by insertMusic"
(let ((where (make-music 'SkipEvent 'duration
               (make-duration-of-length (anchor->moment music sym)))))
  #{ \insertMusic $music $where $insertMusic #}))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%{
global = \relative c'
{
 \time 4/4 s4*4*10
 \time 3/4 s4*3*5
 \time 7/4 s4*7
 \time 4/4 \anchor #'coda s4*4*10
 \bar "|."
}

\new Voice \addAt #'coda \global {\tempo \markup "Extremely slow"}

  %}



