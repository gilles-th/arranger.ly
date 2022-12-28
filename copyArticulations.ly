\version "2.24.0"

%%%%%%%%%%%%%%%%%%%%%% version Y/M/D = 2022/12/28 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LSR = http://lsr.di.unimi.it/LSR/Item?id=769
%% last changes :
%%   - tuplets have now a duration . fix line 92-93 and 179
%%   - fix a typo line 44 : (set! (cons...) => (set! res (cons ...)
%%   - fix a bug for \notCopyArticulations and q chords (has-notes? must
%%     be called)


#(use-modules (ice-9 receive)) %% for the use of receive

#(define skipCurrentArticulationTag (gensym))
#(define notCopyArticulationsTag (gensym))


#(define (defined-music? music)
   (not (eq? 'Music (ly:music-property music 'name))))

#(define (name-of music)
 (ly:music-property music 'name))

#(define (noteEvent? music)
(eq? (name-of music) 'NoteEvent))

#(define (has-notes? music)
"Return the first note of music or false if there is not at least one note"
 (or (and (noteEvent? music) music)
     (let ((e (ly:music-property music 'element)))
        (and (ly:music? e)
             (has-notes? e)))
     (any has-notes? (ly:music-property music 'elements '()))))

#(define (make-arti-list pattern)
"Make a list of articulations "
 (let ((res '()))     ; the list to fill
   (for-some-music    ; see music-functions.scm
     (lambda (evt)
       (case (name-of evt)
         ((EventChord)
           (receive (notes others)
             (partition noteEvent? (ly:music-property evt 'elements))
             (if (pair? notes)(set! res (cons others res))) ; keeps not-note-events
             res))                                          ; stop process
         ((NoteEvent)
             (set! res (cons (ly:music-property evt 'articulations) res))
             res)
         (else #f)))
     pattern)
   (reverse res)))

%% use (ly:music-deep-copy music) for a direct use of copy-articulations
#(define (copy-articulations pattern music)
"Copy articulations of `pattern recursively in each notes of music."
(if (not (has-notes? pattern))              ; avoid endless loops ...
  music
  (let* ((new-arti (make-arti-list pattern))
         (pointer-list new-arti)
         (current-arti (lambda ()
            (let ((res (car pointer-list)))
              (set! pointer-list (cdr pointer-list))
              (if (null? pointer-list)(set! pointer-list new-arti))
              res))))
    (music-filter
      defined-music?                        ; deletes all (make-music 'Music)
      (map-some-music ; see music-functions.scm
        (lambda(evt)
          (let ((tags (ly:music-property evt 'tags)))
             (cond
               ((memq skipCurrentArticulationTag tags); yes => evt = <>
                     (current-arti)                ; ignores return
                     (make-music 'Music))          ; will be deleted...
               ((memq notCopyArticulationsTag tags)
                   (ly:music-set-property! evt 'tags ; just remove the tag
                        (delq notCopyArticulationsTag tags))
                   evt)
               ((noteEvent? evt)
                   (let ((arti (ly:music-property evt 'articulations)))
                     (ly:music-set-property! evt 'articulations
                       (append arti (current-arti)))
                     evt))
               ((eq? 'EventChord (name-of evt))
                   (let ((elts (ly:music-property evt 'elements)))
                     (and
                      (pair? (filter noteEvent? elts))
                      (begin
                       (ly:music-set-property! evt 'elements
                                 (append elts (current-arti)))
                       evt))))
               ;(else (and (ly:music-property evt 'duration #f) ; tuplets now can have
               ;           evt)))))                             ; a duration !
               (else #f))))	                                    ; continue loop
          (expand-repeat-chords! (list 'rhythmic-event) music))))))

copyArticulations = #(define-music-function (pattern music)
                                                          (ly:music? ly:music?)
"Copy articulations of `pattern recursively in each notes of music."
 (copy-articulations pattern music))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% enhancement functions, working inside the music parameter
%% of \copyArticulations
                         %%%%%%%
notCopyArticulations =
#(define-music-function (music)(ly:music?)
"Add a special tag in 'tags of all notes and chords, that will forbid 
articulations to be copied to these events."
(map-some-music
 (lambda(evt)
   (cond ((memq (name-of evt) '(NoteEvent EventChord))
            (ly:music-set-property! evt 'tags (cons
               notCopyArticulationsTag (ly:music-property evt 'tags)))
            evt)
          (else #f))) ; go deeper
 music))

skipArti = \tag #skipCurrentArticulationTag <>

nSkipArti = #(define-music-function (n)(integer?)
"Return \\skipArti \\skipArti \\skipArti ... n times."
(if (< n 2)
  skipArti
 (make-sequential-music (make-list n skipArti))))

#(define (skip-tied-notes mus)
(let ((prev-was-tied #f))
  (map-some-music
   (lambda(evt)
     (let ((name (name-of evt)))
       (cond
        ((or (eq? name 'NoteEvent)
             (and (eq? name 'EventChord)
                  (has-notes? evt)))
         (if prev-was-tied (ly:music-set-property! evt 'tags
                             (cons notCopyArticulationsTag (ly:music-property evt 'tags))))
         (map-some-music
          (lambda(x)
            (set! prev-was-tied (and (eq? (name-of x) 'TieEvent) x))
            prev-was-tied)  ; stop if true
          evt))
        (else #f))))
   (expand-repeat-chords! (list 'rhythmic-event) mus))))

skipTiedNotes = #(define-music-function (music)(ly:music?)
"Add a special tag in 'tags of second tied notes to forbid articulations to be 
copied."
(skip-tied-notes music))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                  %% adding bracket to chords %%

%% \copyArticulations { c\arpeggio } { \arpeggioBracket \music } works well,
%% but we want brackets only to chords with at least 2 notes (not to notes alone), and not to tied
%% chords. So we need a special function.

#(define (add-arp mus)
(map-some-music
 (lambda(evt)
   (let ((tags (ly:music-property evt 'tags)))
     (cond
      ((memq notCopyArticulationsTag tags)
       (ly:music-set-property! evt 'tags ; just remove the tag
         (delq notCopyArticulationsTag tags))
       evt)
      ((eq? 'EventChord (name-of evt))
       (let ((ees (ly:music-property evt 'elements)))
         (if (let loop ((n 0)   ;; takes chords with at least 2 notes
                        (notes ees))
               (or (> n 1)
                   (and (pair? notes)
                        (loop (if (eq? 'NoteEvent (name-of (car notes)))
                                  (1+ n) n)
                              (cdr notes)))))
             (ly:music-set-property! evt 'elements
               (append ees (list (make-music 'ArpeggioEvent)))))
         evt))
      ;(else (and (ly:music-property evt 'duration #f) evt)))))
      (else #f))))
 mus))

#(define (braketify-chords mus) ;; this scheme function is used in "arranger.ly"
   #{ \arpeggioBracket $(add-arp (skip-tied-notes mus)) #})


braketifyChords = #(define-music-function (music) (ly:music?)
"Adds brackets to chords with at least 2 notes."
  (braketify-chords music))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                      %% shortcuts %%
note = #(make-music 'NoteEvent 'duration (ly:make-duration 2) ; independant language note
                               'pitch (ly:make-pitch 0 0))
artiI = { \note-. }   % default values for artiI and artiII, if the user does not define
artiII = { \note( \note) \note-. \note-. }  % them, before using \cAI and \cAII

cAI = #(define-music-function (music) (ly:music?)
#{ \copyArticulations \artiI $music #})

cAII = #(define-music-function (music) (ly:music?)
#{ \copyArticulations \artiII $music #})

#(define cA copyArticulations)
#(define notCA notCopyArticulations)
