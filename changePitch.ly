\version "2.24.0"

%%%%%%%%%%%%%%%%%%%%%% version Y/M/D = 2022/12/28 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LSR =  http://lsr.di.unimi.it/LSR/Item?id=654
%% Last release here:
%%   http://gillesth.free.fr/Lilypond/changePitch/
%% This directory contains also a doc called: changePitch-doc.pdf
%% Last changes (the more recent to the top) :
%%    - \relative in lilypond has changed internally. The lines in copy-dur+arti
%%      that used to manage the relative mode are no longer required. They are deleted.
%%    - combine copy-duration and copy-arti into 1 function: copy-dur+arti
%%    - function check-for-ties has been redone.
%%    - allows \displayLilyMusic to work, using reduce-seq in a music with a \tempo command
%%    - correction in \samePitch : music can contain no-pitch notes => call expand-notes-and-chords
%%    - new changPitch-doc.pdf (made with Context => http://wiki.contextgarden.net/ )
%%    - changePitch checks now, in his pattern arguments, if there is notes with ties,
%%      and automatically groups them with the tieded note in a \samePitch section.
%%      It is possible to desactivate this behaviour, for pieces made with previous
%%      release of changePitch. See doc...
%%    - \tuplet in lilypond 2.18 have now a 'duration property ! It is no more
%% 	    reserved for notes and rests  => we have to change some pieces of
%%      code in change-pitch function.
%%    - \changePitch : a single s as the last event of newnotes parameter
%%      will give you the ending rests of the pattern (if there), and two s
%%      also the beginning ones. If pattern ends with a note, a single
%%      ending s will have no effects.
%%    - changePitch.ly is now \language independant (no more #{ c #})
%%    - new algorithm for make-notes-list, change-pitch
%%      and has-notes? (shorter and safer)
%%
%%%%%%%%%%%%%%%%%%%%%%%%% some utility functions %%%%%%%%%%%%%%%%%%%%%
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

%% An EventChord is sometimes used as a wrapper in Lilypond, so we have to check
%% if a chord is a standard chord with notes. We could have used has-notes?
%% but this version is probably more efficient.
%% Optional events name like 'RestEvent can be included.
#(define (note-or-chord? music . otherEvent)
"Is music a note or a chord with notes ?"
(let ((name (name-of music)))
  (or (memq name (cons 'NoteEvent otherEvent))
      (and (eq? name 'EventChord)  ; have this chord at least one note ?
           (any noteEvent? (ly:music-property music 'elements))))))

#(define (reduce-seq mus)
"Try to reduce the number of sequential music"
   (define (merge-in-list m l)
     (let ((name (name-of m)))
       (cond
         ((eq? name 'SequentialMusic)
            (let ((elts (ly:music-property m 'elements)))
              (cond ((null? elts)
                       (if (ly:music-property m 'to-relative-callback #f)
                         (cons m l) ;; adds \resetRelativeOctave
                         l))  ;; skip m : well not sure it is absoluty safe...
                    ; the \tempo command makes a sequential music of 2 events (the first is a 'TempoChangeEvent). It seems
                    ; that \displayLilyMusic need this sequential music to work ! Is there others Lilypond commands like that ?
                    ((or (eq? 'TempoChangeEvent (name-of (car elts)))
                         (pair? (ly:music-property m 'tags)))  ; a tag can be important !
                       (cons m l))  ; don't reduce the sequential music
                    (else (fold-right merge-in-list l elts))))) ; do reduce
         ((memq name '(RelativeOctaveMusic UnrelativableMusic TransposedMusic))
            (merge-in-list (ly:music-property m 'element) l))
         ((eq? name 'SimultaneousMusic)
             (ly:music-set-property! m 'elements
               (map reduce-seq (ly:music-property m 'elements)))
             (cons m l))
         (else (cons m l)))))
(let ((name (ly:music-property mus 'name)))
  (cond
    ((eq? name 'SequentialMusic)
      (let ((elts (ly:music-property mus 'elements)))
         (ly:music-set-property! mus 'elements
             (fold-right merge-in-list '() elts))))
    ((eq? name 'SimultaneousMusic)
       (ly:music-set-property! mus 'elements
              (map reduce-seq (ly:music-property mus 'elements))))
    ((memq name (list 'RelativeOctaveMusic 'UnrelativableMusic 'TransposedMusic))
       (ly:music-set-property! mus 'element
              (reduce-seq (ly:music-property mus 'element)))))
  mus))

#(define (expand-notes-and-chords music) ; resolves { c2.~ 4 <c e>2. 4 <c e>2.~ q4 }
"Makes sure that all notes have a pitch !"
(expand-repeat-notes! (expand-repeat-chords! (list 'rhythmic-event) music)))
% (list 'rhythmic-event) can be replaced by (cons 'rhythmic-event (ly:parser-lookup '$chord-repeat-events))

#(define (expand-notes-and-chords-copy-of music) ; for arranger.ly
   (expand-repeat-notes!
     (expand-repeat-chords! (list 'rhythmic-event)
       (ly:music-deep-copy music))))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% changePitch %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define cPInsertInPattern (gensym))
#(define cPSamePitch (gensym))
#(define cPSamePitchEnd (gensym))
#(define fakeTie (gensym))
#(define cPPatternEnd (gensym))

#(define (make-notes-list music)
"Make a list with each element has one of these types :
  1- a note, a chord, a rest
  2- an integer, indicating the number of notes to skip in pattern ( The user will
     indicate that, by a corresponding number of skips (s or \\skip) in `newnotes 
     parameter of \\changePitch )
  3- a list of musics, to be inserted between 2 notes of pattern, and added with
     the \\insert function, inside `newnotes"
(let ((res '())     ; the list to fill
      (prev #f))
  (define (fill-notes-list evt)
    (let ((tags (ly:music-property evt 'tags))
          (name (name-of evt)))
      (cond
        ((memq cPInsertInPattern tags)             ; a music added by \insert
            (ly:music-set-property! evt 'tags
                          (delq cPInsertInPattern tags))     ; remove the tag
            (if (integer? prev)(set! res (cons prev res)))
            (set! prev (if (pair? prev)(cons evt prev)(list evt))))  ; a list
         ((memq name (list 'SkipEvent 'SkipMusic))
            (if (pair? prev)(set! res (cons prev res))) ; keeps the reverse order
            (set! prev (if (integer? prev) (1+ prev) 1)))
       ; ((memq name (list 'EventChord 'NoteEvent 'RestEvent))
         ((note-or-chord? evt 'RestEvent) ; a note, a chord, or a rest
            (if (or (pair? prev)(integer? prev))(set! res (cons prev res)))
            (set! prev evt)
            (set! res (cons evt res)))
         (else
          (let ((elt (ly:music-property evt 'element))
                (elts (ly:music-property evt 'elements)))
            (if (ly:music? elt) (fill-notes-list elt))
            (if (pair? elts)(for-each fill-notes-list elts)))))))
(fill-notes-list music)
(if (or (pair? prev)(integer? prev))(set! res (cons prev res)))
(reverse res)))

%%%%%%%%%%%%  used inside change-pitch
#(use-modules (ice-9 receive)) %% for the use of receive
#(define (copy-dur+arti from to) ; from and to as EventChord or NoteEvent
"Copy duration and articulation between 2 notes or chords"
(let((es-from (ly:music-property from 'elements))
     (es-to (ly:music-property to 'elements)))
  (receive (notes-from artis-from)
    (if (null? es-from)
      (values (list from)(ly:music-property from 'articulations)) ; a NoteEvent
      (partition noteEvent? es-from))                             ; an EventChord
    (let* ((durs-from (map (lambda(note)(ly:music-property note 'duration)) notes-from))
           (dur-from (fold    ; 2 notes in a chord can have different durations !
                       (lambda(dur1 dur2)(if (ly:duration<? dur1 dur2) dur2 dur1))
                       (car durs-from) ; initial and default value
                       (cdr durs-from))))
      (if (null? es-to)
        (begin                                       ; a NoteEvent
          (ly:music-set-property! to 'duration dur-from)
          (ly:music-set-property! to 'articulations
            (append (ly:music-property to 'articulations) artis-from)))
        (let((notes-to (filter noteEvent? es-to)))   ; an EventChord
          (for-each (lambda(evt)(ly:music-set-property! evt 'duration dur-from))
                    notes-to)
          (ly:music-set-property! to 'elements (append es-to artis-from)))))
      (ly:music-set-property! to 'tags
        (append (ly:music-property from 'tags)(ly:music-property to 'tags))))))

%% del-note-arti is called for all notes but the first of a \samePitch section.
#(define (del-note-arti note-or-chord)
(ly:music-set-property! note-or-chord 'tags '())
(map-some-music
  (lambda(x)
    (if (eq? (name-of x) 'EventChord)
      (begin (ly:music-set-property! note-or-chord 'elements ; del all but notes
               (filter (lambda(y)(eq? (name-of y) 'NoteEvent))
                       (ly:music-property note-or-chord 'elements)))
             #f)  ; don't stop recursing
      (begin (ly:music-set-property! x 'articulations '())   ; x is a note
             (ly:music-set-property! x 'force-accidental #f)
             (ly:music-set-property! x 'cautionary #f)
             x))) ; stop recursing
  note-or-chord))

#(define ((set-tags-prop-if-not tag-stop) evt tag-to-add . tags-to-del)
"Add and delete tags in evt 'tags property, unless tag-stop is in it.
Set tag-stop to #f to process without conditions"
(let ((filtered-tags (fold delq (ly:music-property evt 'tags) tags-to-del)))
  (if (not (and tag-stop (memq tag-stop filtered-tags)))
    (ly:music-set-property! evt 'tags
      (if (memq tag-to-add filtered-tags) filtered-tags (cons tag-to-add filtered-tags))))))
#(define set-tags-prop (set-tags-prop-if-not #f))

#(define (set-to-same-pitch-callback evt)
   (define (same-pitch-callback x p)           ; set pitch to the prev value
     (ly:prob-set-property! x 'pitch p)
     p)
(ly:music-set-property! evt 'to-relative-callback same-pitch-callback))

#(define (check-for-ties pattern)
"A tied note will get automatically the same pitch than the previous note (= the 
note with the tie symbol)"
(let loop1 ((notes (extract-named-music pattern '(NoteEvent EventChord))))
  (if (null? notes)
    pattern
    (let loop2 ((note (car notes))
                (next (cdr notes)))
      (if (null? next)
        pattern
        (if (or (pair? (extract-named-music note '(TieEvent)))
                (memq fakeTie (ly:music-property note 'tags)))
          (let loop3 ((next-note (car next))
                      (next (cdr next)))
            (set-to-same-pitch-callback next-note)   ; a note will have same pitch as prev
            (if (and (pair? next)
                     (pair? (extract-named-music next-note '(TieEvent))))
              (begin   ; next-note has also a tie => add cPsamePitch tag to next-note
                (set-tags-prop next-note cPSamePitch fakeTie cPSamePitchEnd) ; add cPSamePitch, del fakeTie cPSamePitchEnd
                (loop3 (car next)(cdr next)))
              (begin   ; next-note has no tie, or no more notes after next-note.
                (set-tags-prop note cPSamePitch fakeTie cPSamePitchEnd) ; add to note
                ((set-tags-prop-if-not cPSamePitch) next-note cPSamePitchEnd) ; add cPSamePitchEnd to next-note if not cPSamePitch
                (loop1 next))))
          (loop2 (car next)(cdr next))))))))

#(if (and (defined? 'cPCheckForTies) ; compatibility for files made with previous version. To add :
          (not cPCheckForTies))      ; (define cPCheckForTies #f) before \include "changePitch.ly"
    (ly:parser-define! 'check-for-ties (lambda(pattern) pattern)))

#(define (change-pitch pattern newnotes)
"The scheme function of \\changePitch, `pattern and `newnotes as music."
(let ((seq-list '())           ; list of transformed patterns
      (skip-notnote-event? #f) ; #t if a \skip or an \insert is found in newnotes
      (same-pitch-section? #f) ; #t if we are in the 2nd pattern note of a\samePitch section
      ;(dummy-note #{ c4 #})    ; \language dependant -:(
      (dummy-note (make-music 'NoteEvent ; to avoid pbs with pattern without any notes
                              'duration (ly:make-duration 2 0 1) ;
                              'pitch (ly:make-pitch -1 0 0))) ;
      (pattern2 (make-sequential-music (list
           (check-for-ties pattern)          ; { c4~ c8 } => \samePitch { c4~ c8 }
           #{ \tag #cPPatternEnd s4 #})))    ; to detect the end of pattern
      ;; (pattern2 #{ $pattern \tag #cPPatternEnd s4 #})
      (last-notes-list #f))    ; buffer
 (set! seq-list (cdr  ; skip dummy notes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let loop ((notes-list (cons dummy-note (make-notes-list newnotes))); see make-notes-list
             (pat-list (cons dummy-note (circular-list pattern2)))
             (res '())) ; the list to fill
    (if (or (null? notes-list)(null? pat-list)) ; pat-list can be a regular list in the loop
      (reverse res)               ;;;;;; return the list in the right order
      (let ((x (car notes-list))  ;;;;;; go deeper, taking 1st elt of each lists
            (evt (ly:music-deep-copy (car pat-list))))
       (cond
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ((pair? x)                   ; x is a list of musics, added with \insert in newnotes
          (set! skip-notnote-event? #t)      ; for events between 2 pattern notes
          (set! last-notes-list notes-list)  ; for section "else" of this cond statement
          (loop (cdr notes-list) pat-list (append x res))) ; append x to res
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ((note-or-chord? evt)         ; evt in pattern is a note or a chord (with notes)
          (set! last-notes-list notes-list)  ; for section "else" later
          (cond
            ((ly:music? x)  ;;;;;; the elt of notes-list is a note or a chord
               (if same-pitch-section? ; x is used several times. Copy arti of x only to first
                 (set! x (del-note-arti (ly:music-deep-copy x)))) ; note of \samePitch section
               (copy-dur+arti evt x)  ; evt = from, x = to
               (let ((tags (ly:music-property x 'tags)))
                 (cond               ; are we in a \samePitch section ?
                   ((memq cPSamePitch tags)    ; yes, first,remove the tag
                      (ly:music-set-property! x 'tags (delq cPSamePitch tags))
                      (set! same-pitch-section? #t)) ; then set the flag
                   ((memq cPSamePitchEnd tags) ; last note of \samePitch
                      (ly:music-set-property! x 'tags (delq cPSamePitchEnd tags))
                      (set! same-pitch-section? #f))))      ; unset the flag
               (set! skip-notnote-event? #f); stop deletion of not-notes event.
               (if same-pitch-section?
                   (loop notes-list (cdr pat-list)(cons x res))
                   (loop (cdr notes-list)(cdr pat-list)(cons x res)))) ; next new note
            ((integer? x)   ;;;;;; user want to skip over the current evt note. We also
               (set! skip-notnote-event? x) ; don't add any events bis next pattern note
               ;; (format #t "x : ~a\n" x)  ; for testing
               (cond ((= x 1)                                      ; only one s
                        (loop (cdr notes-list)(cdr pat-list) res)) ; next notes-list elt
                     (else                                         ; several successive s
                        (set-car! notes-list (1- x))               ; for the next loop
                        (loop notes-list (cdr pat-list) res))))))  ; the next evt only
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ((or (and
               (not (eq? 'TimeScaledMusic (name-of evt))) ; \tuplet have  a 'duration !
               (ly:music-property evt 'duration #f))  ; current evt is a skip or a rest
             (not (has-notes? evt)))                  ; \clef, \time, \override for ex.
           (cond ((memq cPPatternEnd (ly:music-property evt 'tags)) ; last evt of pattern
                    (let ((x (car notes-list)))
                      (if (and (integer? x)
                                (or (null? (cdr notes-list))        ; last elt ?
                                    (and (null? (cddr notes-list))  ; 2nd to last and last is
                                         (pair? (cadr notes-list))))) ; a \insert section
                        (cond
                           ((= x 1)
                              (set! skip-notnote-event? x)
                              (loop (cdr notes-list) (cdr pat-list) res))
                           (else
                              (set-car! notes-list (1- x))
                              (loop notes-list (cdr pat-list) res))))
                        (loop notes-list (cdr pat-list) res))) ;; ignores evt
                  (skip-notnote-event? (loop notes-list (cdr pat-list) res))
                  (else (loop notes-list (cdr pat-list)(cons evt res)))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (else
          (let ((e (ly:music-property evt 'element))
                (es (ly:music-property evt 'elements))
                (empty? #f)) ; don't add to res if #t
           (if (ly:music? e)
             (let ((new-e (loop notes-list (list e) '())))
               (ly:music-set-property! evt 'element
                 (case (length new-e)
                   ((0) (set! empty? #t)
                        new-e)
                   ((1) (car new-e))
                   (else (make-sequential-music new-e)))))
             (set! empty? #t))
           (if (pair? es)
             (let ((new-es (loop notes-list es '())))
               (ly:music-set-property! evt 'elements new-es)
               (if empty? (set! empty? (null? new-es))))) ; #t if both empty
           (let ((next-new-notes (if (or same-pitch-section?
                                         (and (integer? skip-notnote-event?)
                                              (> skip-notnote-event? 1)))
                                   last-notes-list
                                   (cdr last-notes-list))))
              (if empty? (loop next-new-notes (cdr pat-list) res)
                         (loop next-new-notes (cdr pat-list) (cons evt res))))))))))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (let ((relativize (lambda(m)
        (let* ((clean-newnotes (reduce-seq newnotes))
               (name (name-of clean-newnotes)))
            (if (memq name (list 'RelativeOctaveMusic 'UnrelativableMusic))
               (make-music name 'element m)
               m)))))
     (cond
        ((null? seq-list) (make-music 'Music))               ; 0 elt
        ((null? (cdr seq-list)) (relativize (car seq-list))) ; 1 elt
        (else (relativize (reduce-seq (make-sequential-music seq-list))))))))

changePitch = #(define-music-function (pattern newnotes) (ly:music? ly:music?)
"Change each notes in `pattern by the notes (or rests) given in `newnotes.
If count of events doesn't match, pattern is duplicated repeatedly or truncated."
 (change-pitch (expand-notes-and-chords pattern) (expand-notes-and-chords newnotes)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% enhancement functions, working with \changePitch pattern newnotes

samePitch = #(define-music-function (music) (ly:music?)
"Inside the `pattern parameter of the \\changePitch function, all notes grouped 
by this function will have the same pitch, according to the current note of
`newnotes parameter of \\changePitch."
(let((notes (extract-named-music music '(NoteEvent EventChord))))
  (for-each set-to-same-pitch-callback (cdr notes))       ; fix all pitches to first pitch
  (let ((rev-notes (reverse notes)))                      ; reverse the list
    (set-tags-prop (car rev-notes) cPSamePitchEnd)        ; last note => tag cPSamePitchEnd
    (for-each set-tags-prop (cdr rev-notes)               ; others notes =>
                            (circular-list cPSamePitch))) ; tag cPSamePitch
  music))

% for arranger.ly users
#(define (same-pitch music)(samePitch (ly:music-deep-copy music)))

absolute = #(define-music-function (music) (ly:music?)
"Make `music unrelativable. To use inside a \\samePitch function in relative mode."
(make-music 'UnrelativableMusic 'element music))

insert = #(define-music-function (music) (ly:music?)
"Using this function inside the `newnotes parameter of the \\changePitch
function, allow you to insert and remplace by `music, all music between one note
and his following, in the `pattern parameter of \\changePitch, ."
#{ \tag #cPInsertInPattern $music #})

%%%%%%%
#(define (n-copy n music)
(cond
  ((> n 1)(ly:music-deep-copy (make-sequential-music (make-list n music))))
  ((= n 1) music)
  (else (make-music 'Music 'void #t))))

nCopy = #(define-music-function (n music)(integer? ly:music?)
(n-copy n music))

%% same effect as { \repeat unfold n s } but \nSkip works inside the `newnotes
%% parameter of \changePitch.
nSkip = #(define-music-function (n)(integer?)
"Return \\skip \\skip \\skip ... n times."
#{ \nCopy #n s #})
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% shortcuts
% default values for patI and patII, if the user do not define
% them, before using \cPI and \cPII
% patI ={ c8. c16 }      % not \language independant
patI = #(make-music 'SequentialMusic 'elements (list
          (make-music 'NoteEvent 'duration (ly:make-duration 3 1 1)
                                 'pitch (ly:make-pitch -1 0 0))
          (make-music 'NoteEvent 'duration (ly:make-duration 4 0 1)
                                 'pitch (ly:make-pitch -1 0 0))))
% patII = { c4. c8 }
patII = #(make-music 'SequentialMusic 'elements (list
          (make-music 'NoteEvent 'duration (ly:make-duration 2 1 1)
                                 'pitch (ly:make-pitch -1 0 0))
          (make-music 'NoteEvent 'duration (ly:make-duration 3 0 1)
                                 'pitch (ly:make-pitch -1 0 0))))


cPI = #(define-music-function (newnotes) (ly:music?)
#{ \changePitch \patI $newnotes #})

cPII = #(define-music-function (newnotes) (ly:music?)
#{ \changePitch \patII $newnotes #})

#(define cP changePitch)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%